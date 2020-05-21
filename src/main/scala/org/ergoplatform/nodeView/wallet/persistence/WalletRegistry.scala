package org.ergoplatform.nodeView.wallet.persistence

import java.io.File

import com.google.common.primitives.{Ints, Shorts}
import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.db.HybridLDBKVStore
import org.ergoplatform.modifiers.history.PreGenesisHeader
import org.ergoplatform.nodeView.wallet.IdUtils.{EncodedBoxId, EncodedTokenId}
import org.ergoplatform.nodeView.wallet.{WalletTransaction, WalletTransactionSerializer}
import org.ergoplatform.settings.{Algos, ErgoSettings, WalletSettings}
import org.ergoplatform.wallet.Constants
import org.ergoplatform.wallet.boxes.{TrackedBox, TrackedBoxSerializer}
import scorex.core.VersionTag
import scorex.crypto.authds.ADKey
import scorex.util.{ModifierId, ScorexLogging, idToBytes}
import Constants.{ApplicationId, PaymentsAppId}
import scorex.db.LDBVersionedStore

import scala.util.{Failure, Success, Try}

/**
  * Provides an access to version-sensitive wallet-specific indexes:
  *
  * * current wallet status (height, balances)
  * * wallet-related transactions
  * * boxes, spent or not
  *
  */
class WalletRegistry(store: HybridLDBKVStore)(ws: WalletSettings) extends ScorexLogging {

  import WalletRegistry._
  import org.ergoplatform.nodeView.wallet.IdUtils.encodedTokenId

  private val keepHistory = ws.keepSpentBoxes

  /**
    * Read wallet-related box with metadata
    * @param id - box identifier (the same as Ergobox identifier)
    * @return wallet related box if it is stored in the database, None otherwise
    */
  def getBox(id: BoxId): Option[TrackedBox] = {
    store.get(key(id)).flatMap(r => TrackedBoxSerializer.parseBytesTry(r).toOption)
  }


  /**
    * Read wallet-related boxes with metadata, see getBox
    * @param ids - box identifier
    * @return wallet related boxes (optional result for each box)
    */
  def getBoxes(ids: Seq[BoxId]): Seq[Option[TrackedBox]] = {
    ids.map(id => store.get(key(id)).flatMap(x => TrackedBoxSerializer.parseBytesTry(x).toOption))
  }

  /**
    * Read unspent boxes which belong to given application
    * @param appId - application identifier
    * @return sequences of application-related unspent boxes found in the database
    */
  def unspentBoxes(appId: ApplicationId): Seq[TrackedBox] = {
    store.getRange(firstAppBoxSpaceKey(appId), lastAppBoxSpaceKey(appId))
      .flatMap { case (_, boxId) =>
        getBox(ADKey @@ boxId)
      }
  }

  /**
    * Read spent boxes which belong to given application
    * @param appId - application identifier
    * @return sequences of application-related spent boxes found in the database
    */
  def spentBoxes(appId: ApplicationId): Seq[TrackedBox] = {
    store.getRange(firstSpentAppBoxSpaceKey(appId), lastSpentAppBoxSpaceKey(appId))
      .flatMap { case (_, boxId) =>
        getBox(ADKey @@ boxId)
      }
  }

  /**
    * Unspent boxes belong to payments application
    */
  def walletUnspentBoxes(): Seq[TrackedBox] = unspentBoxes(Constants.PaymentsAppId)

  /**
    * Spent boxes belong to payments application
    */
  def walletSpentBoxes(): Seq[TrackedBox] = spentBoxes(Constants.PaymentsAppId)

  /**
    * Read boxes with certain number of confirmations at most, both spent or not
    * @param appId application identifier
    * @param fromHeight min height when box was included into the blockchain
    * @return sequence of application-related boxes
    */
  def confirmedBoxes(appId: ApplicationId, fromHeight: Int): Seq[TrackedBox] = {
    store.getRange(firstIncludedAppBoxSpaceKey(appId, fromHeight), lastIncludedAppBoxSpaceKey(appId)).flatMap { case (_, boxId) =>
      getBox(ADKey @@ boxId)
    }
  }

  /**
    * Read boxes belong to the payment application with certain number of confirmations at most, both spent or not
    * @param fromHeight min height when box was included into the blockchain
    * @return sequence of (P2PK-payment)-related boxes
    */
  def walletConfirmedBoxes(fromHeight: Int): Seq[TrackedBox] = confirmedBoxes(Constants.PaymentsAppId, fromHeight)

  /**
    *
    * @param id
    * @return
    */
  def getTx(id: ModifierId): Option[WalletTransaction] = {
    store.get(txKey(id)).flatMap(r => WalletTransactionSerializer.parseBytesTry(r).toOption)
  }

  //todo: filter by application
  /**
    * Read all the wallet-related transactions
    * @return all the transactions for all the applications
    */
  def allWalletTxs(): Seq[WalletTransaction] = {
    store.getRange(FirstTxSpaceKey, LastTxSpaceKey)
      .flatMap { case (_, txBytes) =>
        WalletTransactionSerializer.parseBytesTry(txBytes).toOption
      }
  }

  /**
    * Read aggregate wallet information
    * @return wallet digest
    */
  def fetchDigest(): WalletDigest = {
    store.get(RegistrySummaryKey)
      .flatMap(r => WalletDigestSerializer.parseBytesTry(r).toOption)
      .getOrElse(WalletDigest.empty)
  }


  /**
    * Update aggregate wallet information
    */
  def updateDigest(bag: KeyValuePairsBag)(updateF: WalletDigest => WalletDigest): KeyValuePairsBag = {
    val digest = fetchDigest()
    putDigest(bag, updateF(digest))
  }

  /**
    *
    * Updates indexes according to data extracted from a block and performs versioned update.
    *
    * @param newOutputs  - newly created outputs (but could be spent by inputs)
    * @param inputs      - spent inputs as a sequence of (input tx id, input box id, tracked box)
    * @param txs         - transactions affected
    * @param blockId     - block identifier
    * @param blockHeight - block height
    */
  def updateOnBlock(newOutputs: Seq[TrackedBox],
                    inputs: Seq[(ModifierId, EncodedBoxId, TrackedBox)],
                    txs: Seq[WalletTransaction])
                   (blockId: ModifierId, blockHeight: Int): Unit = {

    val bag0 = KeyValuePairsBag.empty
    val bag1 = putBoxes(bag0, newOutputs)
    val bag2 = putTxs(bag1, txs)

    val spentBoxesWithTx = inputs.map(t => t._1 -> t._3)
    val bag3 = processHistoricalBoxes(bag2, spentBoxesWithTx, blockHeight)

    val bag4 = updateDigest(bag3) { case WalletDigest(height, wBalance, wTokens) =>
      if (height + 1 != blockHeight) {
        log.error(s"Blocks were skipped during wallet scanning, from ${height + 1} until $blockHeight")
      }
      val spentWalletBoxes = spentBoxesWithTx.map(_._2).filter(_.applicationStatuses.contains(PaymentsAppId))
      val spentAmt = spentWalletBoxes.map(_.box.value).sum
      val spentTokensAmt = spentWalletBoxes
        .flatMap(_.box.additionalTokens.toArray)
        .foldLeft(Map.empty[EncodedTokenId, Long]) { case (acc, (id, amt)) =>
          acc.updated(encodedTokenId(id), acc.getOrElse(encodedTokenId(id), 0L) + amt)
        }
      val receivedTokensAmt = newOutputs.filter(_.applicationStatuses.contains(PaymentsAppId))
        .flatMap(_.box.additionalTokens.toArray)
        .foldLeft(Map.empty[EncodedTokenId, Long]) { case (acc, (id, amt)) =>
          acc.updated(encodedTokenId(id), acc.getOrElse(encodedTokenId(id), 0L) + amt)
        }

      val increasedTokenBalances = receivedTokensAmt.foldLeft(wTokens) { case (acc, (encodedId, amt)) =>
        acc.updated(encodedId, acc.getOrElse(encodedId, 0L) + amt)
      }

      val newTokensBalance = spentTokensAmt
        .foldLeft(increasedTokenBalances) { case (acc, (encodedId, amt)) =>
          val decreasedAmt = acc.getOrElse(encodedId, 0L) - amt
          if (decreasedAmt > 0) acc.updated(encodedId, decreasedAmt) else acc - encodedId
        }

      val receivedAmt = newOutputs.filter(_.applicationStatuses.contains(PaymentsAppId)).map(_.box.value).sum
      val newBalance = wBalance + receivedAmt - spentAmt
      require(
        (newBalance >= 0 && newTokensBalance.forall(_._2 >= 0)) || ws.testMnemonic.isDefined,
        "Balance could not be negative")
      WalletDigest(blockHeight, newBalance, newTokensBalance)
    }

    bag4.transact(store, idToBytes(blockId))
  }

  def rollback(version: VersionTag): Try[Unit] =
    store.rollbackTo(scorex.core.versionToBytes(version))

  /**
    * Transits used boxes to a spent state or simply deletes them depending on a settings.
    */
  private[persistence] def processHistoricalBoxes(bag: KeyValuePairsBag,
                                                  spentBoxes: Seq[(ModifierId, TrackedBox)],
                                                  spendingHeight: Int): KeyValuePairsBag = {
    if (keepHistory) {
      val outSpent: Seq[TrackedBox] = spentBoxes.flatMap { case (_, tb) =>
        getBox(tb.box.id).orElse {
          bag.toInsert.find(_._1.sameElements(key(tb))).flatMap { case (_, tbBytes) =>
            TrackedBoxSerializer.parseBytesTry(tbBytes).toOption
          } match {
            case s@Some(_) => s
            case None =>
              log.warn(s"Output spent hasn't found in the wallet: ${Algos.encode(tb.box.id)}, " +
                s"could be okay if it was created before wallet init")
              None
          }
        }: Option[TrackedBox]
      }

      val updatedBoxes = outSpent.map { tb =>
        val spendingTxIdOpt = spentBoxes
          .find { case (_, x) => x.box.id.sameElements(tb.box.id) }
          .map(_._1)
        tb.copy(spendingHeightOpt = Some(spendingHeight), spendingTxIdOpt = spendingTxIdOpt)
      }

      val bagBeforePut = removeBoxes(bag, spentBoxes.map(_._2))
      putBoxes(bagBeforePut, updatedBoxes)
    } else {
      removeBoxes(bag, spentBoxes.map(_._2))
    }
  }

  /**
    * Remove association between an application and a box
    * @param appId application identifier
    * @param boxId box identifier
    */
  def removeApp(appId: ApplicationId, boxId: BoxId): Try[Unit] = {
    getBox(boxId) match {
      case Some(tb) =>
        (if (tb.applicationStatuses.size == 1) {
          if(tb.applicationStatuses.head == appId) {
            val bag = WalletRegistry.removeBox(KeyValuePairsBag.empty, tb)
            Success(bag)
          } else {
            Failure(new Exception(s"Box ${Algos.encode(boxId)} is not associated with app $appId"))
          }
        } else {
          if(tb.applicationStatuses.contains(appId)){
            val updTb = tb.copy(applicationStatuses = tb.applicationStatuses - appId)
            val keyToRemove = Seq(spentIndexKey(appId, updTb),
              inclusionHeightAppBoxIndexKey(appId, updTb))
            Success(KeyValuePairsBag(Seq(boxToKvPair(updTb)), keyToRemove))
          } else {
            Failure(new Exception(s"Box ${Algos.encode(boxId)} is not associated with app $appId"))
          }
        }).map { bag =>
          store.cachePut(bag.toInsert)
          store.cacheRemove(bag.toRemove)
        }

      case None => Failure(new Exception(s"No box with id ${Algos.encode(boxId)} found in the wallet database"))
    }
  }
}

object WalletRegistry {

  val PreGenesisStateVersion: Array[Byte] = idToBytes(PreGenesisHeader.id)

  def readOrCreate(settings: ErgoSettings): WalletRegistry = {
    val dir = new File(s"${settings.directory}/wallet/registry")
    dir.mkdirs()

    val store = new HybridLDBKVStore(dir, settings.nodeSettings.keepVersions)

    // Create pre-genesis state checkpoint
    if (!store.versionIdExists(PreGenesisStateVersion)) store.update(PreGenesisStateVersion, Seq.empty, Seq.empty)

    new WalletRegistry(store)(settings.walletSettings)
  }

  private val BoxKeyPrefix: Byte = 0x01
  private val TxKeyPrefix: Byte = 0x02
  private val UnspentIndexPrefix: Byte = 0x03
  private val SpentIndexPrefix: Byte = 0x04

  private val InclusionHeightAppBoxPrefix: Byte = 0x07

  private val FirstTxSpaceKey: Array[Byte] = TxKeyPrefix +: Array.fill(32)(0: Byte)
  private val LastTxSpaceKey: Array[Byte] = TxKeyPrefix +: Array.fill(32)(-1: Byte)

  private def firstAppBoxSpaceKey(appId: ApplicationId): Array[Byte] =
    UnspentIndexPrefix +: (Shorts.toByteArray(appId) ++ Array.fill(32)(0: Byte))

  private def lastAppBoxSpaceKey(appId: ApplicationId): Array[Byte] =
    UnspentIndexPrefix +: (Shorts.toByteArray(appId) ++ Array.fill(32)(-1: Byte))

  private def firstSpentAppBoxSpaceKey(appId: ApplicationId): Array[Byte] =
    SpentIndexPrefix +: (Shorts.toByteArray(appId) ++ Array.fill(32)(0: Byte))

  private def lastSpentAppBoxSpaceKey(appId: ApplicationId): Array[Byte] =
    SpentIndexPrefix +: (Shorts.toByteArray(appId) ++ Array.fill(32)(-1: Byte))

  private def firstIncludedAppBoxSpaceKey(appId: ApplicationId, height: Int): Array[Byte] =
    UnspentIndexPrefix +: (Shorts.toByteArray(appId) ++ Ints.toByteArray(height) ++ Array.fill(32)(0: Byte))

  private def lastIncludedAppBoxSpaceKey(appId: ApplicationId): Array[Byte] =
    UnspentIndexPrefix +: (Shorts.toByteArray(appId) ++ Ints.toByteArray(Int.MaxValue) ++ Array.fill(32)(-1: Byte))

  private val RegistrySummaryKey: Array[Byte] = Array(0x02: Byte)

  private def key(trackedBox: TrackedBox): Array[Byte] = BoxKeyPrefix +: trackedBox.box.id

  private def key(id: BoxId): Array[Byte] = BoxKeyPrefix +: id

  private def txKey(id: ModifierId): Array[Byte] = TxKeyPrefix +: idToBytes(id)

  private def boxToKvPair(box: TrackedBox) = key(box) -> TrackedBoxSerializer.toBytes(box)

  private def txToKvPair(tx: WalletTransaction) = txKey(tx.id) -> WalletTransactionSerializer.toBytes(tx)

  private def spentIndexKey(appId: ApplicationId, trackedBox: TrackedBox): Array[Byte] = {
    val prefix = if (trackedBox.spent) SpentIndexPrefix else UnspentIndexPrefix
    prefix +: (Shorts.toByteArray(appId) ++ trackedBox.box.id)
  }

  private def inclusionHeightAppBoxIndexKey(appId: ApplicationId, trackedBox: TrackedBox): Array[Byte] = {
    val inclusionHeightBytes = Ints.toByteArray(trackedBox.inclusionHeightOpt.getOrElse(0))
    InclusionHeightAppBoxPrefix +: (Shorts.toByteArray(appId) ++ inclusionHeightBytes ++ trackedBox.box.id)
  }

  def boxIndexKeys(box: TrackedBox): Seq[Array[Byte]] = {
    box.applicationStatuses.toSeq.flatMap { appId =>
      Seq(
        spentIndexKey(appId, box),
        inclusionHeightAppBoxIndexKey(appId, box)
      )
    }
  }

  def boxIndexes(box: TrackedBox): Seq[(Array[Byte], Array[Byte])] = {
    boxIndexKeys(box).map(k => k -> box.box.id)
  }

  def putBox(bag: KeyValuePairsBag, box: TrackedBox): KeyValuePairsBag = {
    val appIndexUpdates = boxIndexes(box)
    val newKvPairs = appIndexUpdates :+ boxToKvPair(box)
    bag.copy(toInsert = bag.toInsert ++ newKvPairs)
  }

  def putBoxes(bag: KeyValuePairsBag, boxes: Seq[TrackedBox]): KeyValuePairsBag = {
    boxes.foldLeft(bag) { case (b, box) => putBox(b, box) }
  }

  def removeBox(bag: KeyValuePairsBag, box: TrackedBox): KeyValuePairsBag = {
    val appIndexKeys = boxIndexKeys(box)
    val boxKeys = appIndexKeys :+ key(box)

    bag.toInsert.find(_._1.sameElements(key(box))) match {
      case Some((id, _)) =>
        bag.copy(toInsert = bag.toInsert.filterNot { case (k, _) =>
          boxKeys.exists(_.sameElements(k))
        })
      case None =>
        bag.copy(toRemove = bag.toRemove ++ boxKeys)
    }
  }

  def removeBoxes(bag: KeyValuePairsBag, boxes: Seq[TrackedBox]): KeyValuePairsBag = {
    boxes.foldLeft(bag) { case (b, box) => removeBox(b, box) }
  }

  def putTx(bag: KeyValuePairsBag, wtx: WalletTransaction): KeyValuePairsBag = {
    bag.copy(toInsert = bag.toInsert :+ txToKvPair(wtx))
  }

  def putTxs(bag: KeyValuePairsBag, txs: Seq[WalletTransaction]): KeyValuePairsBag = {
    bag.copy(toInsert = bag.toInsert ++ txs.map(txToKvPair))
  }

  def removeTxs(bag: KeyValuePairsBag, ids: Seq[ModifierId]): KeyValuePairsBag = {
    bag.copy(toRemove = bag.toRemove ++ ids.map(txKey))
  }

  def putDigest(bag: KeyValuePairsBag, index: WalletDigest): KeyValuePairsBag = {
    val registryBytes = WalletDigestSerializer.toBytes(index)
    bag.copy(toInsert = bag.toInsert :+ (RegistrySummaryKey, registryBytes))
  }
}

/**
  * This class collects data for versioned database update
  * @param toInsert - key-value pairs to write to the database
  * @param toRemove - keys to remove from the database
  */
case class KeyValuePairsBag(toInsert: Seq[(Array[Byte], Array[Byte])],
                            toRemove: Seq[Array[Byte]]) {

  /**
    * Applies non-versioned transaction to a given `store`.
    *
    */
  def transact(store: LDBVersionedStore): Unit = transact(store, None)

  /**
    * Applies versioned transaction to a given `store`.
    */
  def transact(store: LDBVersionedStore, version: Array[Byte]): Unit = transact(store, Some(version))

  private def transact(store: LDBVersionedStore, versionOpt: Option[Array[Byte]]): Unit =
    if (toInsert.nonEmpty || toRemove.nonEmpty) {
      store.update(versionOpt.getOrElse(scorex.utils.Random.randomBytes()), toRemove, toInsert)
    }

}

object KeyValuePairsBag {
  def empty: KeyValuePairsBag = KeyValuePairsBag(Seq.empty, Seq.empty)
}
