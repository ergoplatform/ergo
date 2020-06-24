package org.ergoplatform.nodeView.wallet.persistence

import java.io.File

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
import Constants.{ScanId, PaymentsScanId}
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
    * Read wallet-related boxes with metadata, see [[getBox()]]
    * @param ids - box identifier
    * @return wallet related boxes (optional result for each box)
    */
  def getBoxes(ids: Seq[BoxId]): Seq[Option[TrackedBox]] = {
    ids.map(id => store.get(key(id)).flatMap(x => TrackedBoxSerializer.parseBytesTry(x).toOption))
  }

  /**
    * Read unspent boxes which belong to given scan
    * @param scanId - scan identifier
    * @return sequences of scan-related unspent boxes found in the database
    */
  def unspentBoxes(scanId: ScanId): Seq[TrackedBox] = {
    store.getRange(firstAppBoxSpaceKey(scanId), lastAppBoxSpaceKey(scanId))
      .flatMap { case (_, boxId) =>
        getBox(ADKey @@ boxId)
      }
  }

  /**
    * Read spent boxes which belong to given scan
    * @param scanId - scan identifier
    * @return sequences of scan-related spent boxes found in the database
    */
  def spentBoxes(scanId: ScanId): Seq[TrackedBox] = {
    store.getRange(firstSpentAppBoxSpaceKey(scanId), lastSpentAppBoxSpaceKey(scanId))
      .flatMap { case (_, boxId) =>
        getBox(ADKey @@ boxId)
      }
  }

  /**
    * Unspent boxes belong to payments scan
    */
  def walletUnspentBoxes(): Seq[TrackedBox] = unspentBoxes(Constants.PaymentsScanId)

  /**
    * Spent boxes belong to payments scan
    */
  def walletSpentBoxes(): Seq[TrackedBox] = spentBoxes(Constants.PaymentsScanId)

  /**
    * Read boxes with certain number of confirmations at most, both spent or not
    * @param scanId scan identifier
    * @param fromHeight min height when box was included into the blockchain
    * @return sequence of scan-related boxes
    */
  def confirmedBoxes(scanId: ScanId, fromHeight: Int): Seq[TrackedBox] = {
    store.getRange(firstIncludedAppBoxSpaceKey(scanId, fromHeight), lastIncludedAppBoxSpaceKey(scanId)).flatMap { case (_, boxId) =>
      getBox(ADKey @@ boxId)
    }
  }

  /**
    * Read boxes belong to the payment scan with certain number of confirmations at most, both spent or not
    * @param fromHeight min height when box was included into the blockchain
    * @return sequence of (P2PK-payment)-related boxes
    */
  def walletConfirmedBoxes(fromHeight: Int): Seq[TrackedBox] = confirmedBoxes(Constants.PaymentsScanId, fromHeight)

  /**
    * Read transaction with wallet-related metadata
    * @param id - transaction identifier
    * @return
    */
  def getTx(id: ModifierId): Option[WalletTransaction] = {
    store.get(txKey(id)).flatMap(r => WalletTransactionSerializer.parseBytesTry(r).toOption)
  }

  //todo: filter by scan
  /**
    * Read all the wallet-related transactions
    * @return all the transactions for all the scans
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

    // first, put newly created outputs and related transactions into key-value bag
    val bag1 = putBoxes(KeyValuePairsBag.empty, newOutputs)
    val bag2 = putTxs(bag1, txs)

    // process spent boxes
    val spentBoxesWithTx = inputs.map(t => t._1 -> t._3)
    val bag3 = processHistoricalBoxes(bag2, spentBoxesWithTx, blockHeight)

    // and update wallet digest
    val bag4 = updateDigest(bag3) { case WalletDigest(height, wBalance, wTokens) =>
      if (height + 1 != blockHeight) {
        log.error(s"Blocks were skipped during wallet scanning, from $height until $blockHeight")
      }
      val spentWalletBoxes = spentBoxesWithTx.map(_._2).filter(_.scans.contains(PaymentsScanId))
      val spentAmt = spentWalletBoxes.map(_.box.value).sum
      val spentTokensAmt = spentWalletBoxes
        .flatMap(_.box.additionalTokens.toArray)
        .foldLeft(Map.empty[EncodedTokenId, Long]) { case (acc, (id, amt)) =>
          acc.updated(encodedTokenId(id), acc.getOrElse(encodedTokenId(id), 0L) + amt)
        }
      val receivedTokensAmt = newOutputs.filter(_.scans.contains(PaymentsScanId))
        .flatMap(_.box.additionalTokens.toArray)
        .foldLeft(Map.empty[EncodedTokenId, Long]) { case (acc, (id, amt)) =>
          acc.updated(encodedTokenId(id), acc.getOrElse(encodedTokenId(id), 0L) + amt)
        }

      val increasedTokenBalances = receivedTokensAmt.foldLeft(wTokens) { case (acc, (encodedId, amt)) =>
        acc += encodedId -> (acc.getOrElse(encodedId, 0L) + amt)
      }

      val newTokensBalance = spentTokensAmt
        .foldLeft(increasedTokenBalances) { case (acc, (encodedId, amt)) =>
          val decreasedAmt = acc.getOrElse(encodedId, 0L) - amt
          if (decreasedAmt > 0) {
            acc += encodedId -> decreasedAmt
          } else {
            acc - encodedId
          }
        }

      val receivedAmt = newOutputs.filter(_.scans.contains(PaymentsScanId)).map(_.box.value).sum
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
    * Remove association between an scan and a box
    * @param scanId scan identifier
    * @param boxId box identifier
    */
  def removeApp(scanId: ScanId, boxId: BoxId): Try[Unit] = {
    getBox(boxId) match {
      case Some(tb) =>
        (if (tb.scans.size == 1) {
          if(tb.scans.head == scanId) {
            val bag = WalletRegistry.removeBox(KeyValuePairsBag.empty, tb)
            Success(bag)
          } else {
            Failure(new Exception(s"Box ${Algos.encode(boxId)} is not associated with app $scanId"))
          }
        } else {
          if(tb.scans.contains(scanId)){
            val updTb = tb.copy(scans = tb.scans - scanId)
            val keyToRemove = Seq(spentIndexKey(scanId, updTb),
              inclusionHeightAppBoxIndexKey(scanId, updTb))
            Success(KeyValuePairsBag(Seq(boxToKvPair(updTb)), keyToRemove))
          } else {
            Failure(new Exception(s"Box ${Algos.encode(boxId)} is not associated with app $scanId"))
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
  import scorex.db.ByteArrayUtils._

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

  /** Performance optimized helper, which avoid unnecessary allocations and creates the resulting
    * key bytes directly from the given parameters.
    * It is allocation and boxing free.
    * @return prefix | scanId | Array.fill(32)(suffix)  bytes packed in an array
    */
  private[persistence] final def composeKey(prefix: Byte, scanId: ScanId, suffix: Byte): Array[Byte] = {
    val res = new Array[Byte](35) // 1 + 2 + 32
    res(0) = prefix
    putShort(res, pos = 1, scanId)
    putReplicated(res, pos = 3, n = 32, suffix)
    res
  }

  /** Same as [[composeKey()]] where suffix is given by id. */
  private[persistence] final def composeKeyWithBoxId(prefix: Byte, scanId: ScanId, suffixId: BoxId): Array[Byte] = {
    val res = new Array[Byte](3 + suffixId.length) // 1 byte for prefix + 2 for scanId
    res(0) = prefix
    putShort(res, pos = 1, scanId)
    putBytes(res, pos = 3, suffixId)
    res
  }

  /** Same as [[composeKey()]] with additional height parameter. */
  private[persistence] final def composeKey(prefix: Byte, scanId: ScanId, height: Int, suffix: Byte): Array[Byte] = {
    val res = new Array[Byte](39) // 1 byte for prefix + 2 for scanId + 4 for height + 32 for suffix
    res(0) = prefix
    putShort(res, pos = 1, scanId)
    putInt(res, pos = 3, height)
    putReplicated(res, 7, 32, suffix)
    res
  }

  /** Same as [[composeKey()]] with additional height parameter and suffix given by id. */
  private[persistence] final def composeKeyWithBoxId(prefix: Byte, scanId: ScanId, height: Int, suffixId: BoxId): Array[Byte] = {
    val res = new Array[Byte](7 + suffixId.length) // 1 byte for prefix + 2 for scanId + 4 for height
    res(0) = prefix
    putShort(res, pos = 1, scanId)
    putInt(res, pos = 3, height)
    putBytes(res, 7, suffixId)
    res
  }

  private def firstAppBoxSpaceKey(scanId: ScanId): Array[Byte] =
    composeKey(UnspentIndexPrefix, scanId, 0)

  private def lastAppBoxSpaceKey(scanId: ScanId): Array[Byte] =
    composeKey(UnspentIndexPrefix, scanId, -1)

  private def firstSpentAppBoxSpaceKey(scanId: ScanId): Array[Byte] =
    composeKey(SpentIndexPrefix, scanId, 0)

  private def lastSpentAppBoxSpaceKey(scanId: ScanId): Array[Byte] =
    composeKey(SpentIndexPrefix, scanId, -1)

  private def firstIncludedAppBoxSpaceKey(scanId: ScanId, height: Int): Array[Byte] =
    composeKey(UnspentIndexPrefix, scanId, height, 0)

  private def lastIncludedAppBoxSpaceKey(scanId: ScanId): Array[Byte] =
    composeKey(UnspentIndexPrefix, scanId, Int.MaxValue, -1)

  private val RegistrySummaryKey: Array[Byte] = Array(0x02: Byte)

  private def key(trackedBox: TrackedBox): Array[Byte] = BoxKeyPrefix +: trackedBox.box.id

  private def key(id: BoxId): Array[Byte] = BoxKeyPrefix +: id

  private def txKey(id: ModifierId): Array[Byte] = TxKeyPrefix +: idToBytes(id)

  private def boxToKvPair(box: TrackedBox) = key(box) -> TrackedBoxSerializer.toBytes(box)

  private def txToKvPair(tx: WalletTransaction) = txKey(tx.id) -> WalletTransactionSerializer.toBytes(tx)

  private def spentIndexKey(scanId: ScanId, trackedBox: TrackedBox): Array[Byte] = {
    val prefix = if (trackedBox.isSpent) SpentIndexPrefix else UnspentIndexPrefix
    composeKeyWithBoxId(prefix, scanId, trackedBox.box.id)
  }

  private def inclusionHeightAppBoxIndexKey(scanId: ScanId, trackedBox: TrackedBox): Array[Byte] = {
    val inclusionHeight= trackedBox.inclusionHeightOpt.getOrElse(0)
    composeKeyWithBoxId(InclusionHeightAppBoxPrefix, scanId, inclusionHeight, trackedBox.box.id)
  }

  def boxIndexKeys(box: TrackedBox): Seq[Array[Byte]] = {
    box.scans.toSeq.flatMap { scanId =>
      Seq(
        spentIndexKey(scanId, box),
        inclusionHeightAppBoxIndexKey(scanId, box)
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
      case Some((_, _)) =>
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
