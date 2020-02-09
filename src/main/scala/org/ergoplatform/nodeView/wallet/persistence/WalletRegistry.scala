package org.ergoplatform.nodeView.wallet.persistence

import java.io.File
import com.google.common.primitives.{Ints, Shorts}
import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.db.HybridLDBKVStore
import org.ergoplatform.modifiers.history.PreGenesisHeader
import org.ergoplatform.nodeView.wallet.IdUtils.{EncodedBoxId, EncodedTokenId}
import org.ergoplatform.nodeView.wallet.{WalletTransaction, WalletTransactionSerializer}
import org.ergoplatform.nodeView.wallet.scanning.ExternalApplication.AppId
import org.ergoplatform.settings.{Algos, ErgoSettings, WalletSettings}
import org.ergoplatform.wallet.Constants
import org.ergoplatform.wallet.boxes.{BoxCertainty, TrackedBox, TrackedBoxSerializer}
import scorex.core.VersionTag
import scorex.crypto.authds.ADKey
import scorex.util.{ModifierId, ScorexLogging, idToBytes}
import Constants.PaymentsAppId
import scorex.db.LDBVersionedStore

import scala.util.{Failure, Success, Try}

/**
  * Provides an access to version-sensitive wallet-specific indexes:
  *
  * * current wallet status (height, balances, uncertain boxes)
  * * wallet-related transactions
  * * certain boxes, spent or not
  *
  */
class WalletRegistry(store: HybridLDBKVStore)(ws: WalletSettings) extends ScorexLogging {

  import WalletRegistry._
  import org.ergoplatform.nodeView.wallet.IdUtils.{encodedBoxId, encodedTokenId}

  private val keepHistory = ws.keepSpentBoxes

  def updateBoxes(bag: KeyValuePairsBag,
                  ids: Seq[BoxId])(updateF: TrackedBox => TrackedBox): KeyValuePairsBag = {
    putBoxes(bag, getBoxes(ids).flatten.map(updateF))
  }

  def getBox(id: BoxId): Option[TrackedBox] = {
    store.get(key(id)).flatMap(r => TrackedBoxSerializer.parseBytesTry(r).toOption)
  }

  def getBoxes(ids: Seq[BoxId]): Seq[Option[TrackedBox]] = {
    ids.map(id => store.get(key(id)).flatMap(x => TrackedBoxSerializer.parseBytesTry(x).toOption))
  }

  def unspentBoxes(appId: AppId): Seq[TrackedBox] = {
    store.getRange(firstAppBoxSpaceKey(appId), lastAppBoxSpaceKey(appId))
      .flatMap { case (_, boxId) =>
        getBox(ADKey @@ boxId).flatMap{b =>
          if(b.applicationStatuses(appId).certain) Some(b) else None
        }
      }
  }

  def spentBoxes(appId: AppId): Seq[TrackedBox] = {
    store.getRange(firstSpentAppBoxSpaceKey(appId), lastSpentAppBoxSpaceKey(appId))
      .flatMap { case (_, boxId) =>
        getBox(ADKey @@ boxId)
      }
  }

  def uncertainBoxes(appId: AppId): Seq[TrackedBox] = {
    store.getRange(firstUncertainAppBoxSpaceKey(appId), lastUncertainAppBoxSpaceKey(appId))
      .flatMap { case (_, boxId) =>
        getBox(ADKey @@ boxId)
      }
  }

  def certainBoxes(appId: AppId): Seq[TrackedBox] = {
    store.getRange(firstCertainAppBoxSpaceKey(appId), lastCertainAppBoxSpaceKey(appId))
      .flatMap { case (_, boxId) =>
        getBox(ADKey @@ boxId)
      }
  }

  def walletUnspentBoxes(): Seq[TrackedBox] = unspentBoxes(Constants.PaymentsAppId)

  def walletSpentBoxes(): Seq[TrackedBox] = spentBoxes(Constants.PaymentsAppId)

  def confirmedBoxes(appId: AppId, fromHeight: Int): Seq[TrackedBox] = {
    store.getRange(firstIncludedAppBoxSpaceKey(appId, fromHeight), lastIncludedAppBoxSpaceKey(appId)).flatMap { case (_, boxId) =>
      getBox(ADKey @@ boxId)
    }
  }

  def walletConfirmedBoxes(fromHeight: Int): Seq[TrackedBox] = confirmedBoxes(Constants.PaymentsAppId, fromHeight)

  def getTx(id: ModifierId): Option[WalletTransaction] = {
    store.get(txKey(id)).flatMap(r => WalletTransactionSerializer.parseBytesTry(r).toOption)
  }

  //todo: filter by application
  def allWalletTxs(): Seq[WalletTransaction] = {
    store.getRange(FirstTxSpaceKey, LastTxSpaceKey)
      .flatMap { case (_, txBytes) =>
        WalletTransactionSerializer.parseBytesTry(txBytes).toOption
      }
  }

  def fetchDigest(): RegistryDigest = {
    store.get(RegistrySummaryKey)
      .flatMap(r => RegistryDigestSerializer.parseBytesTry(r).toOption)
      .getOrElse(RegistryDigest.empty)
  }

  def updateDigest(bag: KeyValuePairsBag)(updateF: RegistryDigest => RegistryDigest): KeyValuePairsBag = {
    val digest = fetchDigest()
    putDigest(bag, updateF(digest))
  }

  /**
    * Updates indexes according to a data extracted from a block and performs versioned update.
    */
  def updateOnBlock(outputs: Seq[TrackedBox],
                    inputs: Seq[(ModifierId, EncodedBoxId, TrackedBox)],
                    txs: Seq[WalletTransaction])
                   (blockId: ModifierId, blockHeight: Int): Unit = {

    val spentBoxesWithTx = inputs.map(t => t._1 -> t._3)

    val bag0 = KeyValuePairsBag.empty
    val bag1 = putBoxes(bag0, outputs)
    val bag2 = putTxs(bag1, txs)

    val bag3 = processHistoricalBoxes(bag2, spentBoxesWithTx, blockHeight)

    val bag4 = updateDigest(bag3) { case RegistryDigest(height, wBalance, wTokens) =>
      val spentWalletBoxes = spentBoxesWithTx.map(_._2).filter(_.applicationStatuses.contains(PaymentsAppId))
      val spentAmt = spentWalletBoxes.map(_.box.value).sum
      val spentTokensAmt = spentWalletBoxes
        .flatMap(_.box.additionalTokens.toArray)
        .foldLeft(Map.empty[EncodedTokenId, Long]) { case (acc, (id, amt)) =>
          acc.updated(encodedTokenId(id), acc.getOrElse(encodedTokenId(id), 0L) + amt)
        }
      val receivedTokensAmt = outputs.filter(_.applicationStatuses.contains(PaymentsAppId))
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

      val receivedAmt = outputs.filter(_.applicationStatuses.contains(PaymentsAppId)).map(_.box.value).sum
      val newBalance = wBalance + receivedAmt - spentAmt
      require(
        (newBalance >= 0 && newTokensBalance.forall(_._2 >= 0)) || ws.testMnemonic.isDefined,
        "Balance could not be negative")
      RegistryDigest(blockHeight, newBalance, newTokensBalance)
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
      updateBoxes(bag, spentBoxes.map(_._2.box.id)) { tb =>
        val spendingTxIdOpt = spentBoxes
          .find { case (_, x) => encodedBoxId(x.box.id) == encodedBoxId(tb.box.id) }
          .map(_._1)
        tb.copy(spendingHeightOpt = Some(spendingHeight), spendingTxIdOpt = spendingTxIdOpt)
      }
    } else {
      removeBoxes(bag, spentBoxes.map(_._2))
    }
  }

  def makeCertain(appId: AppId, boxId: BoxId): Try[Unit] = {
    getBox(boxId) match {
      case Some(tb) => tb.applicationStatuses.get(appId).map { _ =>
        val updTb = tb.copy(applicationStatuses = tb.applicationStatuses.updated(appId, BoxCertainty.Certain))

        store.cachePut(Seq(boxToKvPair(updTb), certaintyKey(appId, updTb) -> boxId))
        Success((): Unit)
      }.getOrElse(Failure(new Exception(s"Box ${Algos.encode(boxId)} is not associated with app $appId")))

      case None => Failure(new Exception(s"No box with id ${Algos.encode(boxId)} found in the wallet database"))
    }
  }

  def removeApp(appId: AppId, boxId: BoxId): Try[Unit] = {
    getBox(boxId) match {
      case Some(tb) =>
        (if (tb.applicationStatuses.size == 1) {
          tb.applicationStatuses.get(appId).map { _ =>
            val bag = WalletRegistry.removeBox(KeyValuePairsBag.empty, tb)
            Success(bag)
          }.getOrElse(Failure(new Exception(s"Box ${Algos.encode(boxId)} is not associated with app $appId")))
        } else {
          tb.applicationStatuses.get(appId).map { _ =>
            val updTb = tb.copy(applicationStatuses = tb.applicationStatuses - appId)
            val keyToRemove = Seq(spentIndexKey(appId, updTb),
              certaintyKey(appId, updTb),
              inclusionHeightAppBoxIndexKey(appId, updTb))
            Success(KeyValuePairsBag(Seq(boxToKvPair(updTb)), keyToRemove))
          }.getOrElse(Failure(new Exception(s"Box ${Algos.encode(boxId)} is not associated with app $appId")))
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

  private val UncertainAppBoxIndexPrefix: Byte = 0x05
  private val CertainAppBoxIndexPrefix: Byte = 0x06

  private val InclusionHeightAppBoxPrefix: Byte = 0x07

  private val FirstTxSpaceKey: Array[Byte] = TxKeyPrefix +: Array.fill(32)(0: Byte)
  private val LastTxSpaceKey: Array[Byte] = TxKeyPrefix +: Array.fill(32)(-1: Byte)

  private def firstAppBoxSpaceKey(appId: AppId): Array[Byte] = UnspentIndexPrefix +: (Shorts.toByteArray(appId) ++ Array.fill(32)(0: Byte))

  private def lastAppBoxSpaceKey(appId: AppId): Array[Byte] = UnspentIndexPrefix +: (Shorts.toByteArray(appId) ++ Array.fill(32)(-1: Byte))

  private def firstSpentAppBoxSpaceKey(appId: AppId): Array[Byte] =
    SpentIndexPrefix +: (Shorts.toByteArray(appId) ++ Array.fill(32)(0: Byte))

  private def lastSpentAppBoxSpaceKey(appId: AppId): Array[Byte] =
    SpentIndexPrefix +: (Shorts.toByteArray(appId) ++ Array.fill(32)(-1: Byte))

  private def firstUncertainAppBoxSpaceKey(appId: AppId): Array[Byte] =
    UncertainAppBoxIndexPrefix +: (Shorts.toByteArray(appId) ++ Array.fill(32)(0: Byte))

  private def lastUncertainAppBoxSpaceKey(appId: AppId): Array[Byte] =
    UncertainAppBoxIndexPrefix +: (Shorts.toByteArray(appId) ++ Array.fill(32)(-1: Byte))

  private def firstCertainAppBoxSpaceKey(appId: AppId): Array[Byte] =
    CertainAppBoxIndexPrefix +: (Shorts.toByteArray(appId) ++ Array.fill(32)(0: Byte))

  private def lastCertainAppBoxSpaceKey(appId: AppId): Array[Byte] =
    CertainAppBoxIndexPrefix +: (Shorts.toByteArray(appId) ++ Array.fill(32)(-1: Byte))

  private def firstIncludedAppBoxSpaceKey(appId: AppId, height: Int): Array[Byte] =
    UnspentIndexPrefix +: (Shorts.toByteArray(appId) ++ Ints.toByteArray(height) ++ Array.fill(32)(0: Byte))

  private def lastIncludedAppBoxSpaceKey(appId: AppId): Array[Byte] =
    UnspentIndexPrefix +: (Shorts.toByteArray(appId) ++ Ints.toByteArray(Int.MaxValue) ++ Array.fill(32)(-1: Byte))

  private val RegistrySummaryKey: Array[Byte] = Array(0x02: Byte)

  private def key(trackedBox: TrackedBox): Array[Byte] = BoxKeyPrefix +: trackedBox.box.id

  private def key(id: BoxId): Array[Byte] = BoxKeyPrefix +: id

  private def txKey(id: ModifierId): Array[Byte] = TxKeyPrefix +: idToBytes(id)

  private def boxToKvPair(box: TrackedBox) = key(box) -> TrackedBoxSerializer.toBytes(box)

  private def txToKvPair(tx: WalletTransaction) = txKey(tx.id) -> WalletTransactionSerializer.toBytes(tx)

  private def spentIndexKey(appId: AppId, trackedBox: TrackedBox): Array[Byte] = {
    val prefix = if (trackedBox.spent) SpentIndexPrefix else UnspentIndexPrefix
    prefix +: (Shorts.toByteArray(appId) ++ trackedBox.box.id)
  }

  private def certaintyKey(appId: AppId, trackedBox: TrackedBox): Array[Byte] = {
    val prefix = if (trackedBox.certain(appId).get.certain) CertainAppBoxIndexPrefix else UncertainAppBoxIndexPrefix //todo: .get
    prefix +: (Shorts.toByteArray(appId) ++ trackedBox.box.id)
  }

  private def inclusionHeightAppBoxIndexKey(appId: AppId, trackedBox: TrackedBox): Array[Byte] = {
    val inclusionHeightBytes = Ints.toByteArray(trackedBox.inclusionHeightOpt.getOrElse(0))
    InclusionHeightAppBoxPrefix +: (Shorts.toByteArray(appId) ++ inclusionHeightBytes ++ trackedBox.box.id)
  }


  def putBox(bag: KeyValuePairsBag, box: TrackedBox): KeyValuePairsBag = {
    val appIndexUpdates = box.applicationStatuses.toSeq.flatMap { case (appId, _) =>
      Seq(
        spentIndexKey(appId, box) -> box.box.id,
        certaintyKey(appId, box) -> box.box.id, //todo: avoid for simple payments app
        inclusionHeightAppBoxIndexKey(appId, box) -> box.box.id
      )
    }
    val newKvPairs = appIndexUpdates :+ boxToKvPair(box)
    bag.copy(toInsert = bag.toInsert ++ newKvPairs)
  }

  def putBoxes(bag: KeyValuePairsBag, boxes: Seq[TrackedBox]): KeyValuePairsBag = {
    boxes.foldLeft(bag) { case (b, box) => putBox(b, box) }
  }

  def removeBox(bag: KeyValuePairsBag, box: TrackedBox): KeyValuePairsBag = {
    val appIndexUpdates = box.applicationStatuses.toSeq.flatMap { case (appId, _) =>
      Seq(spentIndexKey(appId, box), certaintyKey(appId, box), inclusionHeightAppBoxIndexKey(appId, box))
    }
    val ids = appIndexUpdates :+ key(box)

    bag.copy(toRemove = bag.toRemove ++ ids)
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

  def putDigest(bag: KeyValuePairsBag, index: RegistryDigest): KeyValuePairsBag = {
    val registryBytes = RegistryDigestSerializer.toBytes(index)
    bag.copy(toInsert = bag.toInsert :+ (RegistrySummaryKey, registryBytes))
  }
}

case class KeyValuePairsBag(toInsert: Seq[(Array[Byte], Array[Byte])], toRemove: Seq[Array[Byte]]) {

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