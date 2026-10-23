package org.ergoplatform.nodeView.wallet.persistence

import com.google.common.primitives.Ints
import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.modifiers.ErgoNodeViewModifier.ModifierIdSize
import org.ergoplatform.modifiers.history.header.PreGenesisHeader
import org.ergoplatform.nodeView.wallet.IdUtils.{EncodedTokenId, encodedTokenId}
import org.ergoplatform.nodeView.wallet.WalletScanLogic.ScanResults
import org.ergoplatform.nodeView.wallet.{WalletTransaction, WalletTransactionSerializer}
import org.ergoplatform.sdk.wallet.AssetUtils
import org.ergoplatform.settings.{Algos, ErgoSettings, WalletSettings}
import org.ergoplatform.wallet.Constants
import org.ergoplatform.wallet.Constants.{PaymentsScanId, ScanId}
import org.ergoplatform.wallet.boxes.{TrackedBox, TrackedBoxSerializer}
import org.ergoplatform.wallet.transactions.TransactionBuilder
import org.ergoplatform.core.VersionTag
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Blake2b256
import scorex.db.LDBVersionedStore
import scorex.util.encode.Base16
import scorex.util.{ModifierId, ScorexLogging, bytesToId, idToBytes}

import java.io.File
import scala.collection.immutable.SortedSet
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

private[wallet] final class UtxoSnapshotChunkIntegrityException(
  message: String,
  cause: Throwable = null
) extends IllegalStateException(message, cause)

/**
  * Provides an access to version-sensitive wallet-specific indexes:
  *
  * * current wallet status (height, balances)
  * * wallet-related transactions
  * * boxes, spent or not
  *
  */
class WalletRegistry(private val store: LDBVersionedStore)(ws: WalletSettings) extends ScorexLogging {

  import WalletRegistry._

  private val keepHistory = ws.keepSpentBoxes

  // Internal buffer that holds unspent boxes for fast retreival
  private[persistence] val cache: mutable.HashMap[ModifierId,TrackedBox] = mutable.HashMap[ModifierId,TrackedBox]()

  // Serializes only snapshot-chunk marker checks, their transaction, and the matching cache update.
  private val snapshotChunkUpdateLock = new AnyRef

  /**
    * Close wallet registry storage
    */
  def close(): Unit = {
    store.close()
    cache.clear()
  }

  /**
    * Returns the persisted 32-byte registry version encoded as a ModifierId.
    * The version may be synthetic, for example for an intermediate snapshot chunk.
    */
  def lastVersionId: Option[ModifierId] =
    store.lastVersionID.filter(_.length == ModifierIdSize).map(bytesToId)

  /** Check whether an exact registry version is still retained for rollback recovery. */
  def versionIdExists(versionId: ModifierId): Try[Boolean] =
    Try(store.versionIdExists(idToBytes(versionId)))

  /** Read the bounded set of retained rollback versions, newest first. */
  def rollbackVersionIds: Try[Seq[ModifierId]] = Try {
    store.rollbackVersions().toSeq.map { versionId =>
      require(versionId.length == ModifierIdSize,
        s"Retained wallet registry version has invalid length ${versionId.length}")
      bytesToId(versionId)
    }
  }

  /**
    * A first UTXO snapshot scan may populate only a registry which has never
    * carried wallet data. Height zero is insufficient: direct scan mutations
    * can persist boxes and indexes under the pre-genesis version without
    * advancing the aggregate digest.
    */
  def isPristineForUtxoSnapshot: Try[Boolean] = Try {
    lastVersionId.contains(PreGenesisHeader.id) &&
      store.getRange(FirstApplicationKey, AfterLastApplicationKey, limit = 1).isEmpty
  }

  /**
    * Read wallet-related box with metadata
    *
    * @param id - box identifier (the same as Ergobox identifier)
    * @return wallet related box if it is stored in the database, None otherwise
    */
  def getBox(id: BoxId): Option[TrackedBox] = {
    cache.get(bytesToId(id)) match {
      case Some(tb) => Some(tb)
      case None =>
        store.get(boxKey(id)) match {
          case Some(bytes) =>
            TrackedBoxSerializer.parseBytesTry(bytes).toOption match {
              case Some(tb) =>
                cache.put(tb.boxId, tb)
                Some(tb)
              case None => None
            }
          case None => None
        }
    }
  }


  /**
    * Read wallet-related boxes with metadata, see getBox()
    *
    * @param ids - box identifier
    * @return wallet related boxes (optional result for each box)
    */
  def getBoxes(ids: Seq[BoxId]): Seq[Option[TrackedBox]] = {
    ids.map(id => getBox(id))
  }

  /**
    * Read unspent boxes which belong to all the scans
    *
    * @return sequences of all the unspent boxes from the database
    */
  def allUnspentBoxes(): Seq[TrackedBox] = {
    store.getRange(firstUnspentBoxKey, lastUnspentBoxKey)
      .flatMap { case (_, boxId) =>
        getBox(ADKey @@ boxId)
      }
  }

  /**
    * Read unspent boxes which belong to a scan with given id
    *
    * @param scanId - scan identifier
    * @return sequences of scan-related unspent boxes found in the database
    */
  def unspentBoxes(scanId: ScanId, limit: Int = Int.MaxValue): Seq[TrackedBox] = {
    store
      .getRange(firstScanBoxSpaceKey(scanId), lastScanBoxSpaceKey(scanId), limit)
      .flatMap { case (_, boxId) => getBox(ADKey @@ boxId) }
  }

  /**
    * Read boxes within height range which belong to a scan with given id
    *
    * @param scanId - scan identifier
    * @param heightFrom - min inclusion height of unspent boxes
    * @param heightTo - max inclusion height of unspent boxes
    * @return sequences of scan-related boxes found in the database
    */
  def boxesByInclusionHeight(scanId: ScanId, heightFrom: Height, heightTo: Height): Seq[TrackedBox] =
    store
      .getRange(fromScanBoxSpaceKey(scanId, heightFrom), toScanBoxSpaceKey(scanId, heightTo))
      .flatMap { case (_, boxId) => getBox(ADKey @@ boxId) }

  /**
    * Read unspent boxes within height range which belong to a scan with given id
    *
    * @param scanId     - scan identifier
    * @param heightFrom - min inclusion height of unspent boxes
    * @param heightTo   - max inclusion height of unspent boxes
    * @return sequences of scan-related unspent boxes found in the database
    */
  def unspentBoxesByInclusionHeight(scanId: ScanId, heightFrom: Height, heightTo: Height): Seq[TrackedBox] =
    boxesByInclusionHeight(scanId, heightFrom, heightTo).filter(_.spendingHeightOpt.isEmpty)

  /**
    * Read spent boxes within height range which belong to a scan with given id
    *
    * @param scanId     - scan identifier
    * @param heightFrom - min inclusion height of unspent boxes
    * @param heightTo   - max inclusion height of unspent boxes
    * @return sequences of scan-related spent boxes found in the database
    */
  def spentBoxesByInclusionHeight(scanId: ScanId, heightFrom: Height, heightTo: Height): Seq[TrackedBox] =
    boxesByInclusionHeight(scanId, heightFrom, heightTo).filter(_.spendingHeightOpt.isDefined)

  /**
    * Read spent boxes which belong to a scan with given id
    *
    * @param scanId - scan identifier
    * @return sequences of scan-related spent boxes found in the database
    */
  def spentBoxes(scanId: ScanId): Seq[TrackedBox] = {
    store.getRange(firstSpentScanBoxSpaceKey(scanId), lastSpentScanBoxSpaceKey(scanId))
      .flatMap { case (_, boxId) =>
        getBox(ADKey @@ boxId)
      }
  }

  /**
    * Unspent boxes belong to the wallet (payments scan)
    */
  def walletUnspentBoxes(limit: Int = Int.MaxValue): Seq[TrackedBox] = unspentBoxes(Constants.PaymentsScanId, limit)

  /**
    * Spent boxes belong to the wallet (payments scan)
    */
  def walletSpentBoxes(): Seq[TrackedBox] = spentBoxes(Constants.PaymentsScanId)

  /**
    * Read wallet boxes, both spent or not
    *
    * @param scanId scan identifier
    * @return sequence of scan-related boxes
    */
  def confirmedBoxes(scanId: ScanId): Seq[TrackedBox] = {
    unspentBoxes(scanId) ++ spentBoxes(scanId)
  }

  /**
    * Read boxes belong to the payment scan, both spent or not
    *
    * @return sequence of (P2PK-payment)-related boxes
    */
  def walletConfirmedBoxes(): Seq[TrackedBox] = confirmedBoxes(Constants.PaymentsScanId)

  /**
    * Read transaction with wallet-related metadata
    *
    * @param id - transaction identifier
    * @return
    */
  def getTx(id: ModifierId): Option[WalletTransaction] = {
    store.get(txKey(id)).flatMap(r => WalletTransactionSerializer.parseBytesTry(r).toOption)
  }

  /**
    * Read all the wallet-related transactions
    *
    * @return all the transactions for all the scans
    */
  def allWalletTxs(): Seq[WalletTransaction] = {
    store.getRange(FirstTxSpaceKey, LastTxSpaceKey)
      .flatMap { case (_, txBytes) =>
        WalletTransactionSerializer.parseBytesTry(txBytes).toOption
      }
  }

  /**
    * Read wallet-related transactions for certain heights
    *
    * @param heightFrom - height to start from (inclusive)
    * @param heightTo - height to finish at (inclusive)
    * @return - wallet transactions for the heights range provided
    */
  def walletTxsBetween(scanId: ScanId, heightFrom: Height, heightTo: Height): Seq[WalletTransaction] = {
    val firstKey = firstIncludedScanTransactionSpaceKey(scanId, heightFrom)
    val lastKey = lastIncludedScanTransactionSpaceKey(scanId, heightTo)

    // Get wallet transactions from heightFrom (inclusive) to heightTo (inclusive)
    val range = store.getRange(firstKey, lastKey)
    range.flatMap { case (_, txId) =>
      store.get(txKey(txId)) match {
        case Some(txBytes) => WalletTransactionSerializer.parseBytesTry(txBytes) match {
          case Success(tx) =>
            Some(tx)
          case Failure(t) =>
            log.error(s"Transaction ${Base16.encode(txId)} can't be read from the db", t); None
        }
        case None =>
          log.error(s"Transaction ${Base16.encode(txId)} is found in indexes but not db"); None
      }
    }
  }

  /**
    * Read aggregate wallet information
    *
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
  def updateDigest(bag: KeyValuePairsBag)(updateF: WalletDigest => Try[WalletDigest]): Try[KeyValuePairsBag] =
    updateF(fetchDigest()).map(digest => putDigest(bag, digest))

  /**
    *
    * Updates indexes according to data extracted from a block and performs versioned update.
    *
    * @param scanResults - block scan data (outputs created and spent along with corresponding transactions)
    * @param blockId     - block identifier
    * @param blockHeight - block height
    */
  def updateOnBlock(scanResults: ScanResults, blockId: ModifierId, blockHeight: Int): Try[Unit] = {

    // first, put newly created outputs and related transactions into key-value bag
    cache ++= scanResults.outputs.map(b => b.boxId -> b)
    val bag1 = putBoxes(KeyValuePairsBag.empty, scanResults.outputs)
    val bag2 = putTxs(bag1, scanResults.relatedTransactions)

    // process spent boxes
    val spentBoxesWithTx = scanResults.inputsSpent.map(t => t.inputTxId -> t.trackedBox)
    val bag3 = processSpentBoxes(bag2, spentBoxesWithTx, blockHeight)

    // and update wallet digest
    updateDigest(bag3) { case WalletDigest(height, wBalance, wTokensSeq) =>
      if (height + 1 != blockHeight) {
        log.error(s"Blocks were skipped during wallet scanning, from $height until $blockHeight")
      }
      val spentWalletBoxes = spentBoxesWithTx.map(_._2).filter(_.scans.contains(PaymentsScanId))
      updateWalletDigest(WalletDigest(height, wBalance, wTokensSeq), scanResults.outputs, spentWalletBoxes, blockHeight)
    }.flatMap { bag4 =>
      bag4.transact(store, idToBytes(blockId))
    }
  }

  /**
    * Updates wallet indexes from a UTXO snapshot chunk without inventing pre-snapshot transactions.
    */
  def updateOnSnapshotChunk(scanResults: ScanResults,
                            snapshotBlockId: ModifierId,
                            snapshotHeight: Int,
                            subtreeIndex: Int,
                            finalChunk: Boolean): Try[Unit] =
    updateOnSnapshotChunk(
      scanResults,
      snapshotBlockId,
      snapshotHeight,
      subtreeIndex,
      subtreeIndex + 1,
      finalChunk
    )

  /**
    * Updates wallet indexes and binds the exact covered part range to the durable replay marker.
    */
  def updateOnSnapshotChunk(scanResults: ScanResults,
                            snapshotBlockId: ModifierId,
                            snapshotHeight: Int,
                            subtreeIndex: Int,
                            nextSubtreeIndex: Int,
                            finalChunk: Boolean): Try[Unit] = {
    if (subtreeIndex < 0 || nextSubtreeIndex <= subtreeIndex) {
      Failure(new IllegalArgumentException(
        s"Invalid UTXO snapshot part range [$subtreeIndex, $nextSubtreeIndex)"))
    } else if (scanResults.inputsSpent.nonEmpty || scanResults.relatedTransactions.nonEmpty) {
      Failure(new IllegalArgumentException("Snapshot chunk scan data must contain only outputs"))
    } else {
      Try {
        val markerKey = snapshotChunkMarkerKey(snapshotBlockId, subtreeIndex)
        val markerValue = snapshotChunkMarkerValue(
          scanResults,
          snapshotHeight,
          nextSubtreeIndex,
          finalChunk
        )

        snapshotChunkUpdateLock.synchronized {
          store.get(markerKey) match {
            case Some(existing) if existing.sameElements(markerValue) =>
              validateSnapshotChunkApplication(
                scanResults,
                snapshotBlockId,
                snapshotHeight,
                subtreeIndex,
                nextSubtreeIndex,
                finalChunk,
                existing)
            case Some(_) =>
              Failure(new UtxoSnapshotChunkIntegrityException(
                s"UTXO snapshot chunk $subtreeIndex for $snapshotBlockId was already applied with different contents"
              ))
            case None =>
              val bag1 = putBoxes(KeyValuePairsBag.empty, scanResults.outputs)
              updateDigest(bag1) { digest =>
                val nextHeight = if (finalChunk) snapshotHeight else digest.height
                updateWalletDigest(digest, scanResults.outputs, Seq.empty, nextHeight)
              }.flatMap { bag2 =>
                val bag3 = bag2.copy(toInsert = bag2.toInsert :+ markerKey -> markerValue)
                bag3.transact(store, snapshotChunkVersion(snapshotBlockId, subtreeIndex, finalChunk)).map { _ =>
                  cache ++= scanResults.outputs.map(b => b.boxId -> b)
                  ()
                }
              }
          }
        }
      }.flatten
    }
  }

  /**
    * Validate a snapshot chunk marker and every wallet row/index it claims without mutating the registry.
    */
  private[wallet] def validateSnapshotChunk(scanResults: ScanResults,
                                             snapshotBlockId: ModifierId,
                                             snapshotHeight: Int,
                                             subtreeIndex: Int,
                                             nextSubtreeIndex: Int,
                                             finalChunk: Boolean): Try[Unit] = {
    if (subtreeIndex < 0 || nextSubtreeIndex <= subtreeIndex) {
      Failure(new IllegalArgumentException(
        s"Invalid UTXO snapshot part range [$subtreeIndex, $nextSubtreeIndex)"))
    } else if (scanResults.inputsSpent.nonEmpty || scanResults.relatedTransactions.nonEmpty) {
      Failure(new IllegalArgumentException("Snapshot chunk scan data must contain only outputs"))
    } else {
      snapshotChunkUpdateLock.synchronized {
        Try(store.get(snapshotChunkMarkerKey(snapshotBlockId, subtreeIndex))).flatMap {
          case Some(marker) =>
            validateSnapshotChunkApplication(
              scanResults,
              snapshotBlockId,
              snapshotHeight,
              subtreeIndex,
              nextSubtreeIndex,
              finalChunk,
              marker)
          case None =>
            Failure(new UtxoSnapshotChunkIntegrityException(
              s"UTXO snapshot chunk marker at subtree $subtreeIndex for $snapshotBlockId is missing"))
        }
      }
    }
  }

  /**
    * Return the first unmarked snapshot part and reject any marker which appears after a gap.
    * This is a structural check only; resume authenticates the last durable batch separately.
    */
  def contiguousSnapshotCursor(snapshotBlockId: ModifierId,
                               totalParts: Int,
                               batchSize: Int): Try[Int] = snapshotChunkUpdateLock.synchronized {
    Try {
      require(totalParts > 0, s"Invalid UTXO snapshot part count $totalParts")
      require(batchSize > 0, s"Invalid UTXO snapshot scan batch size $batchSize")

      var index = 0
      var frontier = 0
      var markerGap = false
      while (index < totalParts) {
        val markerPresent = store.get(snapshotChunkMarkerKey(snapshotBlockId, index)).nonEmpty
        if (markerPresent && markerGap) {
          throw new UtxoSnapshotChunkIntegrityException(
            s"UTXO snapshot chunk marker at subtree $index for $snapshotBlockId appears after a gap")
        }
        if (markerPresent) {
          frontier = Math.min(totalParts.toLong, index.toLong + batchSize.toLong).toInt
        } else {
          markerGap = true
        }
        index = Math.min(totalParts.toLong, index.toLong + batchSize.toLong).toInt
      }
      frontier
    }
  }

  private def updateWalletDigest(currentDigest: WalletDigest,
                                 receivedBoxes: Seq[TrackedBox],
                                 spentWalletBoxes: Seq[TrackedBox],
                                 nextHeight: Int): Try[WalletDigest] = {
    val receivedWalletBoxes = receivedBoxes.filter(_.scans.contains(PaymentsScanId))
    val spentAmt = spentWalletBoxes.map(_.box.value).sum
    val spentTokensAmt = tokenAmounts(spentWalletBoxes)
    val receivedTokensAmt = tokenAmounts(receivedWalletBoxes)

    val wTokens = mutable.LinkedHashMap(currentDigest.walletAssetBalances: _*)

    val increasedTokenBalances = receivedTokensAmt.foldLeft(wTokens) { case (acc, (encodedId, amt)) =>
      acc += encodedId -> (acc.getOrElse(encodedId, 0L) + amt)
    }

    val newTokensBalance = spentTokensAmt
      .foldLeft(increasedTokenBalances) { case (acc, (encodedId, amt)) =>
        val decreasedAmt = acc.getOrElse(encodedId, 0L) - amt
        if (decreasedAmt > 0) {
          acc += encodedId -> decreasedAmt
        } else {
          acc -= encodedId
        }
      }

    val receivedAmt = receivedWalletBoxes.map(_.box.value).sum
    val newBalance = currentDigest.walletBalance + receivedAmt - spentAmt
    if ((newBalance >= 0 && newTokensBalance.forall(_._2 >= 0)) || ws.testMnemonic.isDefined)
      Success(WalletDigest(nextHeight, newBalance, newTokensBalance.toSeq))
    else
      Failure(new IllegalStateException("Balance could not be negative"))
  }

  private def tokenAmounts(boxes: Seq[TrackedBox]): Map[EncodedTokenId, Long] =
    boxes
      .flatMap(_.box.additionalTokens.toArray)
      .foldLeft(Map.empty[EncodedTokenId, Long]) { case (acc, (id, amt)) =>
        val encodedId = encodedTokenId(id)
        acc.updated(encodedId, acc.getOrElse(encodedId, 0L) + amt)
      }

  private def snapshotChunkVersion(snapshotBlockId: ModifierId, subtreeIndex: Int, finalChunk: Boolean): Array[Byte] =
    if (finalChunk) {
      idToBytes(snapshotBlockId)
    } else {
      Blake2b256.hash(idToBytes(snapshotBlockId) ++ Ints.toByteArray(subtreeIndex))
    }

  private def snapshotChunkMarkerKey(snapshotBlockId: ModifierId, subtreeIndex: Int): Array[Byte] =
    Array(SnapshotChunkMarkerPrefix) ++ idToBytes(snapshotBlockId) ++ Ints.toByteArray(subtreeIndex)

  private def snapshotChunkMarkerValue(scanResults: ScanResults,
                                       snapshotHeight: Int,
                                       nextSubtreeIndex: Int,
                                       finalChunk: Boolean): Array[Byte] = {
    val seed = Array(SnapshotChunkMarkerFormatVersion) ++
      Ints.toByteArray(snapshotHeight) ++
      Ints.toByteArray(nextSubtreeIndex) ++
      Array(if (finalChunk) 1.toByte else 0.toByte)
    // Snapshot traversal is deterministic; output sequence ordering is deliberately part of the batch identity.
    val digest = scanResults.outputs.foldLeft(Blake2b256.hash(seed)) { case (digest, box) =>
      Blake2b256.hash(digest ++ snapshotChunkMarkerTrackedBoxBytes(box))
    }
    Array(SnapshotChunkMarkerFormatVersion) ++ digest
  }

  private def snapshotChunkMarkerTrackedBoxBytes(box: TrackedBox): Array[Byte] =
    if (box.scans.size < 2) {
      TrackedBoxSerializer.toBytes(box)
    } else {
      val scanOrdering = Ordering.by[ScanId, Short](_.toShort)
      val orderedScans = SortedSet.empty[ScanId](scanOrdering) ++ box.scans
      TrackedBoxSerializer.toBytes(box.copy(scans = orderedScans))
    }

  private def validateSnapshotChunkApplication(scanResults: ScanResults,
                                               snapshotBlockId: ModifierId,
                                               snapshotHeight: Int,
                                               subtreeIndex: Int,
                                               nextSubtreeIndex: Int,
                                               finalChunk: Boolean,
                                               marker: Array[Byte]): Try[Unit] = Try {
    require(scanResults.inputsSpent.isEmpty && scanResults.relatedTransactions.isEmpty,
      s"Invalid UTXO snapshot scan results at subtree $subtreeIndex for $snapshotBlockId")
    val expectedMarker = snapshotChunkMarkerValue(
      scanResults,
      snapshotHeight,
      nextSubtreeIndex,
      finalChunk)
    require(marker.sameElements(expectedMarker),
      s"UTXO snapshot chunk marker at subtree $subtreeIndex for $snapshotBlockId " +
        "does not match the immutable snapshot batch")

    scanResults.outputs.foreach { expectedBox =>
      val storedBytes = store.get(boxKey(expectedBox.box.id)).getOrElse {
        throw new IllegalStateException(
          s"UTXO snapshot chunk $subtreeIndex for $snapshotBlockId is missing tracked box ${expectedBox.boxId}")
      }
      val storedBox = TrackedBoxSerializer.parseBytesTry(storedBytes).get
      require(
        snapshotChunkMarkerTrackedBoxBytes(storedBox)
          .sameElements(snapshotChunkMarkerTrackedBoxBytes(expectedBox)),
        s"UTXO snapshot chunk $subtreeIndex for $snapshotBlockId has inconsistent tracked box ${expectedBox.boxId}")
      boxIndexes(expectedBox).foreach { case (indexKey, expectedValue) =>
        require(store.get(indexKey).exists(_.sameElements(expectedValue)),
          s"UTXO snapshot chunk $subtreeIndex for $snapshotBlockId is missing an index for ${expectedBox.boxId}")
      }
    }
  }.recoverWith {
    case integrityFailure: UtxoSnapshotChunkIntegrityException => Failure(integrityFailure)
    case t => Failure(new UtxoSnapshotChunkIntegrityException(
      s"UTXO snapshot chunk $subtreeIndex for $snapshotBlockId failed integrity validation: ${t.getMessage}",
      t))
  }

  def rollback(version: VersionTag): Try[Unit] = {
    cache.clear()
    store.rollbackTo(org.ergoplatform.core.versionToBytes(version))
  }

  /**
    * Transits used boxes to a spent state or simply deletes them depending on a settings.
    */
  private[persistence] def processSpentBoxes(bag: KeyValuePairsBag,
                                             spentBoxes: Seq[(ModifierId, TrackedBox)],
                                             spendingHeight: Int): KeyValuePairsBag = {
    if (keepHistory) {
      val outSpent: Seq[TrackedBox] = spentBoxes.flatMap { case (_, tb) =>
        getBox(tb.box.id).orElse {
          bag.toInsert.find(_._1.sameElements(boxKey(tb))).flatMap { case (_, tbBytes) =>
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

      cache --= spentBoxes.map(_._2.boxId)
      val bagBeforePut = removeBoxes(bag, spentBoxes.map(_._2))
      cache ++= updatedBoxes.map(b => b.boxId -> b)
      putBoxes(bagBeforePut, updatedBoxes)
    } else {
      cache --= spentBoxes.map(_._2.boxId)
      removeBoxes(bag, spentBoxes.map(_._2))
    }
  }

  /**
    * Updates scans of a box stored in the wallet database,
    * puts the box into the database if it is not there
    * removes the box from the database if its there and scanIds are empty
    *
    * @param newScans - ids of new scans box should be associated with
    * @param box - box to be updated (new version)
    * @return
    */
  def updateScans(newScans: Set[ScanId], box: ErgoBox): Try[Unit] = Try {
    val bag0 = KeyValuePairsBag.empty
    val oldBox = getBox(box.id) // read old version from the database
    val oldScans = oldBox.map(_.scans).getOrElse(Set.empty)

    val newBox = TrackedBox(box, box.creationHeight, newScans)

    val bag1 = (oldScans.isEmpty, newScans.isEmpty) match {
      case (false, false) =>
        // replace scans of the box by removing it along with indexes related to old scans,
        // and then adding the box with indexes related to the new scans
        cache.update(oldBox.get.boxId, newBox)
        putBox(removeBox(bag0, oldBox.get), newBox)
      case (false, true) =>
        // if new scans are empty, remove the box along with indexes
        cache.remove(oldBox.get.boxId)
        removeBox(bag0, oldBox.get)
      case (true, false) =>
        // if old scans are empty, add the box along with indexes
        cache.put(newBox.boxId, newBox)
        putBox(bag0, newBox)
      case (true, true) =>
        //old and new scans are empty, can't do anything useful
        throw new Exception("Can't remove a box which does not exist")
    }

    // Flag showing that box has been added to the payments app (p2pk-wallet) or removed from it
    // If true, we need to update wallet digest
    val digestChanged = (oldScans.contains(Constants.PaymentsScanId) || newScans.contains(Constants.PaymentsScanId)) &&
                        !(oldScans.contains(Constants.PaymentsScanId) && newScans.contains(Constants.PaymentsScanId))

    val bag2 = if (digestChanged) {
      val digest = fetchDigest()

      val walletAssets = mutable.LinkedHashMap(digest.walletAssetBalances :_*)
      val boxAssets = TransactionBuilder.collTokensToMap(box.additionalTokens)

      val updDigest = if (!oldScans.contains(Constants.PaymentsScanId) && newScans.contains(Constants.PaymentsScanId)) {
        AssetUtils.mergeAssetsMut(walletAssets, boxAssets) //mutating digest!
        WalletDigest(
          digest.height,
          digest.walletBalance + box.value,
          walletAssets.toArray[(EncodedTokenId, Long)])
      } else if (oldScans.contains(Constants.PaymentsScanId) && !newScans.contains(Constants.PaymentsScanId)) {
        //mutating digest! exception can be thrown here
        AssetUtils.subtractAssetsMut(walletAssets, boxAssets)
        WalletDigest(
          digest.height,
          digest.walletBalance - box.value,
          walletAssets.toArray[(EncodedTokenId, Long)])
      } else {
        throw new Exception(s"Wallet can't update digest for a box with old scans $oldScans, new ones $newScans")
      }
      putDigest(bag1, updDigest)
    } else {
      bag1
    }

    bag2.transact(store, store.lastVersionID.getOrElse(scorex.util.Random.randomBytes(32)))
  }

  /**
    * Remove association between an application and a box.
    * Please note that in case of rollback association remains removed!
    *
    * @param boxId  box identifier
    * @param scanId scan identifier
    */
  def removeScan(boxId: BoxId, scanId: ScanId): Try[Unit] = {
    getBox(boxId) match {
      case Some(tb) =>
        val newScans = tb.scans - scanId
        updateScans(newScans, tb.box)

      case None => Failure(new Exception(s"No box with id ${Algos.encode(boxId)} found in the wallet database"))
    }
  }
}

object WalletRegistry {

  import scorex.db.ByteArrayUtils._

  val PreGenesisStateVersion: Array[Byte] = idToBytes(PreGenesisHeader.id)

  def registryFolder(settings: ErgoSettings): File = new File(s"${settings.directory}/wallet/registry")

  def apply(settings: ErgoSettings): Try[WalletRegistry] = Try {
      val dir = registryFolder(settings)
      dir.mkdirs()
      new LDBVersionedStore(dir, settings.nodeSettings.keepVersions)
    }.flatMap(store => initializeOpenedStore(store, settings.walletSettings))

  private[persistence] def initializeOpenedStore(store: LDBVersionedStore,
                                                  walletSettings: WalletSettings): Try[WalletRegistry] = {
    val initialized = Try(store.lastVersionID).flatMap {
      case Some(version) =>
        Try {
          require(version.length == ModifierIdSize,
            s"Invalid wallet registry version length ${version.length}")
          new WalletRegistry(store)(walletSettings)
        }
      case None =>
        Try(store.getRange(FirstApplicationKey, AfterLastApplicationKey, limit = 1)).flatMap {
          case entries if entries.nonEmpty =>
            Failure(new IllegalStateException(
              "Wallet registry has application data but no durable version"))
          case _ =>
            // Create the pre-genesis checkpoint only for a genuinely empty registry. An older
            // checkpoint may legitimately have been pruned while a newer version remains current.
            store.update(PreGenesisStateVersion, Seq.empty, Seq.empty).flatMap { _ =>
              Try(new WalletRegistry(store)(walletSettings))
            }
        }
    }

    initialized.recoverWith { case initializationFailure =>
      Try(store.close()).failed.foreach { closeFailure =>
        if (closeFailure ne initializationFailure) {
          initializationFailure.addSuppressed(closeFailure)
        }
      }
      Failure(initializationFailure)
    }
  }

  private val BoxKeyPrefix: Byte = 0x01
  private val TxKeyPrefix: Byte = 0x02

  // box indexes prefixes
  private val UnspentIndexPrefix: Byte = 0x03
  private val SpentIndexPrefix: Byte = 0x04

  // box index prefix that tracks all (spent & unspent) boxes by inclusion height
  private val InclusionHeightScanBoxPrefix: Byte = 0x07

  // tx index prefix that tracks transactions by inclusion height
  private val InclusionHeightScanTxPrefix: Byte = 0x08

  // Applied UTXO snapshot chunks, keyed by snapshot block id and starting subtree index.
  private val SnapshotChunkMarkerPrefix: Byte = 0x09

  // All registry-owned application keys currently use prefixes 0x01 through 0x09.
  private val FirstApplicationKey: Array[Byte] = Array(BoxKeyPrefix)
  private val AfterLastApplicationKey: Array[Byte] = Array((SnapshotChunkMarkerPrefix + 1).toByte)

  // Stored as the first marker-value byte so future encodings cannot be mistaken for this format.
  private val SnapshotChunkMarkerFormatVersion: Byte = 0x02

  private val FirstTxSpaceKey: Array[Byte] = TxKeyPrefix +: Array.fill(32)(0: Byte)
  private val LastTxSpaceKey: Array[Byte] = TxKeyPrefix +: Array.fill(32)(-1: Byte)

  // All the unspent boxes range, dependless on scan
  private val firstUnspentBoxKey: Array[Byte] = UnspentIndexPrefix +: Array.fill(34)(0: Byte)
  private val lastUnspentBoxKey: Array[Byte] = UnspentIndexPrefix +: Array.fill(34)(-1: Byte)

  /** Performance optimized helper, which avoid unnecessary allocations and creates the resulting
    * key bytes directly from the given parameters.
    * It is allocation and boxing free.
    *
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
  private[persistence] final def composeKeyWithId(prefix: Byte, scanId: ScanId, suffixId: Array[Byte]): Array[Byte] = {
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

  /** Same as [[composeKey()]] with additional height parameter. */
  private[persistence] final def composeKey(prefix: Byte, scanId: ScanId, height: Int): Array[Byte] = {
    val res = new Array[Byte](39) // 1 byte for prefix + 2 for scanId + 4 for height
    res(0) = prefix
    putShort(res, pos = 1, scanId)
    putInt(res, pos = 3, height)
    res
  }


  /** Same as [[composeKey()]] with additional height parameter and suffix given by id. */
  private[persistence] final def composeKeyWithHeightAndId(prefix: Byte, scanId: ScanId,
                                                           height: Int, suffixId: Array[Byte]): Array[Byte] = {
    val res = new Array[Byte](7 + suffixId.length) // 1 byte for prefix + 2 for scanId + 4 for height
    res(0) = prefix
    putShort(res, pos = 1, scanId)
    putInt(res, pos = 3, height)
    putBytes(res, 7, suffixId)
    res
  }

  private def firstScanBoxSpaceKey(scanId: ScanId): Array[Byte] =
    composeKey(UnspentIndexPrefix, scanId, 0)

  private def lastScanBoxSpaceKey(scanId: ScanId): Array[Byte] =
    composeKey(UnspentIndexPrefix, scanId, -1)

  private def firstSpentScanBoxSpaceKey(scanId: ScanId): Array[Byte] =
    composeKey(SpentIndexPrefix, scanId, 0)

  private def lastSpentScanBoxSpaceKey(scanId: ScanId): Array[Byte] =
    composeKey(SpentIndexPrefix, scanId, -1)

  private def fromScanBoxSpaceKey(scanId: ScanId, height: Int): Array[Byte] =
    composeKey(InclusionHeightScanBoxPrefix, scanId, height)

  private def toScanBoxSpaceKey(scanId: ScanId, height: Int): Array[Byte] =
    composeKey(InclusionHeightScanBoxPrefix, scanId, height, -1)

  private def firstIncludedScanTransactionSpaceKey(scanId: ScanId, height: Int): Array[Byte] =
    composeKey(InclusionHeightScanTxPrefix, scanId, height)

  private def lastIncludedScanTransactionSpaceKey(scanId: ScanId, height: Int): Array[Byte] =
    composeKey(InclusionHeightScanTxPrefix, scanId, height, -1)

  private val RegistrySummaryKey: Array[Byte] = Array(0x02: Byte)

  private def boxKey(trackedBox: TrackedBox): Array[Byte] = BoxKeyPrefix +: trackedBox.box.id

  private def boxKey(id: BoxId): Array[Byte] = {
    // exported from ArrayOps +: to avoid boxing
    val currentLength = id.length
    val result = new Array[Byte](currentLength + 1)
    result(0) = BoxKeyPrefix
    Array.copy(id, 0, result, 1, currentLength)
    result
  }

  private def txKey(id: ModifierId): Array[Byte] = TxKeyPrefix +: idToBytes(id)

  private def txKey(id: Array[Byte]): Array[Byte] = TxKeyPrefix +: id

  private def boxToKvPair(box: TrackedBox) = boxKey(box) -> TrackedBoxSerializer.toBytes(box)

  private def spentIndexKey(scanId: ScanId, trackedBox: TrackedBox): Array[Byte] = {
    val prefix = if (trackedBox.isSpent) SpentIndexPrefix else UnspentIndexPrefix
    composeKeyWithId(prefix, scanId, trackedBox.box.id)
  }

  private def inclusionHeightScanBoxIndexKey(scanId: ScanId, trackedBox: TrackedBox): Array[Byte] = {
    val inclusionHeight = trackedBox.inclusionHeightOpt.getOrElse(0)
    composeKeyWithHeightAndId(InclusionHeightScanBoxPrefix, scanId, inclusionHeight, trackedBox.box.id)
  }

  private def boxIndexKeys(box: TrackedBox): Seq[Array[Byte]] = {
    box.scans.toSeq.flatMap { scanId =>
      Seq(
        spentIndexKey(scanId, box),
        inclusionHeightScanBoxIndexKey(scanId, box)
      )
    }
  }

  private def boxIndexes(box: TrackedBox): Seq[(Array[Byte], Array[Byte])] = {
    boxIndexKeys(box).map(k => k -> box.box.id)
  }

  private[persistence] def putBox(bag: KeyValuePairsBag, box: TrackedBox): KeyValuePairsBag = {
    val scanIndexUpdates = boxIndexes(box)
    val newKvPairs = scanIndexUpdates :+ boxToKvPair(box)
    bag.copy(toInsert = bag.toInsert ++ newKvPairs)
  }

  private[persistence] def putBoxes(bag: KeyValuePairsBag, boxes: Seq[TrackedBox]): KeyValuePairsBag = {
    boxes.foldLeft(bag) { case (b, box) => putBox(b, box) }
  }

  private[persistence] def removeBox(bag: KeyValuePairsBag, box: TrackedBox): KeyValuePairsBag = {
    val boxKeys = boxIndexKeys(box) :+ boxKey(box)

    bag.toInsert.find(_._1.sameElements(boxKey(box))) match {
      case Some((_, _)) =>
        bag.copy(toInsert = bag.toInsert.filterNot { case (k, _) =>
          boxKeys.exists(_.sameElements(k))
        })
      case None =>
        bag.copy(toRemove = bag.toRemove ++ boxKeys)
    }
  }

  private[persistence] def removeBoxes(bag: KeyValuePairsBag, boxes: Seq[TrackedBox]): KeyValuePairsBag = {
    boxes.foldLeft(bag) { case (b, box) => removeBox(b, box) }
  }

  private def inclusionHeightScanTxIndexKey(scanId: ScanId, tx: WalletTransaction): Array[Byte] = {
    val inclusionHeight = tx.inclusionHeight
    composeKeyWithHeightAndId(InclusionHeightScanTxPrefix, scanId, inclusionHeight, tx.idBytes)
  }

  private def txIndexKeys(tx: WalletTransaction): Seq[Array[Byte]] = {
    tx.scanIds.map { scanId =>
      inclusionHeightScanTxIndexKey(scanId, tx)
    }
  }

  private def txToKvPairs(tx: WalletTransaction): Seq[(Array[Byte], Array[Byte])] = {
    txIndexKeys(tx).map(k => k -> tx.idBytes) :+
      (txKey(tx.id) -> WalletTransactionSerializer.toBytes(tx))
  }

  private[persistence] def putTx(bag: KeyValuePairsBag, wtx: WalletTransaction): KeyValuePairsBag = {
    bag.copy(toInsert = bag.toInsert ++ txToKvPairs(wtx))
  }

  private[persistence] def putTxs(bag: KeyValuePairsBag, txs: Seq[WalletTransaction]): KeyValuePairsBag = {
    bag.copy(toInsert = bag.toInsert ++ txs.flatMap(txToKvPairs))
  }

  private[persistence] def removeTxs(bag: KeyValuePairsBag, txs: Seq[WalletTransaction]): KeyValuePairsBag = {
    bag.copy(toRemove = bag.toRemove ++ txs.flatMap(txToKvPairs).map(_._1))
  }

  private[persistence] def putDigest(bag: KeyValuePairsBag, digest: WalletDigest): KeyValuePairsBag = {
    val registryBytes = WalletDigestSerializer.toBytes(digest)
    bag.copy(toInsert = bag.toInsert :+ RegistrySummaryKey -> registryBytes)
  }
}

/**
  * This class collects data for versioned database update
  *
  * @param toInsert - key-value pairs to write to the database
  * @param toRemove - keys to remove from the database
  */
case class KeyValuePairsBag(toInsert: Seq[(Array[Byte], Array[Byte])],
                            toRemove: Seq[Array[Byte]]) {

  /**
    * Applies non-versioned transaction to a given `store`.
    *
    */
  def transact(store: LDBVersionedStore): Try[Unit] = transact(store, None)

  /**
    * Applies versioned transaction to a given `store`.
    */
  def transact(store: LDBVersionedStore, version: Array[Byte]): Try[Unit] = transact(store, Some(version))

  private def transact(store: LDBVersionedStore, versionOpt: Option[Array[Byte]]): Try[Unit] =
    if (toInsert.nonEmpty || toRemove.nonEmpty) {
      store.update(versionOpt.getOrElse(scorex.utils.Random.randomBytes()), toRemove, toInsert)
    } else {
      Success(())
    }

}

object KeyValuePairsBag {

  def empty: KeyValuePairsBag = KeyValuePairsBag(Seq.empty, Seq.empty)

}
