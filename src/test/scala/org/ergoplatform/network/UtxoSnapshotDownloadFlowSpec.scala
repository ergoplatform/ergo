package org.ergoplatform.network

import org.ergoplatform.nodeView.history.storage.modifierprocessors.UtxoSetSnapshotDownloadPlan
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.ErgoCorePropertyTest
import scorex.crypto.authds.avltree.batch.VersionedLDBAVLStorage
import scorex.crypto.hash.Digest32
import scorex.util.ModifierId

import scala.util.{Failure, Success}

class UtxoSnapshotDownloadFlowSpec extends ErgoCorePropertyTest {
  property("a zero-chunk snapshot initializes exactly once immediately after manifest registration") {
    val height = 100
    val blockId = ModifierId @@ Algos.encode(Array.fill(32)(1: Byte))
    val plan = UtxoSetSnapshotDownloadPlan(
      createdTime = 1L,
      latestUpdateTime = 1L,
      snapshotHeight = height,
      utxoSetRootHash = Digest32 @@ Array.fill(32)(2: Byte),
      utxoSetTreeHeight = 1,
      expectedChunkIds = IndexedSeq.empty,
      downloadedChunkIds = IndexedSeq.empty,
      downloadingChunks = 0,
      peersToDownload = Seq.empty
    )
    var initialized = Vector.empty[(Int, ModifierId)]
    var requestedMore = 0

    ErgoNodeViewSynchronizer.continueUtxoSnapshotDownload(
      plan,
      snapshotAlreadyApplied = false,
      snapshotMatchesCurrentHeader = true,
      blockIdAtHeight = _ => Some(blockId),
      initialize = (h, id) => initialized :+= h -> id,
      requestMore = () => requestedMore += 1,
      invalidateAndRedownload = () => fail("current zero-chunk snapshot was invalidated"),
      warnAlreadyApplied = () => fail("zero-chunk snapshot was treated as already applied"),
      reportMissingHeader = _ => fail("zero-chunk snapshot could not resolve its block")
    )

    initialized shouldBe Vector(height -> blockId)
    requestedMore shouldBe 0
  }

  property("restart recovery requeues only allocated false chunks not already requested") {
    val first = Digest32 @@ Array.fill(32)(3: Byte)
    val cleared = Digest32 @@ Array.fill(32)(4: Byte)
    val stillRequested = Digest32 @@ Array.fill(32)(5: Byte)
    val plan = UtxoSetSnapshotDownloadPlan(
      createdTime = 1L,
      latestUpdateTime = 1L,
      snapshotHeight = 100,
      utxoSetRootHash = Digest32 @@ Array.fill(32)(6: Byte),
      utxoSetTreeHeight = 1,
      expectedChunkIds = IndexedSeq(first, cleared, stillRequested),
      downloadedChunkIds = IndexedSeq(true, false, false),
      downloadingChunks = 2,
      peersToDownload = Seq.empty
    )
    var requeued = Vector.empty[Digest32]

    ErgoNodeViewSynchronizer.requeueAllocatedUtxoSnapshotChunks(
      plan,
      isRequested = id => id.sameElements(stillRequested),
      requestAgain = id => requeued :+= id
    )

    requeued.size shouldBe 1
    requeued.head.sameElements(cleared) shouldBe true
  }

  property("changed best-header root invalidates a completed plan instead of initializing") {
    val height = 101
    val blockId = ModifierId @@ Algos.encode(Array.fill(32)(7: Byte))
    val plan = UtxoSetSnapshotDownloadPlan(
      createdTime = 1L,
      latestUpdateTime = 1L,
      snapshotHeight = height,
      utxoSetRootHash = Digest32 @@ Array.fill(32)(8: Byte),
      utxoSetTreeHeight = 2,
      expectedChunkIds = IndexedSeq.empty,
      downloadedChunkIds = IndexedSeq.empty,
      downloadingChunks = 0,
      peersToDownload = Seq.empty
    )
    val changedStateRoot = VersionedLDBAVLStorage.digest(
      Digest32 @@ Array.fill(32)(9: Byte),
      plan.utxoSetTreeHeight
    )
    var initialized = 0
    var invalidated = 0

    ErgoNodeViewSynchronizer.continueUtxoSnapshotDownload(
      plan,
      snapshotAlreadyApplied = false,
      snapshotMatchesCurrentHeader =
        ErgoNodeViewSynchronizer.snapshotMatchesCurrentHeader(plan, Some(changedStateRoot)),
      blockIdAtHeight = _ => Some(blockId),
      initialize = (_, _) => initialized += 1,
      requestMore = () => fail("completed stale snapshot requested more chunks"),
      invalidateAndRedownload = () => invalidated += 1,
      warnAlreadyApplied = () => fail("stale snapshot was treated as already applied"),
      reportMissingHeader = _ => fail("stale snapshot attempted block resolution")
    )

    initialized shouldBe 0
    invalidated shouldBe 1
  }

  property("stale snapshot cleanup failure aborts without requesting a replacement") {
    val failure = new IllegalStateException("cleanup failed")
    var requestedFresh = 0
    var aborted = Vector.empty[Throwable]

    ErgoNodeViewSynchronizer.invalidateAndRedownloadUtxoSnapshot(
      invalidate = () => Failure(failure),
      requestFresh = () => requestedFresh += 1,
      abort = cause => aborted :+= cause
    )

    requestedFresh shouldBe 0
    aborted shouldBe Vector(failure)

    ErgoNodeViewSynchronizer.invalidateAndRedownloadUtxoSnapshot(
      invalidate = () => Success(()),
      requestFresh = () => requestedFresh += 1,
      abort = cause => aborted :+= cause
    )
    requestedFresh shouldBe 1
    aborted shouldBe Vector(failure)
  }

  property("recovery routes a completed plan through anchor revalidation") {
    val plan = UtxoSetSnapshotDownloadPlan(
      createdTime = 1L,
      latestUpdateTime = 1L,
      snapshotHeight = 102,
      utxoSetRootHash = Digest32 @@ Array.fill(32)(10: Byte),
      utxoSetTreeHeight = 3,
      expectedChunkIds = IndexedSeq.empty,
      downloadedChunkIds = IndexedSeq.empty,
      downloadingChunks = 0,
      peersToDownload = Seq.empty
    )
    var continued = 0

    ErgoNodeViewSynchronizer.recoverUtxoSnapshotDownload(
      Some(plan),
      snapshotAlreadyApplied = false,
      continue = () => continued += 1
    )

    continued shouldBe 1

    ErgoNodeViewSynchronizer.recoverUtxoSnapshotDownload(
      Some(plan),
      snapshotAlreadyApplied = true,
      continue = () => continued += 1
    )
    ErgoNodeViewSynchronizer.recoverUtxoSnapshotDownload(
      None,
      snapshotAlreadyApplied = false,
      continue = () => continued += 1
    )
    continued shouldBe 1
  }

}
