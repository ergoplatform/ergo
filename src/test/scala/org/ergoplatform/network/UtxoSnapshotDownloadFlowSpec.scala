package org.ergoplatform.network

import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.UtxoSnapshotStateRestorationFailed
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

  property("changed best-header root invalidates a partial plan before requesting more chunks") {
    val height = 104
    val blockId = ModifierId @@ Algos.encode(Array.fill(32)(14: Byte))
    val pendingChunk = Digest32 @@ Array.fill(32)(15: Byte)
    val plan: UtxoSetSnapshotDownloadPlan = UtxoSetSnapshotDownloadPlan(
      createdTime = 1L,
      latestUpdateTime = 1L,
      snapshotHeight = height,
      utxoSetRootHash = Digest32 @@ Array.fill(32)(16: Byte),
      utxoSetTreeHeight = 2,
      expectedChunkIds = IndexedSeq(pendingChunk),
      downloadedChunkIds = IndexedSeq(false),
      downloadingChunks = 1,
      peersToDownload = Seq.empty
    )
    val changedStateRoot = VersionedLDBAVLStorage.digest(
      Digest32 @@ Array.fill(32)(17: Byte),
      plan.utxoSetTreeHeight
    )
    var initialized = 0
    var requestedMore = 0
    var invalidated = 0

    ErgoNodeViewSynchronizer.continueUtxoSnapshotDownload(
      plan,
      snapshotAlreadyApplied = false,
      snapshotMatchesCurrentHeader =
        ErgoNodeViewSynchronizer.snapshotMatchesCurrentHeader(plan, Some(changedStateRoot)),
      blockIdAtHeight = _ => Some(blockId),
      initialize = (_, _) => initialized += 1,
      requestMore = () => requestedMore += 1,
      invalidateAndRedownload = () => invalidated += 1,
      warnAlreadyApplied = () => fail("partial stale snapshot was treated as already applied"),
      reportMissingHeader = _ => fail("partial stale snapshot attempted block resolution")
    )

    initialized shouldBe 0
    requestedMore shouldBe 0
    invalidated shouldBe 1
  }

  property("an applied snapshot wins over an incomplete download plan") {
    val blockId = ModifierId @@ Algos.encode(Array.fill(32)(11: Byte))
    val pendingChunk = Digest32 @@ Array.fill(32)(12: Byte)
    val plan: UtxoSetSnapshotDownloadPlan = UtxoSetSnapshotDownloadPlan(
      createdTime = 1L,
      latestUpdateTime = 1L,
      snapshotHeight = 103,
      utxoSetRootHash = Digest32 @@ Array.fill(32)(13: Byte),
      utxoSetTreeHeight = 3,
      expectedChunkIds = IndexedSeq(pendingChunk),
      downloadedChunkIds = IndexedSeq(false),
      downloadingChunks = 1,
      peersToDownload = Seq.empty
    )
    var requestedMore = 0
    var warnedAlreadyApplied = 0

    ErgoNodeViewSynchronizer.continueUtxoSnapshotDownload(
      plan,
      snapshotAlreadyApplied = true,
      snapshotMatchesCurrentHeader = true,
      blockIdAtHeight = _ => Some(blockId),
      initialize = (_, _) => fail("an applied snapshot was initialized again"),
      requestMore = () => requestedMore += 1,
      invalidateAndRedownload = () => fail("an applied snapshot was invalidated"),
      warnAlreadyApplied = () => warnedAlreadyApplied += 1,
      reportMissingHeader = _ => fail("an applied snapshot attempted header resolution")
    )

    requestedMore shouldBe 0
    warnedAlreadyApplied shouldBe 1
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

  property("a matching snapshot restoration failure clears the plan request state and does not reinitialize it") {
    val height = 105
    val blockId = ModifierId @@ Algos.encode(Array.fill(32)(18: Byte))
    val snapshotId = Digest32 @@ Array.fill(32)(19: Byte)
    val firstChunk = Digest32 @@ Array.fill(32)(20: Byte)
    val secondChunk = Digest32 @@ Array.fill(32)(21: Byte)
    val plan: UtxoSetSnapshotDownloadPlan = UtxoSetSnapshotDownloadPlan(
      createdTime = 42L,
      latestUpdateTime = 43L,
      snapshotHeight = height,
      utxoSetRootHash = snapshotId,
      utxoSetTreeHeight = 3,
      expectedChunkIds = IndexedSeq(firstChunk, secondChunk),
      downloadedChunkIds = IndexedSeq(true, true),
      downloadingChunks = 0,
      peersToDownload = Seq.empty
    )
    val failure = UtxoSnapshotStateRestorationFailed(
      height,
      blockId,
      snapshotId,
      plan.createdTime,
      new IllegalArgumentException("invalid internal routing key")
    )
    val expectedChunkIds = plan.expectedChunkIds.map(id => ModifierId @@ Algos.encode(id))
    var currentPlan = Option(plan)
    var invalidations = 0
    var cancelledRetries = Vector.empty[ModifierId]
    var clearedRequests = Vector.empty[ModifierId]
    var freshRequests = 0
    var aborts = Vector.empty[Throwable]

    val handled = ErgoNodeViewSynchronizer.handleUtxoSnapshotStateRestorationFailure(
      failure,
      currentPlan,
      currentBlockId = Some(blockId),
      invalidate = () => {
        invalidations += 1
        currentPlan = None
        Success(())
      },
      cancelLocalRetries = ids => cancelledRetries ++= ids,
      clearRequested = id => clearedRequests :+= id,
      requestFresh = () => freshRequests += 1,
      abort = cause => aborts :+= cause
    )

    handled shouldBe true
    invalidations shouldBe 1
    cancelledRetries shouldBe expectedChunkIds
    clearedRequests shouldBe expectedChunkIds
    freshRequests shouldBe 1
    aborts shouldBe empty
    currentPlan shouldBe None

    var repeatedInitializations = 0
    ErgoNodeViewSynchronizer.recoverUtxoSnapshotDownload(
      currentPlan,
      snapshotAlreadyApplied = false,
      continue = () => repeatedInitializations += 1
    )
    repeatedInitializations shouldBe 0
  }

  property("a restoration failure with any stale plan identity is side-effect free") {
    val height = 106
    val blockId = ModifierId @@ Algos.encode(Array.fill(32)(22: Byte))
    val snapshotId = Digest32 @@ Array.fill(32)(23: Byte)
    val chunkId = Digest32 @@ Array.fill(32)(24: Byte)
    val otherBlockId = ModifierId @@ Algos.encode(Array.fill(32)(25: Byte))
    val otherSnapshotId = Digest32 @@ Array.fill(32)(26: Byte)
    val plan: UtxoSetSnapshotDownloadPlan = UtxoSetSnapshotDownloadPlan(
      createdTime = 52L,
      latestUpdateTime = 53L,
      snapshotHeight = height,
      utxoSetRootHash = snapshotId,
      utxoSetTreeHeight = 3,
      expectedChunkIds = IndexedSeq(chunkId),
      downloadedChunkIds = IndexedSeq(true),
      downloadingChunks = 0,
      peersToDownload = Seq.empty
    )
    val matchingFailure = UtxoSnapshotStateRestorationFailed(
      height,
      blockId,
      snapshotId,
      planCreatedTime = plan.createdTime,
      new IllegalArgumentException("snapshot restoration failed")
    )
    val cases: Seq[(String, UtxoSnapshotStateRestorationFailed,
      Option[UtxoSetSnapshotDownloadPlan], Option[ModifierId])] = Seq(
      ("created time", matchingFailure.copy(planCreatedTime = plan.createdTime - 1), Some(plan), Some(blockId)),
      ("snapshot root", matchingFailure.copy(snapshotId = otherSnapshotId), Some(plan), Some(blockId)),
      ("snapshot height", matchingFailure.copy(blockHeight = height + 1), Some(plan), Some(blockId)),
      ("canonical block", matchingFailure, Some(plan), Some(otherBlockId)),
      ("missing canonical block", matchingFailure, Some(plan), None),
      ("download completeness", matchingFailure,
        Some(plan.copy(downloadedChunkIds = IndexedSeq(false))), Some(blockId)),
      ("missing plan", matchingFailure, None, Some(blockId))
    )

    cases.foreach { case (label, failure, currentPlan, currentBlockId) =>
      var effects = Vector.empty[String]
      val handled = ErgoNodeViewSynchronizer.handleUtxoSnapshotStateRestorationFailure(
        failure,
        currentPlan,
        currentBlockId,
        invalidate = () => {
          effects :+= "invalidate"
          Success(())
        },
        cancelLocalRetries = _ => effects :+= "cancel local retries",
        clearRequested = _ => effects :+= "clear requested",
        requestFresh = () => effects :+= "request fresh",
        abort = _ => effects :+= "abort"
      )

      withClue(label) {
        handled shouldBe false
        effects shouldBe empty
      }
    }
  }

}
