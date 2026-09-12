package org.ergoplatform.nodeView.history.storage.modifierprocessors

import java.io.File

import org.ergoplatform.modifiers.SnapshotsInfoTypeId
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.state.{StateType, UtxoState}
import org.ergoplatform.serialization.ManifestSerializer
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.wallet.utils.FileUtils
import scorex.db.LDBFactory

/** Ordinary header append and snapshot-download planning contracts, using test history stores. */
class UtxoBootstrapToDownloadSpecification extends ErgoCorePropertyTest with FileUtils {
  import org.ergoplatform.utils.HistoryTestHelpers._
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ChainGenerator._
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._

  private def withHistory(keep: Int = BlocksToKeep, verify: Boolean = true, bootstrap: Boolean = true)
                         (test: ErgoHistory => Unit): Unit = {
    val history = generateHistory(verify, StateType.Utxo, PoPoWBootstrap = false, keep, utxoBootstrap = bootstrap)
    try test(history) finally history.closeStorage()
  }

  /** Simulate the persisted download boundary after snapshot finalization.
    * Canonical snapshot finalization is covered by the dedicated processor and
    * node-view-holder specifications; this suite exercises download selection.
    */
  private def simulatePersistedPostSnapshotDownloadBoundary(history: ErgoHistory,
                                                              snapshotHeight: Int): Unit =
    history.writeMinimalFullBlockHeight(snapshotHeight + 1)

  private def staleHeaders(history: ErgoHistory, count: Int = BlocksInChain + 2): ErgoHistory =
    applyHeaderChain(history, genHeaderChain(count, history, diffBitsOpt = None, useRealTs = false))

  private def freshHeader(history: ErgoHistory) =
    nextHeader(history.bestHeaderOpt, history.difficultyCalculator,
      tsOpt = Some(math.max(System.currentTimeMillis(), history.bestHeaderOpt.get.timestamp + 1)), useRealTs = true)

  for (keep <- Seq(-1, 3)) {
    property(s"ordinary fresh header starts snapshot discovery without moving retention floor, keep=$keep") {
      withHistory(keep) { initial =>
        val history = staleHeaders(initial)
        val floor = history.minFullBlockAvailable
        history.isHeadersChainSynced shouldBe false
        history.isUtxoSnapshotApplied shouldBe false
        history.bestFullBlockOpt shouldBe None
        history.nextModifiersToDownload(1, (_, _) => true) shouldBe Map.empty

        val header = freshHeader(history)
        if (keep > 0) header.height should be > keep
        val (updated, progress) = history.append(header).get
        progress.toDownload shouldBe Seq.empty
        updated.isHeadersChainSynced shouldBe true
        updated.minFullBlockAvailable shouldBe floor
        updated.bestFullBlockOpt shouldBe None
        updated.isUtxoSnapshotApplied shouldBe false
        updated.nextModifiersToDownload(1, (_, id) => !updated.contains(id)) shouldBe
          Map(SnapshotsInfoTypeId.value -> Seq.empty)
      }
    }
  }

  property("old headers and disabled transaction verification do not start snapshot discovery") {
    withHistory() { initial =>
      val history = staleHeaders(initial)
      val oldNext = genHeaderChain(1, history, diffBitsOpt = None, useRealTs = false).headers.last
      history.append(oldNext).get._2.toDownload shouldBe Seq.empty
      history.isHeadersChainSynced shouldBe false
      history.nextModifiersToDownload(1, (_, _) => true) shouldBe Map.empty
    }
    withHistory(verify = false) { initial =>
      val history = staleHeaders(initial)
      val floor = history.minFullBlockAvailable
      history.append(freshHeader(history)).get._2.toDownload shouldBe Seq.empty
      history.isHeadersChainSynced shouldBe false
      history.minFullBlockAvailable shouldBe floor
      history.isUtxoSnapshotApplied shouldBe false
      history.nextModifiersToDownload(1, (_, _) => true) shouldBe Map.empty
    }
  }

  property("explicit headers-synced setter remains idempotent and requests snapshot information") {
    withHistory() { initial =>
      val history = staleHeaders(initial)
      val floor = history.minFullBlockAvailable
      history.isHeadersChainSynced shouldBe false
      history.setHeadersChainSynced()
      history.setHeadersChainSynced()
      history.isHeadersChainSynced shouldBe true
      history.minFullBlockAvailable shouldBe floor
      history.nextModifiersToDownload(1, (_, _) => true) shouldBe
        Map(SnapshotsInfoTypeId.value -> Seq.empty)
    }
  }

  property("no repeated snapshot request once download plan is registered") {
    withHistory() { initial =>
      val history = staleHeaders(initial)
      val header = freshHeader(history)
      history.append(header).get
      val directory = createTempDir
      val stateSettings = org.ergoplatform.utils.ErgoNodeTestConstants.settings.copy(directory = directory.getAbsolutePath)
      val boxes = boxesHolderGenOfSize(1024).sample.get
      val stateDirectory = new File(directory, "state")
      stateDirectory.mkdir() shouldBe true
      val state = UtxoState.fromBoxHolder(boxes, None, stateDirectory, stateSettings, parameters)
      try {
        state.dumpSnapshot(header.height, state.rootDigest.dropRight(1))
        val manifestId = state.snapshotsDb.readSnapshotsInfo.availableManifests(header.height)
        val manifestBytes = state.snapshotsDb.readManifestBytes(manifestId).get
        val manifest = ManifestSerializer.defaultSerializer.parseBytes(manifestBytes)
        history.registerManifestToDownload(manifest, manifestBytes, header.height, Seq.empty)
        history.utxoSetSnapshotDownloadPlan() should not be empty
        history.isUtxoSnapshotApplied shouldBe false
        history.nextModifiersToDownload(1, (_, id) => !history.contains(id)) shouldBe Map.empty
      } finally {
        try state.closeStorage() finally {
          LDBFactory.createKvDb(new File(directory, "snapshots").getAbsolutePath).close()
          deleteRecursive(directory)
        }
      }
    }
  }

  property("toDownload returns no block sections before snapshot, and sections after snapshot") {
    withHistory() { initial =>
      val history = staleHeaders(initial)
      val first = freshHeader(history)
      history.append(first).get._2.toDownload shouldBe Seq.empty
      history.isHeadersChainSynced shouldBe true
      simulatePersistedPostSnapshotDownloadBoundary(history, first.height)
      history.isUtxoSnapshotApplied shouldBe true
      val next = freshHeader(history)
      val progress = history.append(next).get._2
      progress.toDownload shouldBe history.requiredModifiersForHeader(next)
      progress.toDownload should not be empty
    }
  }

  property("without utxoBootstrap no snapshot request and block sections downloaded as usual") {
    withHistory(bootstrap = false) { initial =>
      val history = staleHeaders(initial)
      history.append(freshHeader(history)).get
      history.isHeadersChainSynced shouldBe true
      val next = freshHeader(history)
      val progress = history.append(next).get._2
      progress.toDownload shouldBe history.requiredModifiersForHeader(next)
      progress.toDownload should not be empty
      val requests = history.nextModifiersToDownload(1, (_, id) => !history.contains(id))
      requests should not be empty
      requests.contains(SnapshotsInfoTypeId.value) shouldBe false
    }
  }
}
