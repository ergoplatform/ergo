package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.SnapshotsInfoTypeId
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.SortingOption
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings._
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.HistoryTestHelpers.BlocksInChain
import org.ergoplatform.utils.generators.ChainGenerator._
import org.ergoplatform.wallet.utils.FileUtils

import scala.concurrent.duration._

/**
  * Close and reopen actual history stores inside one JVM, then inspect download planning.
  * Headers are appended normally; these fixtures do not apply a NiPoPoW proof, restart a process,
  * or establish production-network recovery.
  */
class UtxoBootstrapRestartSpecification extends ErgoCorePropertyTest with FileUtils {

  import org.ergoplatform.utils.ErgoNodeTestConstants._

  private def utxoBootstrapSettings(dir: java.io.File, nipopow: Boolean): ErgoSettings = {
    val txCostLimit = initSettings.nodeSettings.maxTransactionCost
    val txSizeLimit = initSettings.nodeSettings.maxTransactionSize
    val nodeSettings = NodeConfigurationSettings(
      StateType.Utxo,
      verifyTransactions = true,
      blocksToKeep = -1,
      UtxoSettings(utxoBootstrap = true, 0, 2),
      NipopowSettings(nipopowBootstrap = nipopow, 1),
      mining = false,
      txCostLimit,
      txSizeLimit,
      blockCandidateGenerationInterval = 20.seconds,
      useExternalMiner = false,
      internalMinersCount = 1,
      internalMinerPollingInterval = 1.second,
      miningPubKeyHex = None,
      offlineGeneration = false,
      200,
      5.minutes,
      100000,
      1.minute,
      mempoolSorting = SortingOption.FeePerByte,
      rebroadcastCount = 200,
      1000000,
      100,
      adProofsSuffixLength = 112 * 1024,
      extraIndex = false
    )
    ErgoSettings(dir.getAbsolutePath, NetworkType.TestNet, settings.chainSettings, nodeSettings,
      null, null, settings.cacheSettings)
  }

  for (nipopow <- Seq(false, true)) {
    property(s"reopened snapshot-bootstrap history resumes discovery with ordinary fresh-header reentry, nipopow=$nipopow") {
      val dir = createTempDir
      val historySettings = utxoBootstrapSettings(dir, nipopow)
      var first: Option[ErgoHistory] = None
      var reopened: Option[ErgoHistory] = None
      try {
        val history = ErgoHistory.readOrGenerate(historySettings)(null)
        first = Some(history)
        val stored = applyHeaderChain(history,
          genHeaderChain(BlocksInChain, history, diffBitsOpt = None, useRealTs = false))
        val expectedTip = stored.bestHeaderOpt.get.id
        val expectedHeight = stored.headersHeight
        val floor = stored.minFullBlockAvailable
        stored.bestFullBlockOpt shouldBe None
        stored.isUtxoSnapshotApplied shouldBe false
        stored.isHeadersChainSynced shouldBe false

        // Release the first native store before acquiring the second wrapper over the same files.
        stored.closeStorage()
        first = None
        val restarted = ErgoHistory.readOrGenerate(historySettings)(null)
        reopened = Some(restarted)
        restarted.headersHeight shouldBe expectedHeight
        restarted.bestHeaderOpt.get.id shouldBe expectedTip
        restarted.bestFullBlockOpt shouldBe None
        restarted.isUtxoSnapshotApplied shouldBe false
        restarted.minFullBlockAvailable shouldBe floor
        // The existing configured startup predicate is distinct from ordinary header synchronization.
        restarted.isHeadersChainSynced shouldBe nipopow
        restarted.nextModifiersToDownload(1, (_, _) => true) shouldBe
          (if (nipopow) Map(SnapshotsInfoTypeId.value -> Seq.empty) else Map.empty)

        val fresh = nextHeader(restarted.bestHeaderOpt, restarted.difficultyCalculator,
          tsOpt = Some(System.currentTimeMillis()), useRealTs = true)
        val (updated, progress) = restarted.append(fresh).get
        progress.toDownload shouldBe Seq.empty
        updated.isHeadersChainSynced shouldBe true
        updated.isUtxoSnapshotApplied shouldBe false
        updated.bestFullBlockOpt shouldBe None
        updated.minFullBlockAvailable shouldBe floor
        updated.nextModifiersToDownload(1, (_, id) => !updated.contains(id)) shouldBe
          Map(SnapshotsInfoTypeId.value -> Seq.empty)
      } finally {
        try reopened.foreach(_.closeStorage())
        finally {
          first.foreach(_.closeStorage())
          deleteRecursive(dir)
        }
      }
    }
  }
}
