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
  * Reproduction of a restart deadlock during UTXO set snapshot bootstrapping.
  *
  * Scenario: a node bootstrapping via NiPoPoW proofs + UTXO set snapshot (nipopowBootstrap +
  * utxoBootstrap) is restarted after the headers were persisted but before the UTXO set snapshot
  * application completed (e.g. killed in the middle of snapshot chunk downloading). After the
  * restart the node never resumes bootstrapping:
  *
  * - `isHeadersChainSynced` is kept in memory only (FullBlockPruningProcessor.isHeadersChainSyncedVar)
  *   so it is false after every restart, even though the headers persisted before the restart
  *   are still at the network tip
  * - on restart the headers are read from the database, so no new NiPoPoW proof is requested
  *   (ErgoNodeViewSynchronizer.sendSync asks for proofs only when `bestHeaderOpt` is empty)
  * - the synchronizer asks for the UTXO set snapshot / block sections only when
  *   `isHeadersChainSynced` is true (ErgoNodeViewSynchronizer.requestMoreModifiers), and
  *   ToDownloadProcessor filters block-section invs out while the snapshot is not applied
  *
  * Result: headers are at tip, the state is at genesis, and the node loops sending sync
  * messages forever. Observed on mainnet with a node restarted mid snapshot download.
  *
  * The startup recovery in ErgoHistory.readOrGenerate must mark the headers chain as synced when
  * the persisted best header is fresh and no snapshot was applied. It must not require
  * nipopowBootstrap (persisted headers with no snapshot applied mean bootstrap was already
  * underway in both modes), and it must not fire for a stale or partial header chain - such a
  * node transitions organically when the next fresh header arrives (ToDownloadProcessor).
  *
  * The property below simulates the restart by closing the history storage and reopening the
  * database, and asserts that the headers chain is considered synced after the restart, so that
  * snapshot bootstrapping can resume. It fails before the fix (for both bootstrap modes).
  */
class UtxoBootstrapRestartSpecification extends ErgoCorePropertyTest with FileUtils {

  import org.ergoplatform.utils.ErgoNodeTestConstants._

  private def utxoBootstrapSettings(dir: java.io.File, nipopowBootstrap: Boolean): ErgoSettings = {
    val txCostLimit = initSettings.nodeSettings.maxTransactionCost
    val txSizeLimit = initSettings.nodeSettings.maxTransactionSize
    val nodeSettings = NodeConfigurationSettings(
      StateType.Utxo,
      verifyTransactions = true,
      blocksToKeep = -1,
      UtxoSettings(utxoBootstrap = true, 0, 2),
      NipopowSettings(nipopowBootstrap, 1),
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
      null, null, settings.cacheSettings, matrix = settings.matrix)
  }

  property("node restarted after nipopow headers persisted but before snapshot application resumes bootstrap") {
    restartsResumeBootstrap(nipopowBootstrap = true)
  }

  property("node restarted after headers persisted (no nipopow) but before snapshot application resumes bootstrap") {
    restartsResumeBootstrap(nipopowBootstrap = false)
  }

  private def restartsResumeBootstrap(nipopowBootstrap: Boolean): Unit = {
    val dir = createTempDir
    val historySettings = utxoBootstrapSettings(dir, nipopowBootstrap)

    // headers-only chain, result of header sync / a NiPoPoW proof application (no full blocks yet).
    // Real (fresh) timestamps: this is the state of a node killed mid bootstrap - headers at tip.
    val history = ErgoHistory.readOrGenerate(historySettings)(null)
    val headers = genHeaderChain(BlocksInChain, history, diffBitsOpt = None, useRealTs = true)
    val updHistory = applyHeaderChain(history, headers)

    updHistory.bestFullBlockOpt shouldBe None
    updHistory.isUtxoSnapshotApplied shouldBe false
    // fresh headers drove the organic headers-synced transition via the append path
    updHistory.isHeadersChainSynced shouldBe true

    // capture the height before closing: reads after close are not guaranteed
    // (a cache miss would reach LevelDB on a closed store)
    val heightBeforeRestart = updHistory.headersHeight

    // simulate node restart: close the storage and reopen the same database.
    // Without the close, StoreRegistry hands back the already-open underlying DB and the
    // "restart" is not a real cold reopen.
    updHistory.closeStorage()
    val restarted = ErgoHistory.readOrGenerate(historySettings)(null)

    restarted.headersHeight shouldBe heightBeforeRestart
    restarted.bestFullBlockOpt shouldBe None
    restarted.isUtxoSnapshotApplied shouldBe false

    // without this the synchronizer never asks for the snapshot manifest / blocks
    // (ErgoNodeViewSynchronizer.requestMoreModifiers -> sendSync loop) and bootstrap
    // can never resume
    restarted.isHeadersChainSynced shouldBe true

    // the synchronizer's next download request would be UTXO set snapshot information
    restarted.nextModifiersToDownload(10, (_, _) => true).keySet should contain(SnapshotsInfoTypeId.value)
  }

}
