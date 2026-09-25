package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.SnapshotsInfoTypeId
import org.ergoplatform.modifiers.history.HeaderChain
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions}
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.nodeView.history.ErgoHistoryUtils.GenesisHeight
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.SortingOption
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings._
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.HistoryTestHelpers.BlocksInChain
import org.ergoplatform.utils.generators.ChainGenerator._
import org.ergoplatform.wallet.utils.FileUtils

import scala.concurrent.duration._

/**
  * Regression: UTXO set snapshot bootstrap without NiPoPoW must still detect headers-chain
  * synchronization through the normal header append path.
  *
  * With `utxoBootstrap = true` and `nipopowBootstrap = false` there is no NiPoPoW proof
  * application to mark the headers chain as synced (FullBlockPruningProcessor.setHeadersChainSynced),
  * and the startup recovery in ErgoHistory.readOrGenerate requires nipopowBootstrap, so it cannot
  * help either. The transition used to happen in ToDownloadProcessor.toDownload when a fresh header
  * arrived (`updateBestFullBlock`), but the "do not download block sections until the snapshot is
  * applied" early return shadowed that branch: `isHeadersChainSynced` stayed false forever,
  * `nextModifiersToDownload` kept returning an empty map and the UTXO set snapshot was never
  * requested — bootstrap deadlocked.
  *
  * The property delivers headers through the normal append path until a fresh tip is reached and
  * asserts that snapshot information is requested while block sections remain suppressed.
  * It intentionally never calls setHeadersChainSynced() itself. Fails before the fix.
  */
class UtxoBootstrapNoNipopowSyncSpecification extends ErgoCorePropertyTest with FileUtils {

  import org.ergoplatform.utils.ErgoNodeTestConstants._

  private def utxoBootstrapNoNipopowSettings(dir: java.io.File, blocksToKeep: Int = -1): ErgoSettings = {
    val txCostLimit = initSettings.nodeSettings.maxTransactionCost
    val txSizeLimit = initSettings.nodeSettings.maxTransactionSize
    val nodeSettings = NodeConfigurationSettings(
      StateType.Utxo,
      verifyTransactions = true,
      blocksToKeep = blocksToKeep,
      UtxoSettings(utxoBootstrap = true, 0, 2),
      NipopowSettings(nipopowBootstrap = false, 1),
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

  /**
    * Delivers a stale header chain (old timestamps) followed by one fresh tip header through
    * the normal append path, mimicking the real network where headers arrive in order and only
    * the tip is fresh. The stale part alone must not trigger the headers-synced transition;
    * the fresh tip must.
    */
  private def applyStaleChainWithFreshTip(history: ErgoHistory): ErgoHistory = {
    val staleHeaders = genHeaderChain(BlocksInChain, history, diffBitsOpt = None, useRealTs = false)
    val histAfterStale = applyHeaderChain(history, staleHeaders)
    histAfterStale.isHeadersChainSynced shouldBe false

    val freshTip = nextHeader(
      prev = Some(staleHeaders.last),
      control = histAfterStale.difficultyCalculator,
      tsOpt = Some(System.currentTimeMillis()),
      diffBitsOpt = None,
      useRealTs = false
    )
    applyHeaderChain(histAfterStale, HeaderChain(Seq(freshTip)))
  }

  property("utxo bootstrap without nipopow detects headers sync and requests snapshot info") {
    val dir = createTempDir
    val historySettings = utxoBootstrapNoNipopowSettings(dir)

    val history = ErgoHistory.readOrGenerate(historySettings)(null)
    val updHistory = applyStaleChainWithFreshTip(history)

    updHistory.bestFullBlockOpt shouldBe None
    updHistory.isUtxoSnapshotApplied shouldBe false

    // transition must happen via toDownload on a fresh header (ordinary headers-sync
    // detection), not via setHeadersChainSynced (which this test never calls)
    updHistory.isHeadersChainSynced shouldBe true

    // the synchronizer would now request UTXO set snapshot information ...
    val toDownload = updHistory.nextModifiersToDownload(10, (_, _) => true)
    toDownload.keySet should contain(SnapshotsInfoTypeId.value)

    // ... while full block sections stay suppressed until the snapshot is applied
    toDownload.keySet should not contain BlockTransactions.modifierTypeId
    toDownload.keySet should not contain ADProofs.modifierTypeId
    toDownload.keySet should not contain Extension.modifierTypeId
  }

  property("headers sync transition must not poison isUtxoSnapshotApplied for pruned configs (blocksToKeep >= 0)") {
    val dir = createTempDir
    // blocksToKeep = 2 makes the assertion discriminate: the wrong fix (updateBestFullBlock,
    // fired by the fresh tip at height BlocksInChain + 1) would persist
    // BlocksInChain + 1 - blocksToKeep + 1 = 10 > GenesisHeight here, flipping
    // isUtxoSnapshotApplied with no snapshot applied (with blocksToKeep >= BlocksInChain + 1
    // the computed height collapses to GenesisHeight and the assertion would pass either way)
    val historySettings = utxoBootstrapNoNipopowSettings(dir, blocksToKeep = 2)

    val history = ErgoHistory.readOrGenerate(historySettings)(null)
    history.minimalFullBlockHeight shouldBe GenesisHeight // nothing persisted yet

    // a stale chain followed by a fresh tip drives the ordinary headers-synced transition
    // via the append path
    val updHistory = applyStaleChainWithFreshTip(history)
    updHistory.isHeadersChainSynced shouldBe true

    // ...but the transition must not persist minimalFullBlockHeight: isUtxoSnapshotApplied is
    // derived from it (readMinimalFullBlockHeight() > GenesisHeight), so writing e.g.
    // header.height - blocksToKeep + 1 here (as updateBestFullBlock would) would flip it to
    // true with no snapshot applied and the snapshot would be skipped forever
    updHistory.minimalFullBlockHeight shouldBe GenesisHeight
    updHistory.isUtxoSnapshotApplied shouldBe false
    updHistory.bestFullBlockOpt shouldBe None

    // snapshot bootstrap is still requested, block sections still suppressed
    val toDownload = updHistory.nextModifiersToDownload(10, (_, _) => true)
    toDownload.keySet should contain(SnapshotsInfoTypeId.value)
    toDownload.keySet should not contain BlockTransactions.modifierTypeId
    toDownload.keySet should not contain ADProofs.modifierTypeId
    toDownload.keySet should not contain Extension.modifierTypeId
  }

}
