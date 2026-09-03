package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.modifiers.SnapshotsInfoTypeId
import org.ergoplatform.modifiers.history.HeaderChain
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.serialization.ManifestSerializer
import org.ergoplatform.utils.ErgoCorePropertyTest

/**
  * Tests for UTXO set snapshot bootstrap behavior in `ToDownloadProcessor` /
  * `FullBlockPruningProcessor`:
  * - `setHeadersChainSynced` makes `nextModifiersToDownload` issue a UTXO set snapshot request
  *   (instead of full blocks) when no full blocks are applied yet
  * - `toDownload` returns no block sections until a UTXO set snapshot is applied
  * - without `utxoBootstrap` enabled, none of the above holds
  */
class UtxoBootstrapToDownloadSpecification extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.HistoryTestHelpers._
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ChainGenerator._
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._
  import org.ergoplatform.utils.generators.ValidBlocksGenerators._

  private def genUtxoBootstrapHistory() =
    generateHistory(verifyTransactions = true,
                    StateType.Utxo,
                    PoPoWBootstrap = false,
                    BlocksToKeep,
                    utxoBootstrap = true)

  private def headersWithFreshTail(history: ErgoHistory, extra: Int = 1) = {
    val chain = genChain(BlocksInChain + extra, history)
    val headers = HeaderChain(chain.dropRight(extra).map(_.header))
    val updHistory = applyHeaderChain(history, headers)
    (updHistory, chain)
  }

  property("snapshot request is issued when headers chain synced and no full blocks applied") {
    var history = genUtxoBootstrapHistory()
    val chain = genChain(BlocksInChain, history)
    history = applyHeaderChain(history, HeaderChain(chain.map(_.header)))

    history.bestFullBlockOpt shouldBe None
    history.isHeadersChainSynced shouldBe false

    // nothing to download before headers chain is considered synced
    history.nextModifiersToDownload(1, (_, id) => !history.contains(id)) shouldBe
      Map.empty

    history.setHeadersChainSynced()
    history.isHeadersChainSynced shouldBe true

    // no full blocks applied, no snapshot plan yet => ask peers for UTXO set snapshots
    history.nextModifiersToDownload(1, (_, id) => !history.contains(id)) shouldBe
      Map(SnapshotsInfoTypeId.value -> Seq.empty)

    // setter must be idempotent
    history.setHeadersChainSynced()
    history.isHeadersChainSynced shouldBe true
  }

  property("no repeated snapshot request once download plan is registered") {
    var history = genUtxoBootstrapHistory()
    val (updHistory, chain) = headersWithFreshTail(history)
    history = updHistory
    history.setHeadersChainSynced()
    val freshHeader = chain.last.header

    // manifest of some UTXO set snapshot, needed to create a download plan
    val bh = boxesHolderGenOfSize(1024).sample.get
    val us = createUtxoState(bh, parameters)
    val snapshotHeight = freshHeader.height
    us.dumpSnapshot(snapshotHeight, us.rootDigest.dropRight(1))
    val manifestId = us.snapshotsDb.readSnapshotsInfo.availableManifests(snapshotHeight)
    val manifestBytes = us.snapshotsDb.readManifestBytes(manifestId).get
    val manifest = ManifestSerializer.defaultSerializer.parseBytes(manifestBytes)

    history.registerManifestToDownload(manifest, snapshotHeight, Seq.empty)
    history.utxoSetSnapshotDownloadPlan() should not be empty
    history.isUtxoSnapshotApplied shouldBe false

    // download plan exists, so no new snapshot info request
    history.nextModifiersToDownload(1, (_, id) => !history.contains(id)) shouldBe
      Map.empty
  }

  property("toDownload returns no block sections before snapshot, and sections after snapshot") {
    var history = genUtxoBootstrapHistory()
    val (updHistory, chain) = headersWithFreshTail(history, extra = 2)
    history = updHistory
    history.setHeadersChainSynced()
    val freshHeader = chain(chain.length - 2).header
    val nextHeader = chain.last.header

    // headers chain is synced and the header is not too far back, still no block sections
    // must be downloaded before the UTXO set snapshot is applied
    val piBefore = history.append(freshHeader).get._2
    piBefore.toDownload shouldBe Seq.empty

    // apply snapshot at freshHeader's height, so that full blocks downloading
    // starts from nextHeader
    history.onUtxoSnapshotApplied(freshHeader.height)
    history.isUtxoSnapshotApplied shouldBe true

    val piAfter = history.append(nextHeader).get._2
    piAfter.toDownload shouldBe history.requiredModifiersForHeader(nextHeader)
    piAfter.toDownload should not be empty
  }

  property("without utxoBootstrap no snapshot request and block sections downloaded as usual") {
    var history =
      generateHistory(verifyTransactions = true,
                      StateType.Utxo,
                      PoPoWBootstrap = false,
                      BlocksToKeep)
    val (updHistory, chain) = headersWithFreshTail(history)
    history = updHistory
    history.setHeadersChainSynced()
    val freshHeader = chain.last.header

    // full block sections are requested right away, no snapshot request is involved
    val pi = history.append(freshHeader).get._2
    pi.toDownload shouldBe history.requiredModifiersForHeader(freshHeader)
    pi.toDownload should not be empty

    val toDownloadMap =
      history.nextModifiersToDownload(1, (_, id) => !history.contains(id))
    toDownloadMap should not be empty
    toDownloadMap.contains(SnapshotsInfoTypeId.value) shouldBe false
  }

}
