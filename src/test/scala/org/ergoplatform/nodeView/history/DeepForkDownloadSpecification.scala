package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.history.HeaderChain
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoNodeTestConstants.initSettings
import org.ergoplatform.utils.HistoryTestHelpers.generateHistory
import org.ergoplatform.utils.generators.ChainGenerator.{applyChain, applyHeaderChain, genChain}
import org.ergoplatform.wallet.utils.FileUtils

/** Regression scenarios reported by cafebedouin in #2575. */
class DeepForkDownloadSpecification extends ErgoCorePropertyTest with FileUtils {

  Seq(129, 130).foreach { headerLead =>
    property(s"download a one-block orphan after restart with headers $headerLead ahead") {
      val historySettings = initSettings.copy(
        directory = createTempDir.getAbsolutePath,
        chainSettings = initSettings.chainSettings.copy(epochLength = 100000000),
        nodeSettings = initSettings.nodeSettings.copy(
          stateType = StateType.Digest,
          verifyTransactions = true,
          blocksToKeep = -1,
          utxoSettings = initSettings.nodeSettings.utxoSettings.copy(utxoBootstrap = false),
          nipopowSettings = initSettings.nodeSettings.nipopowSettings.copy(nipopowBootstrap = false),
          keepVersions = 200,
          extraIndex = false
        )
      )
      var history = ErgoHistory.readOrGenerate(historySettings)(null)
      try {
        val common = genChain(13, history)
        history = applyChain(history, common)
        val own = genChain(1, common.last).tail
        history = applyChain(history, own)
        val heavier = genChain(headerLead + 1, common.last).tail

        history.bestFullBlockOpt.get.header.id shouldBe own.head.header.id
        history.isHeadersChainSynced shouldBe true
        history.closeStorage()
        history = ErgoHistory.readOrGenerate(historySettings)(null)
        history.bestFullBlockOpt.get.header.id shouldBe own.head.header.id
        history.isHeadersChainSynced shouldBe false

        // The first fresh fork header enables sync, but its append requests no sections.
        val (afterFirstHeader, progress) = history.append(heavier.head.header).get
        history = afterFirstHeader
        history.isHeadersChainSynced shouldBe true
        progress.toDownload shouldBe empty
        history = applyHeaderChain(history, HeaderChain(heavier.tail.map(_.header)))

        history.bestHeaderOpt.get.id shouldBe heavier.last.header.id
        history.bestFullBlockOpt.get.height shouldBe 14
        history.headersHeight - history.bestFullBlockOpt.get.height shouldBe headerLead
        history.isInBestChain(common.last.header.id) shouldBe true
        history.isInBestChain(own.head.header.id) shouldBe false

        val firstForkSections = history.requiredModifiersForHeader(heavier.head.header).map(_._2).toSet
        firstForkSections should not be empty
        firstForkSections.foreach(id => history.contains(id) shouldBe false)
        val requested = history.nextModifiersToDownload(1, (_, id) => !history.contains(id))
          .values.flatten.toSet
        firstForkSections.subsetOf(requested) shouldBe true
      } finally {
        history.closeStorage()
      }
    }
  }

  Seq(301, 381).foreach { headerHeight =>
    property(s"download fork blocks below the old full tip with headers at $headerHeight") {
      var history = generateHistory(verifyTransactions = true, StateType.Digest,
        PoPoWBootstrap = false, blocksToKeep = -1)
      try {
        history.writeMinimalFullBlockHeight(ErgoHistoryUtils.GenesisHeight)
        history.isHeadersChainSyncedVar = true
        val common = genChain(100, history)
        history = applyChain(history, common)
        val own = genChain(151, common.last).tail
        history = applyChain(history, own)
        val heavier = genChain(headerHeight - 100, common.last).tail
        history = applyHeaderChain(history, HeaderChain(heavier.map(_.header)))

        history.bestHeaderOpt.get.id shouldBe heavier.last.header.id
        history.bestFullBlockOpt.get.header.id shouldBe own.last.header.id
        history.bestFullBlockOpt.get.height shouldBe 251
        history.isInBestChain(own.last.header.id) shouldBe false

        val requested = history.nextModifiersToDownload(1000, (_, id) => !history.contains(id))
          .values.flatten.toSet
        val firstForkSections = history.requiredModifiersForHeader(heavier.head.header).map(_._2).toSet
        val requestedHeights = heavier.filter(b => b.blockSections.exists(s => requested.contains(s.id)))
          .map(_.header.height).toVector
        withClue(s"requested fork heights: $requestedHeights; ") {
          firstForkSections.subsetOf(requested) shouldBe true
        }
      } finally {
        history.historyStorage.close()
      }
    }
  }
}
