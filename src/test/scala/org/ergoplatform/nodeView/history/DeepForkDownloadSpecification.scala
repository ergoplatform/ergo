package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.history.HeaderChain
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.HistoryTestHelpers.generateHistory
import org.ergoplatform.utils.generators.ChainGenerator.{applyChain, applyHeaderChain, genChain}

/** Regression scenarios reported by cafebedouin in #2575. */
class DeepForkDownloadSpecification extends ErgoCorePropertyTest {

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
