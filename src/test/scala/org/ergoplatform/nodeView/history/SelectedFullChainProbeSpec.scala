package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.history.HeaderChain
import org.ergoplatform.nodeView.history.ErgoHistoryReader._
import org.ergoplatform.nodeView.history.storage.modifierprocessors.FullBlockProcessor
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.ErgoCorePropertyTest

class SelectedFullChainProbeSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.HistoryTestHelpers._
  import org.ergoplatform.utils.generators.ChainGenerator._

  private def resolve(history: ErgoHistory,
                      targetId: scorex.util.ModifierId,
                      height: Int,
                      maxHeaders: Int): FullChainProbe = {
    var result = history.selectedFullChainProbe(targetId, height, maxHeaders = maxHeaders)
    while (result.isInstanceOf[FullChainPending]) {
      result = history.selectedFullChainProbe(
        targetId, height, Some(result.asInstanceOf[FullChainPending].cursor), maxHeaders
      )
    }
    result
  }

  property("selected full chain resolves an unmarked prefix after first full block") {
    var history = generateHistory(
      verifyTransactions = true, StateType.Digest, PoPoWBootstrap = false, BlocksToKeep
    )
    history.writeMinimalFullBlockHeight(5)
    history.isHeadersChainSyncedVar = true
    val chain = genChain(6)
    history = applyHeaderChain(history, HeaderChain(chain.map(_.header)))

    history.selectedFullChainProbe(chain.head.id, chain.head.height) shouldBe FullChainUnknown
    history = applyBlock(history, chain(4))
    history.bestFullBlockIdOpt shouldBe Some(chain(4).id)

    // The first full block marks only its own ID; earlier selected headers have no marker.
    history.historyStorage.getIndex(FullBlockProcessor.chainStatusKey(chain.head.id)) shouldBe None
    history.selectedFullChainProbe(chain.head.id, chain.head.height, maxHeaders = 2)
      .isInstanceOf[FullChainPending] shouldBe true
    resolve(history, chain.head.id, chain.head.height, 2) shouldBe
      FullChainSelected(chain(4).id)
    resolve(history, chain(1).id, chain.head.height, 2) shouldBe
      FullChainOther(chain(4).id)
  }

  property("selected full-chain cursor restarts after full-tip switch") {
    var history = generateHistory(
      verifyTransactions = true, StateType.Digest, PoPoWBootstrap = false, BlocksToKeep
    )
    history.writeMinimalFullBlockHeight(1)
    history.isHeadersChainSyncedVar = true
    val firstChain = genChain(6, history)
    history = applyChain(history, firstChain)
    val cursor = history.selectedFullChainProbe(
      firstChain.head.id, firstChain.head.height, maxHeaders = 2
    ).asInstanceOf[FullChainPending].cursor

    val otherChain = genChain(8, firstChain(2)).tail
    history = applyChain(history, otherChain)
    history.bestFullBlockIdOpt shouldBe Some(otherChain.last.id)
    val resumed = history.selectedFullChainProbe(
      firstChain.head.id, firstChain.head.height, Some(cursor), maxHeaders = 2
    ).asInstanceOf[FullChainPending]
    resumed.cursor.fullTipId shouldBe otherChain.last.id
    resolve(history, firstChain.head.id, firstChain.head.height, 2) shouldBe
      FullChainSelected(otherChain.last.id)
    resolve(history, firstChain.last.id, firstChain.last.height, 2) shouldBe
      FullChainOther(otherChain.last.id)
  }
}
