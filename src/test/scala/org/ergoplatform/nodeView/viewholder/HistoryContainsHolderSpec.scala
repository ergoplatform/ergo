package org.ergoplatform.nodeView.viewholder

import akka.testkit.TestProbe
import org.ergoplatform.core.idToVersion
import org.ergoplatform.modifiers.history.{BlockTransactions, HistoryModifierSerializer}
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import org.ergoplatform.nodeView.LocallyGeneratedModifier
import org.ergoplatform.nodeView.state.{ErgoState, StateType}
import org.ergoplatform.utils.{ErgoCorePropertyTest, NodeViewTestConfig, NodeViewTestOps}
import org.ergoplatform.utils.fixtures.NodeViewFixture
import org.ergoplatform.utils.generators.ValidBlocksGenerators._

class HistoryContainsHolderSpec extends ErgoCorePropertyTest with NodeViewTestOps {
  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters

  property("foreign object under a section id does not block the genuine section in Holder") {
    val settings = NodeViewTestConfig(
      StateType.Utxo,
      verifyTransactions = true,
      popowBootstrap = false
    ).toSettings
    val (generationState, boxes) = createUtxoState(settings)
    try {
      val block = validFullBlock(None, generationState, boxes)
      val genuine = block.blockTransactions
      val foreign = genuine.copy(headerId = Header.GenesisParentId)
      foreign.id == genuine.id shouldBe false
      val foreignBytes = HistoryModifierSerializer.toBytes(foreign)
      HistoryModifierSerializer.parseBytesTry(foreignBytes).get.id shouldBe foreign.id

      Seq(false, true).foreach { corrupt =>
        new NodeViewFixture(settings, parameters).apply { fixture =>
          import fixture._
          applyHeader(block.header).isSuccess shouldBe true
          val history = getHistory

          if (corrupt) {
            history.dumpToDb(genuine.serializedId, foreignBytes).isSuccess shouldBe true
            history.typedModifierById[BlockTransactions](genuine.id) shouldBe None
          }

          val barrier = TestProbe()(actorSystem)
          barrier.send(nodeViewHolderRef, LocallyGeneratedModifier(genuine))
          barrier.send(nodeViewHolderRef, LocallyGeneratedModifier(block.extension))
          barrier.send(nodeViewHolderRef,
            GetDataFromCurrentView[ErgoState[_], CurView](view => view))
          val current = barrier.expectMsgType[CurView]

          current.history.bestHeaderIdOpt shouldBe Some(block.id)
          current.history.typedModifierById[Extension](block.extension.id) shouldBe Some(block.extension)
          current.history.typedModifierById[BlockTransactions](genuine.id) shouldBe Some(genuine)
          current.history.getFullBlock(block.header).map(_.id) shouldBe Some(block.id)
          current.history.bestFullBlockIdOpt shouldBe Some(block.id)
          current.state.version shouldBe idToVersion(block.id)
        }
      }
    } finally {
      generationState.closeStorage()
    }
  }
}
