package org.ergoplatform.nodeView.viewholder

import akka.pattern.ask
import akka.util.Timeout
import org.ergoplatform.consensus.{ModifierSemanticValidity, ProgressInfo}
import org.ergoplatform.core.idToVersion
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock}
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, ResetBlockchainTo}
import org.ergoplatform.nodeView.state.{ErgoState, StateType}
import org.ergoplatform.utils.{ErgoCorePropertyTest, NodeViewTestConfig, NodeViewTestContext, NodeViewTestOps, RandomWrapper}
import org.ergoplatform.utils.fixtures.NodeViewFixture
import org.ergoplatform.utils.generators.ValidBlocksGenerators.{createUtxoState, validFullBlockWithBoxHolder}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
  * The reset scenario requested in the review of #2249 (issue #2232), run through the node view holder:
  * the transactions of a valid block are marked invalid (as after a failed disk read), the node is
  * reset to a block below it, the same blocks are applied again through the holder, and the block
  * is applied to the UTXO state and marked valid by the holder itself (no test-side
  * reportModifierIsValid call).
  */
class ResetRevalidationSpec extends ErgoCorePropertyTest with NodeViewTestOps {
  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters

  property("reset below an invalidated block lets the same blocks be applied again and marked valid") {
    val config = NodeViewTestConfig(StateType.Utxo, verifyTransactions = true, popowBootstrap = false).toSettings
    NodeViewFixture(config, parameters) { fixture =>
      implicit val ctx: NodeViewTestContext = fixture
      implicit val timeout: Timeout = Timeout(30.seconds)
      var (generatedState, boxes) = createUtxoState(fixture.settings)
      var parent: Option[ErgoFullBlock] = None
      var blocks = Vector.empty[ErgoFullBlock]
      try {
        for (height <- 1 to 4) {
          val (block, nextBoxes) =
            validFullBlockWithBoxHolder(parent, generatedState, boxes, new RandomWrapper(Some(height)))
          generatedState = generatedState.applyModifier(block, None)(_ => ()).get
          applyBlock(block).isSuccess shouldBe true
          boxes = nextBoxes
          parent = Some(block)
          blocks :+= block
        }
        getHistory.fullBlockHeight shouldBe 4

        val corrupted = blocks(2) // height 3
        // Mark the transactions of a valid block invalid, inside the holder.
        val invalidate = GetDataFromCurrentView[ErgoState[_], Boolean] { v =>
          v.history.reportModifierIsInvalid(corrupted.blockTransactions,
            ProgressInfo[BlockSection](None, Seq.empty, Seq.empty, Seq.empty)).isSuccess
        }
        Await.result((fixture.nodeViewHolderRef ? invalidate).mapTo[Boolean], 30.seconds) shouldBe true
        getHistory.isSemanticallyValid(corrupted.id) shouldBe ModifierSemanticValidity.Invalid
        getHistory.isSemanticallyValid(blocks(3).id) shouldBe ModifierSemanticValidity.Invalid

        // Reset to height 1, below the invalidated block.
        val reset = Await.result((fixture.nodeViewHolderRef ? ResetBlockchainTo(1)).mapTo[Try[String]], 30.seconds)
        info(s"reset reply: $reset")
        reset.isSuccess shouldBe true
        val afterReset = getCurrentView
        afterReset.history.headersHeight shouldBe 1
        afterReset.history.fullBlockHeight shouldBe 1
        afterReset.state.version shouldBe idToVersion(blocks(0).id)
        blocks.drop(1).foreach { b =>
          afterReset.history.isSemanticallyValid(b.id) shouldBe ModifierSemanticValidity.Absent
          afterReset.history.isSemanticallyValid(b.blockTransactions.id) shouldBe ModifierSemanticValidity.Absent
        }

        // Apply the same blocks again through the holder.
        blocks.drop(1).foreach { b =>
          val outcome = applyBlock(b)
          info(s"re-apply height ${b.height}: $outcome")
          outcome.isSuccess shouldBe true
        }
        val view = getCurrentView
        view.history.fullBlockHeight shouldBe 4
        view.history.bestFullBlockIdOpt shouldBe Some(blocks(3).id)
        view.state.version shouldBe idToVersion(blocks(3).id)
        view.history.isSemanticallyValid(corrupted.id) shouldBe ModifierSemanticValidity.Valid
        view.history.isSemanticallyValid(corrupted.blockTransactions.id) shouldBe ModifierSemanticValidity.Valid
        view.history.isSemanticallyValid(blocks(3).id) shouldBe ModifierSemanticValidity.Valid
      } finally {
        generatedState.closeStorage()
      }
    }
  }
}
