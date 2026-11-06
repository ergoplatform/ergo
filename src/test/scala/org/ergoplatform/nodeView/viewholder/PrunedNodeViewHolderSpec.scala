package org.ergoplatform.nodeView.viewholder

import akka.actor.ActorRef
import org.ergoplatform.mining.DefaultFakePowScheme
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.nodeView.LocallyGeneratedBlockSection
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.nodeView.state.{DigestState, StateType}
import org.ergoplatform.settings.{ErgoSettings, ErgoSettingsReader, VotingSettings}
import org.ergoplatform.utils.fixtures.NodeViewFixture
import org.ergoplatform.utils.{ErgoCorePropertyTest, NoShrink, NodeViewTestOps}
import scala.concurrent.duration._

/**
  * Test how node view holder is working in pruned mode
  */
class PrunedNodeViewHolderSpec extends ErgoCorePropertyTest with NodeViewTestOps with NoShrink {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ValidBlocksGenerators._

  private val BlockInterval = 2.minutes

  def prunedSettings(blocksToKeep: Int): ErgoSettings = {
    val defaultSettings = ErgoSettingsReader.read()
    defaultSettings.copy(
      chainSettings = defaultSettings.chainSettings.copy(
        powScheme = new DefaultFakePowScheme(defaultSettings.chainSettings.powScheme.k, defaultSettings.chainSettings.powScheme.n),
        voting = VotingSettings(10, 10, 10, 10000, "01"),
        blockInterval = BlockInterval
      ),
      nodeSettings = defaultSettings.nodeSettings.copy(
        stateType = StateType.Digest,
        verifyTransactions = true,
        blocksToKeep = blocksToKeep
      )
    )
  }

  def genFullChain(genesisState: WrappedUtxoState, howMany: Int, nodeViewHolderRef: ActorRef): Seq[ErgoFullBlock] = {
    (1 to howMany).foldLeft((Seq[ErgoFullBlock](), genesisState, None: Option[ErgoFullBlock])) { case ((chain, wus, parentOpt), h) =>
      val time = System.currentTimeMillis() - (howMany - h) * (BlockInterval.toMillis * 20)
      val block = validFullBlock(parentOpt, wus, time)
      val newState = wus.applyModifier(block)(mod => nodeViewHolderRef ! mod).get
      (chain :+ block, newState, Some(block))
    }._1
  }

  private def testCode(fixture: NodeViewFixture, toSkip: Int, totalBlocks: Int = 20) = {
    import fixture._

    val (us, bh) = createUtxoState(fixture.settings)
    val wus = WrappedUtxoState(us, bh, fixture.settings)

    val fullChain = genFullChain(wus, totalBlocks, nodeViewHolderRef)

    fullChain.foreach { block =>
      applyHeader(block.header).isSuccess shouldBe true
    }

    fullChain.takeRight(totalBlocks - toSkip).foreach { block =>
      block.blockSections.foreach { section =>
        nodeViewHolderRef ! LocallyGeneratedBlockSection(section)
        Thread.sleep(50)
      }
    }

    val state = getCurrentState.asInstanceOf[DigestState]
    state.version shouldBe fullChain.last.id
    state.stateContext.lastHeaderOpt.get.id shouldBe fullChain.last.header.id
  }

  property(s"pruned chain bootstrapping - blocksToKeep = -1 - all the blocks are to be applied to the state") {
    new NodeViewFixture(prunedSettings(-1), parameters).apply(f => testCode(f, 0))
  }

  property(s"pruned chain bootstrapping - blocksToKeep = 3 - first 9 blocks out of 20 are not to be applied to the state") {
    new NodeViewFixture(prunedSettings(3), parameters).apply(f => testCode(f, 9))
  }

  property(s"pruned chain bootstrapping - blocksToKeep = 15 - all the blocks are to be applied to the state") {
    new NodeViewFixture(prunedSettings(15), parameters).apply(f => testCode(f, 0))
  }

  property(s"pruned chain bootstrapping - total = 30, blocksToKeep = 15 - first 9 blocks are not to be applied to the state") {
    new NodeViewFixture(prunedSettings(15), parameters).apply(f => testCode(f, 9, 30))
  }

}
