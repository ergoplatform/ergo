package org.ergoplatform.nodeView.viewholder

import akka.testkit.TestProbe
import org.ergoplatform.core.idToVersion
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.BlockSectionsProcessingCacheUpdate
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.ModifiersFromRemote
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.utils.{ErgoCorePropertyTest, NodeViewTestConfig, NodeViewTestOps}
import org.ergoplatform.utils.fixtures.NodeViewFixture
import org.ergoplatform.utils.generators.ValidBlocksGenerators._

import scala.concurrent.Await
import scala.concurrent.duration._

class HeaderBodyCacheWakeupSpec extends ErgoCorePropertyTest with NodeViewTestOps {
  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters

  Seq(StateType.Utxo, StateType.Digest).foreach { stateType =>
    property(s"remote header wakes already cached block sections in $stateType state") {
      val fixture = new NodeViewFixture(
        NodeViewTestConfig(stateType, verifyTransactions = true, popowBootstrap = false).toSettings,
        parameters)
      import fixture._
      val (generationState, boxes) = createUtxoState(fixture.settings)
      try {
        val prefix = validFullBlock(None, generationState, boxes)
        val afterPrefix = WrappedUtxoState(generationState, boxes, fixture.settings)
          .applyModifier(prefix)(_ => ()).get
        val next = validFullBlock(Some(prefix), afterPrefix)
        applyBlock(prefix).isSuccess shouldBe true
        getCurrentState.version shouldBe idToVersion(prefix.id)

        val cacheProbe = TestProbe()(actorSystem)
        actorSystem.eventStream.subscribe(cacheProbe.ref, classOf[BlockSectionsProcessingCacheUpdate])
        val sections: Seq[BlockSection] = Seq(next.blockTransactions, next.extension, next.adProofs.get)
        sections.map(_.modifierTypeId).distinct.size shouldBe 3
        sections.zipWithIndex.foreach { case (section, index) =>
          nodeViewHolderRef ! ModifiersFromRemote(Seq(section))
          val cached = cacheProbe.expectMsgType[BlockSectionsProcessingCacheUpdate](5.seconds)
          cached.blockSectionsCacheSize shouldBe index + 1
          cached.cleared._2 shouldBe empty
        }
        val beforeHeader = getCurrentView
        beforeHeader.history.contains(next.header.id) shouldBe false
        sections.foreach(section => beforeHeader.history.contains(section.id) shouldBe false)
        beforeHeader.state.version shouldBe idToVersion(prefix.id)

        // The cache-update event is an actor-processing barrier, not another drain trigger.
        // No body section is resent after its prerequisite header arrives.
        nodeViewHolderRef ! ModifiersFromRemote(Seq(next.header))
        val afterHeader = cacheProbe.expectMsgType[BlockSectionsProcessingCacheUpdate](5.seconds)
        afterHeader.headersCacheSize shouldBe 0
        val current = getCurrentView
        current.history.getFullBlock(next.header).map(_.id) shouldBe Some(next.id)
        sections.foreach(section => current.history.contains(section.id) shouldBe true)
        current.history.bestFullBlockIdOpt shouldBe Some(next.id)
        current.state.version shouldBe idToVersion(next.id)
        current.state.rootDigest.toSeq shouldBe next.header.stateRoot.toSeq
        afterHeader.blockSectionsCacheSize shouldBe 0
      } finally {
        generationState.closeStorage()
        Await.result(actorSystem.terminate(), 15.seconds)
      }
    }
  }
}
