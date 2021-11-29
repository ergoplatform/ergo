package scorex.testkit.properties

import akka.actor._
import akka.testkit.TestProbe
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.nodeView.history.ErgoHistory
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.ergoplatform.nodeView.ErgoNodeViewHolder.CurrentView
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, LocallyGeneratedModifier, ModifiersFromRemote}
import org.ergoplatform.network.ErgoNodeViewSynchronizer.ReceivableMessages._
import org.ergoplatform.nodeView.state.ErgoState
import scorex.testkit.generators._
import scorex.testkit.utils.AkkaFixture
import scorex.util.ScorexLogging

import scala.concurrent.Await
import scala.concurrent.duration._

@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
trait NodeViewHolderTests[ST <: ErgoState[ST]]
  extends AnyPropSpec
    with Matchers
    with ScorexLogging
    with SyntacticallyTargetedModifierProducer
    with TotallyValidModifierProducer[ST]
    with SemanticallyInvalidModifierProducer[ST]
    with CustomModifierProducer[ST]
    with ObjectGenerators {

  def nodeViewHolder(implicit system: ActorSystem): (ActorRef, TestProbe, BlockSection, ST, ErgoHistory)

  class HolderFixture extends AkkaFixture {
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val (node, eventListener, mod, s, h) = nodeViewHolder
  }

  private def withFixture(testCode: HolderFixture => Any): Unit = {
    val fixture = new HolderFixture
    try {
      testCode(fixture)
    } finally {
      Await.result(fixture.system.terminate(), Duration.Inf)
    }
  }

  private type CurrentViewType = CurrentView[ST]

  private def withView[T](node: ActorRef)(f: CurrentViewType => T)
                         (implicit system: ActorSystem): T = {
    val probe = TestProbe()
    probe.send(node,
      GetDataFromCurrentView[ST, CurrentViewType] { view => view })
    val view = probe.expectMsgClass(10.seconds, classOf[CurrentViewType])
    f(view)
  }

  property("NodeViewHolder: modifiers from remote") {
    withFixture { ctx =>
      import ctx._
      val p = TestProbe()

      system.eventStream.subscribe(eventListener.ref, classOf[ModifiersProcessingResult])
      p.send(node, ModifiersFromRemote(Seq(mod)))
      eventListener.expectMsgType[ModifiersProcessingResult]
    }
  }

  property("NodeViewHolder syntactically valid modifier subscription") {
    withFixture { ctx =>
      import ctx._
      val p = TestProbe()

      system.eventStream.subscribe(eventListener.ref, classOf[SyntacticallySuccessfulModifier])
      p.send(node, GetDataFromCurrentView[ST, BlockSection] { v => totallyValidModifiers(v.history, v.state, 2).head })
      val mod = p.expectMsgClass(classOf[BlockSection])
      p.send(node, LocallyGeneratedModifier(mod))
      eventListener.expectMsgType[SyntacticallySuccessfulModifier]
    }
  }

  property("NodeViewHolder: syntactically failed modifier subscription") {
    withFixture { ctx =>
      import ctx._
      val p = TestProbe()

      system.eventStream.subscribe(eventListener.ref, classOf[SyntacticallyFailedModification])
      val invalid = syntacticallyInvalidModifier(h)
      p.send(node, LocallyGeneratedModifier(invalid))
      eventListener.expectMsgType[SyntacticallyFailedModification]
    }
  }

  property("NodeViewHolder: semantically valid modifier subscription") {
    withFixture { ctx =>
      import ctx._
      val p = TestProbe()

      system.eventStream.subscribe(eventListener.ref, classOf[SyntacticallySuccessfulModifier])
      system.eventStream.subscribe(eventListener.ref, classOf[SemanticallySuccessfulModifier])
      p.send(node, GetDataFromCurrentView[ST, BlockSection] { v => totallyValidModifiers(v.history, v.state, 2).head })
      val mod = p.expectMsgClass(classOf[BlockSection])
      p.send(node, LocallyGeneratedModifier(mod))
      eventListener.expectMsgType[SyntacticallySuccessfulModifier]
      eventListener.expectMsgType[SemanticallySuccessfulModifier]
    }
  }

  property("NodeViewHolder: semantically failed modifier subscription") {
    withFixture { ctx =>
      import ctx._
      val p = TestProbe()

      system.eventStream.subscribe(eventListener.ref, classOf[SyntacticallySuccessfulModifier])
      system.eventStream.subscribe(eventListener.ref, classOf[SemanticallyFailedModification])
      p.send(node, GetDataFromCurrentView[ST, BlockSection] { v => semanticallyInvalidModifier(v.state) })
      val invalid = p.expectMsgClass(classOf[BlockSection])
      p.send(node, LocallyGeneratedModifier(invalid))
      eventListener.expectMsgType[SyntacticallySuccessfulModifier]
      eventListener.expectMsgType[SemanticallyFailedModification]
    }
  }

  property("NodeViewHolder: syntactically/semantically valid modifier subscription") {
    withFixture { ctx =>
      import ctx._
      val p = TestProbe()

      system.eventStream.subscribe(eventListener.ref, classOf[SyntacticallySuccessfulModifier])
      system.eventStream.subscribe(eventListener.ref, classOf[SemanticallySuccessfulModifier])
      p.send(node, GetDataFromCurrentView[ST, BlockSection] { v => totallyValidModifiers(v.history, v.state, 2).head })
      val mod = p.expectMsgClass(classOf[BlockSection])
      p.send(node, LocallyGeneratedModifier(mod))
      eventListener.expectMsgType[SyntacticallySuccessfulModifier]
      eventListener.expectMsgType[SemanticallySuccessfulModifier]
    }
  }

  property("NodeViewHolder: check state after creation") {
    withFixture { ctx =>
      import ctx._
      val p = TestProbe()

      p.send(node, GetDataFromCurrentView[ST, Boolean] { v =>
        v.state.version == s.version
      })
      p.expectMsg(true)
    }
  }

  property("NodeViewHolder: check that a valid modifier is applicable") {
    withFixture { ctx =>
      import ctx._
      val p = TestProbe()

      p.send(node, GetDataFromCurrentView[ST, Boolean] { v =>
        v.history.applicableTry(mod).isSuccess
      })
      p.expectMsg(true)
    }
  }

  property("NodeViewHolder: check that valid modifiers are applicable") {
    withFixture { ctx =>
      import ctx._
      val p = TestProbe()

      system.eventStream.subscribe(eventListener.ref, classOf[SyntacticallySuccessfulModifier])
      system.eventStream.subscribe(eventListener.ref, classOf[SyntacticallyFailedModification])
      p.send(node, GetDataFromCurrentView[ST, Seq[BlockSection]] { v =>
        totallyValidModifiers(v.history, v.state, 10) //todo: fix magic number
      })
      val mods = p.expectMsgClass(classOf[Seq[BlockSection]])

      mods.foreach { mod =>
        p.send(node, LocallyGeneratedModifier(mod))
      }

      (1 to mods.size).foreach(_ => eventListener.expectMsgType[SyntacticallySuccessfulModifier])
    }
  }

  property("NodeViewHolder: apply locally generated mod") {
    withFixture { ctx =>
      import ctx._
      val p = TestProbe()

      system.eventStream.subscribe(eventListener.ref, classOf[SyntacticallySuccessfulModifier])
      system.eventStream.subscribe(eventListener.ref, classOf[SyntacticallyFailedModification])

      val invalid = syntacticallyInvalidModifier(h)

      p.send(node, LocallyGeneratedModifier(invalid))

      eventListener.expectMsgType[SyntacticallyFailedModification]

      p.send(node, LocallyGeneratedModifier(mod))

      eventListener.expectMsgType[SyntacticallySuccessfulModifier]

      p.send(node, GetDataFromCurrentView[ST,  Boolean] { v =>
        v.state.version == s.version && v.history.contains(mod.id)
      })

      p.expectMsg(true)
    }
  }

  property("NodeViewHolder: simple forking") {
    withFixture { ctx =>
      import ctx._
      val p = TestProbe()

      val waitDuration = 5.seconds

      system.eventStream.subscribe(eventListener.ref, classOf[SyntacticallySuccessfulModifier])
      system.eventStream.subscribe(eventListener.ref, classOf[SyntacticallyFailedModification])

      p.send(node, GetDataFromCurrentView[ST, Seq[BlockSection]] { v => totallyValidModifiers(v.history, v.state, 2) })
      val initMods = p.expectMsgClass(waitDuration, classOf[Seq[BlockSection]])
      initMods.foreach { mod =>
        p.send(node, LocallyGeneratedModifier(mod))
        eventListener.expectMsgType[SyntacticallySuccessfulModifier]
      }

      p.send(node, GetDataFromCurrentView[ST, BlockSection] { v =>
        totallyValidModifiers(v.history, v.state, 2).head
      })
      val fork1Mod = p.expectMsgClass(waitDuration, classOf[BlockSection])

      p.send(node, GetDataFromCurrentView[ST, BlockSection] { v =>
        totallyValidModifiers(v.history, v.state, 2).head
      })
      val fork2Mod = p.expectMsgClass(waitDuration, classOf[BlockSection])

      p.send(node, LocallyGeneratedModifier(fork1Mod))
      p.send(node, LocallyGeneratedModifier(fork2Mod))
      eventListener.expectMsgType[SyntacticallySuccessfulModifier]
      eventListener.expectMsgType[SyntacticallySuccessfulModifier]

      p.send(node, GetDataFromCurrentView[ST, Boolean] { v =>
        v.history.contains(fork1Mod.id) || v.history.contains(fork2Mod.id)
      })

      p.expectMsg(10.seconds, true)
    }
  }

  /**
    * In this test we apply first a chain of 2 blocks and then a chain of 4 blocks, both started with the same
    * common block. We are expecting to observe "switching" here, though with non-chain structures there could be no
    * notion of switching, so what we check finally is that last block from the second chain is in "open surface"
    * (list of open blocks which do not have successors yet, size of the list is 1 in case of blockchain)
    */
  property("NodeViewHolder: forking - switching") {
    withFixture { ctx =>
      import ctx._
      val p = TestProbe()

      val opCountBeforeFork = 10
      val fork1OpCount = 2
      val fork2OpCount = 4

      val waitDuration = 10.seconds

      //some base operations, we don't wanna have fork right from genesis
      p.send(node, GetDataFromCurrentView[ST, Seq[BlockSection]] { v =>
        totallyValidModifiers(v.history, v.state, opCountBeforeFork)
      })
      val plainMods = p.expectMsgClass(waitDuration, classOf[Seq[BlockSection]])
      plainMods.foreach { mod => p.send(node, LocallyGeneratedModifier(mod)) }

      p.send(node, GetDataFromCurrentView[ST, Seq[BlockSection]] { v =>
        val mods = totallyValidModifiers(v.history, v.state, fork1OpCount)
        assert(mods.head.parentId == v.history.bestFullBlockIdOpt.orElse(v.history.bestHeaderIdOpt).get)
        mods
      })
      val fork1Mods = p.expectMsgClass(waitDuration, classOf[Seq[BlockSection]])

      p.send(node, GetDataFromCurrentView[ST, Seq[BlockSection]] { v =>
        totallyValidModifiers(v.history, v.state, fork2OpCount)
      })
      val fork2Mods = p.expectMsgClass(waitDuration, classOf[Seq[BlockSection]])

      fork1Mods.foreach { mod => p.send(node, LocallyGeneratedModifier(mod)) }
      fork2Mods.foreach { mod => p.send(node, LocallyGeneratedModifier(mod)) }

      p.send(node, GetDataFromCurrentView[ST, Boolean] { v =>
        v.history.bestFullBlockIdOpt.orElse(v.history.bestHeaderIdOpt).contains(fork2Mods.last.id)
      })
      p.expectMsg(true)
    }
  }

  property("NodeViewHolder: forking - switching with an invalid block") {
    withFixture { ctx =>
      import ctx._

      val opCountBeforeFork = 10
      val fork1OpCount = 4

      //some base operations, we don't wanna have fork right from genesis
      withView(node) { v =>
        totallyValidModifiers(v.history, v.state, opCountBeforeFork)
      }.foreach {
        mod => node ! LocallyGeneratedModifier(mod)
      }
      // generate the first fork with valid blocks
      val fork1Mods = withView(node) { v =>
        val mods = totallyValidModifiers(v.history, v.state, fork1OpCount)
        assert(mods.head.parentId == v.history.bestFullBlockIdOpt.orElse(v.history.bestHeaderIdOpt).get)
        mods
      }
      // generate the second fork with the invalid block
      val fork2Mods = withView(node) { v =>
        customModifiers(v.history, v.state,
          Seq[ModifierProducerTemplateItem](Valid,
            SynInvalid, // invalid modifier
            Valid, Valid, Valid, Valid, Valid, Valid))
      }
      // apply the first fork with valid blocks
      fork1Mods.foreach { mod => node ! LocallyGeneratedModifier(mod) }
      // apply the second fork with invalid block
      fork2Mods.foreach { mod => node ! LocallyGeneratedModifier(mod) }
      // verify that open surface consist of last block of the first chain,
      // or first block of the second chain, or both, but no any other option
      withView(node) { v =>
        v.history.bestFullBlockIdOpt.orElse(v.history.bestHeaderIdOpt).toSeq should (
          contain only fork1Mods.last.id
            or contain only fork2Mods.head.id
            or contain only(fork1Mods.last.id, fork2Mods.head.id)
          )
      }
    }
  }

}
