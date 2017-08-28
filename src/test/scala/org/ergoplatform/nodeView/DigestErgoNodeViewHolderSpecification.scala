package org.ergoplatform.nodeView

import java.io.File

import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{DigestState, ErgoState}
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoGenerators
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, Matchers, PropSpecLike}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.core.LocalInterface.LocallyGeneratedModifier
import scorex.core.NodeViewHolder
import scorex.core.NodeViewHolder.{EventType, GetDataFromCurrentView, SuccessfulModification}
import scorex.testkit.TestkitHelpers

import scala.reflect.io.Path
import scala.util.Random
import EventType._
import io.iohk.iodb.ByteArrayWrapper

class DigestErgoNodeViewHolderSpecification extends
  TestKit(ActorSystem("DigestErgoNodeViewHolderSpec"))
  with ImplicitSender
  with PropSpecLike
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators
  with TestkitHelpers
  with BeforeAndAfterAll {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  property("genesis - state digest") {
    //todo: reduce this boilerplate
    val settings: ErgoSettings = ErgoSettings.read(None).copy(directory = s"/tmp/ergo/nvh-test-${Random.nextInt()}")
    Path(new File(settings.directory)).deleteRecursively()
    new File(settings.directory).mkdirs()


    val digestHolder = system.actorOf(Props(classOf[DigestErgoNodeViewHolder], settings))

    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Boolean] { v =>
      v.state.rootHash.sameElements(ErgoState.afterGenesisStateDigest)
    }
    expectMsg(true)

    Path(new File(settings.directory)).deleteRecursively()
  }

  property("genesis - history (no genesis block there yet)") {
    val settings: ErgoSettings = ErgoSettings.read(None).copy(directory = s"/tmp/ergo/nvh-test-${Random.nextInt()}")
    Path(new File(settings.directory)).deleteRecursively()
    new File(settings.directory).mkdirs()


    val digestHolder = system.actorOf(Props(classOf[DigestErgoNodeViewHolder], settings))

    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[Header]] { v =>
      v.history.bestHeaderOpt
    }
    expectMsg(None)

    Path(new File(settings.directory)).deleteRecursively()
  }

  property("genesis - apply valid block") {
    val settings: ErgoSettings = ErgoSettings.read(None).copy(directory = s"/tmp/ergo/nvh-test-${Random.nextInt()}")
    Path(new File(settings.directory)).deleteRecursively()
    new File(settings.directory).mkdirs()


    val digestHolder = system.actorOf(Props(classOf[DigestErgoNodeViewHolder], settings))

    val (us, bh) = ErgoState.generateGenesisUtxoState(new File(s"/tmp/ergo/${Random.nextInt()}").ensuring(_.mkdirs()))

    val block = validFullBlock(None, us, bh)


    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[Header]] { v =>
      v.history.bestHeaderOpt
    }
    expectMsg(None)


    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Int] { v =>
      v.history.height
    }
    expectMsg(-1)

    digestHolder ! NodeViewHolder.Subscribe(Seq(SuccessfulPersistentModifier, FailedPersistentModifier))

    //sending header
    digestHolder ! LocallyGeneratedModifier[Header](block.header)

    expectMsg(SuccessfulModification(block.header, None))

    /*
    todo: uncomment / fix
    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Int] { v =>
      v.history.height
    }
    expectMsg(0)

    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[Int]] { v =>
      v.history.heightOf(block.header.id)
    }
    expectMsg(Some(0))


    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Int] { v =>
      v.history.lastHeaders(10).size
    }
    expectMsg(1)

    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Seq[ByteArrayWrapper]] { v =>
      v.history.openSurfaceIds().map(ByteArrayWrapper.apply)
    }
    expectMsg(Seq(ByteArrayWrapper(block.header.id)))

    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[Header]] { v =>
      v.history.bestHeaderOpt
    }
    expectMsg(Some(block.header))
  */

    Path(new File(settings.directory)).deleteRecursively()
  }
}
