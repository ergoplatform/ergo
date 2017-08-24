package org.ergoplatform.nodeView

import java.io.File

import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import io.circe
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{DigestState, ErgoState}
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoGenerators
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, Matchers, PropSpecLike}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.core.LocalInterface.LocallyGeneratedModifier
import scorex.core.NodeViewHolder.GetDataFromCurrentView
import scorex.core.NodeViewModifier.ModifierId
import scorex.testkit.TestkitHelpers

import scala.reflect.io.Path
import scala.util.Random
import scala.concurrent.duration._


class DigestErgoNodeViewHolderSpecification extends
  TestKit(ActorSystem("DigestErgoNodeViewHolderSpec"))
  with ImplicitSender
  with PropSpecLike
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators
  with TestkitHelpers
  with BeforeAndAfterEach
  with BeforeAndAfterAll {

  lazy val settings: ErgoSettings = new ErgoSettings {
    override lazy val dataDir: String = s"/tmp/ergo/${Random.nextInt}"

    override def settingsJSON: Map[String, circe.Json] = Map()
  }

  override def beforeEach {
    Path(new File(settings.dataDir)).deleteRecursively()
    new File(settings.dataDir).mkdirs()
  }

  override def afterEach {
    Path(new File(settings.dataDir)).deleteRecursively()
  }

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  property("genesis - state digest") {
    val digestHolder = system.actorOf(Props(classOf[DigestErgoNodeViewHolder], settings))

    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Boolean] { v =>
      v.state.rootHash.sameElements(ErgoState.afterGenesisStateDigest)
    }
    expectMsg(true)
  }

  property("genesis - history (no genesis block there yet)") {
    val digestHolder = system.actorOf(Props(classOf[DigestErgoNodeViewHolder], settings))

    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[Header]] { v =>
      v.history.bestHeaderOpt
    }
    expectMsg(None)
  }

  property("genesis - apply valid block") {
    val digestHolder = system.actorOf(Props(classOf[DigestErgoNodeViewHolder], settings))

    val (us, bh) = ErgoState.generateGenesisUtxoState(new File(s"/tmp/ergo/${Random.nextInt()}").ensuring(_.mkdirs()))

    val block = validFullBlock(None, us, bh)

    /*
    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, ErgoHistory] { v =>
      v.history
    }

    println(receiveN(1).head.asInstanceOf[ErgoHistory].append(block).get)*/

    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[Header]] { v =>
      v.history.bestHeaderOpt
    }
    expectMsg(None)


    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Int] { v =>
      v.history.height
    }
    expectMsg(-1)


    //sending header
    digestHolder ! LocallyGeneratedModifier[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction, Header](block.header)


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

    /*digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[Header]] { v =>
      v.history.bestHeaderOpt
    }
    expectMsg(Some(block.header))*/
  }
}
