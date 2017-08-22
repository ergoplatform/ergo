package org.ergoplatform.nodeView

import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestActors, TestKit}
import io.circe
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{DigestState, ErgoState}
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoGenerators
import org.scalatest.{BeforeAndAfterAll, Matchers, PropSpecLike}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.core.NodeViewHolder.GetDataFromCurrentView
import scorex.testkit.TestkitHelpers

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

  lazy val settings: ErgoSettings = new ErgoSettings {
    override def settingsJSON: Map[String, circe.Json] = Map()
  }

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  property("genesis state digest") {
    val digestHolder = system.actorOf(Props(classOf[DigestErgoNodeViewHolder], settings))

    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Boolean] { v =>
      println("rh: " + v.state.rootHash.sameElements(ErgoState.afterGenesisStateDigest))
      v.state.rootHash.sameElements(ErgoState.afterGenesisStateDigest)
    }
    expectMsg(true)
  }
}
