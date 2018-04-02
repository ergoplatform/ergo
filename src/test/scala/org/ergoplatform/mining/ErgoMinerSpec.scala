package org.ergoplatform.mining

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import akka.testkit.TestKit
import org.ergoplatform.local.ErgoMiner.{MiningStatusRequest, MiningStatusResponse, StartMining}
import org.ergoplatform.local.TransactionGenerator.StartGeneration
import org.ergoplatform.local.{ErgoMinerRef, TransactionGeneratorRef}
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.nodeView.{ErgoNodeViewRef, ErgoReadersHolderRef}
import org.ergoplatform.settings.{ErgoSettings, TestingSettings}
import org.ergoplatform.utils.ErgoGenerators
import org.scalatest.{FlatSpecLike, Matchers}
import scorex.core.settings.ScorexSettings
import scorex.core.utils.NetworkTimeProvider

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutor}

class ErgoMinerSpec extends TestKit(ActorSystem()) with FlatSpecLike with Matchers with ErgoGenerators {

  val defaultAwaitDuration = 5 seconds
  implicit val timeout = akka.util.Timeout(defaultAwaitDuration)

  it should "not freeze while generating candidate block with large amount of txs" in {
    val defaultSettings: ErgoSettings = ErgoSettings.read(None)
    implicit val ec: ExecutionContextExecutor = system.dispatcher

    val nodeSettings = defaultSettings.nodeSettings.copy(mining = true, stateType = StateType.Utxo, offlineGeneration = true)
    val ergoSettings = defaultSettings.copy(nodeSettings = nodeSettings)
    val settings: ScorexSettings = ergoSettings.scorexSettings
    val timeProvider = new NetworkTimeProvider(settings.ntp)

    val nodeId = Array.fill(10)(1: Byte)

    val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(ergoSettings, timeProvider)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(nodeViewHolderRef)
    val minerRef: ActorRef = ErgoMinerRef(ergoSettings, nodeViewHolderRef, readersHolderRef, nodeId, timeProvider)

    val testingSettings = TestingSettings(true, 500)
    val txGen = TransactionGeneratorRef(nodeViewHolderRef, testingSettings)
    txGen ! StartGeneration
    minerRef ! StartMining

    expectNoMessage(40 seconds)

    val respF = (minerRef ? MiningStatusRequest).mapTo[MiningStatusResponse]
    //check that miner actor is still alive
    noException should be thrownBy {
      val result = Await.result(respF, defaultAwaitDuration)
      result.isMining shouldBe true
      result.candidateBlock shouldBe defined
      result.votes.sameElements(nodeId) shouldBe true
    }
  }
}
