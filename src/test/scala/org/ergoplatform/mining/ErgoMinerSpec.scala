package org.ergoplatform.mining

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.testkit.TestKit
import akka.util.Timeout
import org.ergoplatform.local.ErgoMiner.{MiningStatusRequest, MiningStatusResponse, StartMining}
import org.ergoplatform.local.TransactionGenerator.StartGeneration
import org.ergoplatform.local.{ErgoMinerRef, TransactionGeneratorRef}
import org.ergoplatform.mining.Listener._
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.nodeView.{ErgoNodeViewRef, ErgoReadersHolderRef}
import org.ergoplatform.settings.{Algos, ErgoSettings, TestingSettings}
import org.ergoplatform.utils.ErgoGenerators
import org.scalatest.{FlatSpecLike, Matchers}
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import scorex.core.settings.ScorexSettings
import scorex.core.utils.NetworkTimeProvider
import scorex.testkit.utils.FileUtils

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

class ErgoMinerSpec extends TestKit(ActorSystem()) with FlatSpecLike with Matchers with ErgoGenerators with FileUtils {

  val defaultAwaitDuration: FiniteDuration = 5.seconds
  implicit val timeout: Timeout = Timeout(defaultAwaitDuration)

  def await[A](f: Future[A]): A = Await.result[A](f, defaultAwaitDuration)

  it should "not freeze while generating candidate block with large amount of txs" in {
    val tmpDir = createTempDir

    val defaultSettings: ErgoSettings = ErgoSettings.read(None).copy(directory = tmpDir.getAbsolutePath)
    implicit val ec: ExecutionContext = system.dispatcher

    val nodeSettings = defaultSettings.nodeSettings.copy(mining = true,
      stateType = StateType.Utxo,
      offlineGeneration = true,
      verifyTransactions = true)
    val chainSettings = defaultSettings.chainSettings.copy(blockInterval = 2.seconds)
    val ergoSettings = defaultSettings.copy(nodeSettings = nodeSettings, chainSettings = chainSettings)
    val networkSettings = ergoSettings.scorexSettings.network.copy(knownPeers = Seq.empty)
    val settings: ScorexSettings = ergoSettings.scorexSettings.copy(network = networkSettings)
    val timeProvider = new NetworkTimeProvider(settings.ntp)

    val nodeId = Algos.hash(ergoSettings.scorexSettings.network.nodeName).take(5)

    val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(ergoSettings, timeProvider)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(nodeViewHolderRef)
    val minerRef: ActorRef = ErgoMinerRef(ergoSettings, nodeViewHolderRef, readersHolderRef, nodeId, timeProvider)
    val listener = system.actorOf(Props(new Listener))

    val testingSettings = TestingSettings(transactionGeneration = true, 500)
    val txGen = TransactionGeneratorRef(nodeViewHolderRef, testingSettings)
    txGen ! StartGeneration

    minerRef ! StartMining
    expectNoMessage(20.seconds)

    //check that miner actor is still alive
    noException should be thrownBy {
      val result = await((minerRef ? MiningStatusRequest).mapTo[MiningStatusResponse])
      result.isMining shouldBe true
      result.candidateBlock shouldBe defined
      result.candidateBlock.get.votes.sameElements(nodeId) shouldBe true
      val height = result.candidateBlock.get.parentOpt.get.height
      val blocksGenerated = await((listener ? Status).mapTo[Int])
      blocksGenerated should be > 1
      blocksGenerated should be >= height
    }
  }
}

class Listener extends Actor {
  var generatedBlocks: Int = 0

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[_]])
  }

  override def receive: Receive = {
    case SemanticallySuccessfulModifier(_) => generatedBlocks += 1
    case Status => sender ! generatedBlocks
  }
}

object Listener {
  case object Status
}
