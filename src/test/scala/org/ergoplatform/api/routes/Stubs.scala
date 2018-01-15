package org.ergoplatform.api.routes

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorSystem, Props}
import org.ergoplatform.ErgoSanity.HT
import org.ergoplatform.local.ErgoMiner.{MiningStatusRequest, MiningStatusResponse}
import org.ergoplatform.modifiers.history.{DefaultFakePowScheme, Header}
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetDataFromHistory, GetReaders, Readers}
import org.ergoplatform.nodeView.WrappedUtxoState
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.DigestState
import org.ergoplatform.settings.Constants.hashLength
import org.ergoplatform.settings._
import org.ergoplatform.utils.{ChainGenerator, ErgoGenerators, ErgoTestHelpers}
import scorex.core.app.Version
import scorex.core.network.Handshake
import scorex.core.network.peer.{PeerInfo, PeerManager}
import scorex.core.settings.ScorexSettings
import scorex.core.utils.{NetworkTime, NetworkTimeProvider}
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import scorex.testkit.utils.FileUtils

import scala.concurrent.duration._

trait Stubs extends ErgoGenerators with ErgoTestHelpers with ChainGenerator with FileUtils {

  implicit val system: ActorSystem

  lazy val chain = genChain(4, Seq.empty)

  lazy val nodeId = Algos.hash("testroute").take(5)

  lazy val settings = ErgoSettings.read(None)
  lazy val history = applyChain(generateHistory(), chain)

  lazy val state = { boxesHolderGen.map(WrappedUtxoState(_, createTempDir, None)).map { wus =>
    DigestState.create(Some(wus.version), Some(wus.rootHash), createTempDir, settings.nodeSettings).get
  }
  }.sample.get

  val txs = chain.head.transactions

  lazy val memPool = ErgoMemPool.empty.put(txs).get
  lazy val readers = Readers(Some(history), Some(state), Some(memPool))


  val inetAddr1 = new InetSocketAddress("92.92.92.92",27017)
  val inetAddr2 = new InetSocketAddress("93.93.93.93",27017)
  val ts1 = System.currentTimeMillis() - 100
  val ts2 = System.currentTimeMillis() + 100

  val peers = Map(
    inetAddr1 -> PeerInfo(ts1, Some(1L), Some("first")),
    inetAddr2 -> PeerInfo(ts2, Some(2L), Some("second"))
  )

  val protocolVersion = Version("1.1.1")

  val connectedPeers = Seq(
    Handshake("node_pop", protocolVersion, "first", Some(inetAddr1), ts1),
    Handshake("node_pop", protocolVersion, "second", Some(inetAddr2), ts2)
  )

  val blacklistedPeers = Seq("4.4.4.4:1111", "8.8.8.8:2222")

  class PeersManagerStub extends Actor {
    def receive = {
      case PeerManager.GetConnectedPeers => sender() ! connectedPeers
      case PeerManager.GetAllPeers => sender() ! peers
      case PeerManager.GetBlacklistedPeers => sender() ! blacklistedPeers
    }
  }

  object PeersManagerStub {
    def props() = Props(new PeersManagerStub)
  }

  val minerInfo = MiningStatusResponse(isMining = false, votes = Array.empty, candidateBlock = None)

  class MinerStub extends Actor {
    def receive = { case MiningStatusRequest => sender() ! minerInfo }
  }

  object MinerStub {
    def props() = Props(new MinerStub)
  }

  class NodeViewStub extends Actor {
    def receive = { case _ => println("hey") }
  }

  object NodeViewStub {
    def props() = Props(new NodeViewStub)
  }

  class NetworkControllerStub extends Actor {
    def receive = { case _ => println("hey") }
  }

  object NetworkControllerStub {
    def props() = Props(new NetworkControllerStub)
  }


  class ReadersStub extends Actor {
    def receive = {
      case GetReaders =>
        sender() ! readers
      case GetDataFromHistory(f) => sender() ! f(history)
    }
  }

  object ReadersStub {
    def props() = Props(new ReadersStub)
  }

  lazy val readersRef = system.actorOf(ReadersStub.props())
  lazy val minerRef = system.actorOf(MinerStub.props())
  lazy val pmRef = system.actorOf(PeersManagerStub.props())
  lazy val nodeViewRef = system.actorOf(NodeViewStub.props())
  lazy val networkControllerRef = system.actorOf(NetworkControllerStub.props())

  def generateHistory(verifyTransactions: Boolean = true,
                      ADState: Boolean = true,
                      PoPoWBootstrap: Boolean = false,
                      blocksToKeep: Int  = 100,
                      epochLength: Int = 100000000,
                      useLastEpochs: Int = 10): ErgoHistory = {

    val blockInterval = 1.minute
    val miningDelay = 1.second
    val minimalSuffix = 2
    val nodeSettings: NodeConfigurationSettings = NodeConfigurationSettings(ADState, verifyTransactions, blocksToKeep,
      PoPoWBootstrap, minimalSuffix, false, miningDelay, false)
    val scorexSettings: ScorexSettings = null
    val testingSettings: TestingSettings = null
    val chainSettings = ChainSettings(blockInterval, epochLength, useLastEpochs, DefaultFakePowScheme)

    val dir = createTempDir
    val fullHistorySettings: ErgoSettings = ErgoSettings(dir.getAbsolutePath, chainSettings, testingSettings,
      nodeSettings, scorexSettings)

    ErgoHistory.readOrGenerate(fullHistorySettings, timeProvider)
  }

  def syntacticallyValidModifier(history: HT): Header = {
    val bestTimestamp = history.bestHeaderOpt.map(_.timestamp + 1).getOrElse(timeProvider.time())

    DefaultFakePowScheme.prove(
      history.bestHeaderOpt,
      Constants.InitialNBits,
      ADDigest @@ Array.fill(hashLength + 1)(0.toByte),
      Digest32 @@ Array.fill(hashLength)(0.toByte),
      Digest32 @@ Array.fill(hashLength)(0.toByte),
      Math.max(timeProvider.time(), bestTimestamp),
      Array.fill(5)(0.toByte)
    )
  }
}
