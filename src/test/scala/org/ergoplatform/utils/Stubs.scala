package org.ergoplatform.utils

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import org.ergoplatform.local.ErgoMiner.{MiningStatusRequest, MiningStatusResponse}
import org.ergoplatform.mining.DefaultFakePowScheme
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetDataFromHistory, GetReaders, Readers}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.nodeView.state.{DigestState, StateType}
import org.ergoplatform.nodeView.wallet.ErgoWalletActor.{GenerateTransaction, ReadBalances, ReadTrackedAddresses}
import org.ergoplatform.nodeView.wallet._
import org.ergoplatform.sanity.ErgoSanity.HT
import org.ergoplatform.settings.Constants.hashLength
import org.ergoplatform.settings._
import org.ergoplatform.utils.generators.{ChainGenerator, ErgoGenerators, ErgoTransactionGenerators}
import scorex.core.app.Version
import scorex.core.network.Handshake
import scorex.core.network.peer.PeerInfo
import scorex.core.network.peer.PeerManager.ReceivableMessages.{GetAllPeers, GetBlacklistedPeers, GetConnectedPeers}
import scorex.core.settings.ScorexSettings
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import scorex.testkit.utils.FileUtils

import scala.collection.mutable
import scala.concurrent.duration._
import scala.util.Success

trait Stubs extends ErgoGenerators with ErgoTestHelpers with ChainGenerator with FileUtils {

  implicit val system: ActorSystem

  lazy val chain: Seq[ErgoFullBlock] = genChain(4)

  lazy val history: HT = applyChain(generateHistory(), chain)

  lazy val state: DigestState = {
    boxesHolderGen.map(WrappedUtxoState(_, createTempDir, emission, None)).map { wus =>
      DigestState.create(Some(wus.version), Some(wus.rootHash), createTempDir, settings)
    }
  }.sample.value

  lazy val wallet = new WalletStub

  val txs: Seq[ErgoTransaction] = chain.head.transactions

  lazy val memPool: ErgoMemPool = ErgoMemPool.empty.put(txs).get
  lazy val readers = Readers(history, state, memPool, wallet)


  val inetAddr1 = new InetSocketAddress("92.92.92.92", 27017)
  val inetAddr2 = new InetSocketAddress("93.93.93.93", 27017)
  val ts1: Long = System.currentTimeMillis() - 100
  val ts2: Long = System.currentTimeMillis() + 100

  val peers = Map(
    inetAddr1 -> PeerInfo(ts1, Some("first")),
    inetAddr2 -> PeerInfo(ts2, Some("second"))
  )

  val protocolVersion = Version("1.1.1")

  val connectedPeers = Seq(
    Handshake("node_pop", protocolVersion, "first", Some(inetAddr1), Seq(), ts1),
    Handshake("node_pop", protocolVersion, "second", Some(inetAddr2), Seq(), ts2)
  )

  val blacklistedPeers = Seq("4.4.4.4:1111", "8.8.8.8:2222")

  class PeersManagerStub extends Actor {
    def receive: PartialFunction[Any, Unit] = {
      case GetConnectedPeers => sender() ! connectedPeers
      case GetAllPeers => sender() ! peers
      case GetBlacklistedPeers => sender() ! blacklistedPeers
    }
  }

  object PeersManagerStub {
    def props(): Props = Props(new PeersManagerStub)
  }

  val minerInfo = MiningStatusResponse(isMining = false, candidateBlock = None)

  class MinerStub extends Actor {
    def receive: PartialFunction[Any, Unit] = {
      case MiningStatusRequest => sender() ! minerInfo
    }
  }

  object MinerStub {
    def props(): Props = Props(new MinerStub)
  }

  class NodeViewStub extends Actor {
    def receive: PartialFunction[Any, Unit] = {
      case _ =>
    }
  }

  object NodeViewStub {
    def props(): Props = Props(new NodeViewStub)
  }

  class NetworkControllerStub extends Actor {
    def receive: PartialFunction[Any, Unit] = {
      case _ =>
    }
  }

  object NetworkControllerStub {
    def props(): Props = Props(new NetworkControllerStub)
  }

  class PeerManagerStub extends Actor {
    def receive: PartialFunction[Any, Unit] = {
      case _ =>
    }
  }

  object PeerManagerStub {
    def props(): Props = Props(new PeerManagerStub)
  }

  class WalletActorStub extends Actor {
    def seed: String = "walletstub"

    private implicit val addressEncoder: ErgoAddressEncoder = new ErgoAddressEncoder(settings)
    private val prover = new ErgoProvingInterpreter(seed, 2)
    private val trackedAddresses: mutable.Buffer[ErgoAddress] =
      mutable.Buffer(prover.dlogPubkeys: _ *).map(P2PKAddress.apply)


    def receive: Receive = {

      case ReadBalances(chainStatus) =>
        sender ! BalancesSnapshot(0, WalletActorStub.balance(chainStatus), Map.empty)

      case ReadTrackedAddresses =>
        sender ! trackedAddresses

      case GenerateTransaction(_) =>
        val input = ErgoTransactionGenerators.inputGen.sample.value
        val tx = ErgoTransaction(IndexedSeq(input), IndexedSeq(ergoBoxCandidateGen.sample.value))
        sender ! Success(tx)
    }
  }

  object WalletActorStub {
    def props(): Props = Props(new WalletActorStub)
    def balance(chainStatus: ChainStatus): Long = if (chainStatus.onchain) confirmedBalance else unconfirmedBalance
    def confirmedBalance: Long = 1L
    def unconfirmedBalance: Long = 2L
  }

  class WalletStub extends ErgoWalletReader {
    val actor: ActorRef = system.actorOf(WalletActorStub.props())
  }


  class ReadersStub extends Actor {
    def receive: PartialFunction[Any, Unit] = {
      case GetReaders => sender() ! readers
      case GetDataFromHistory(f) => sender() ! f(history)
    }
  }

  object ReadersStub {
    def props(): Props = Props(new ReadersStub)
  }

  lazy val readersRef: ActorRef = system.actorOf(ReadersStub.props())
  lazy val minerRef: ActorRef = system.actorOf(MinerStub.props())
  lazy val peerManagerRef: ActorRef = system.actorOf(PeerManagerStub.props())
  lazy val pmRef: ActorRef = system.actorOf(PeersManagerStub.props())
  lazy val nodeViewRef: ActorRef = system.actorOf(NodeViewStub.props())
  lazy val networkControllerRef: ActorRef = system.actorOf(NetworkControllerStub.props())

  def generateHistory(verifyTransactions: Boolean = true,
                      stateType: StateType = StateType.Digest,
                      PoPoWBootstrap: Boolean = false,
                      blocksToKeep: Int = 100,
                      epochLength: Int = 100000000,
                      useLastEpochs: Int = 10): ErgoHistory = {

    val networkPrefix = 0: Byte
    val blockInterval = 1.minute
    val miningDelay = 1.second
    val minimalSuffix = 2
    val nodeSettings: NodeConfigurationSettings = NodeConfigurationSettings(stateType, verifyTransactions, blocksToKeep,
      PoPoWBootstrap, minimalSuffix, mining = false, miningDelay, offlineGeneration = false, 200)
    val scorexSettings: ScorexSettings = null
    val testingSettings: TestingSettings = null
    val walletSettings: WalletSettings = null
    val monetarySettings = settings.chainSettings.monetary
    val chainSettings =
      ChainSettings(networkPrefix, blockInterval, epochLength, useLastEpochs, DefaultFakePowScheme, monetarySettings)

    val dir = createTempDir
    val fullHistorySettings: ErgoSettings = ErgoSettings(dir.getAbsolutePath, chainSettings, testingSettings,
      nodeSettings, scorexSettings, walletSettings, CacheSettings.default)

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
      Digest32 @@ Array.fill(hashLength)(0.toByte)
    ).value
  }
}
