package org.ergoplatform.utils

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.local.ErgoMiner
import org.ergoplatform.mining.{AutolykosSolution, ExternalCandidateBlock}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetDataFromHistory, GetReaders, Readers}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.nodeView.state.{DigestState, StateType}
import org.ergoplatform.nodeView.wallet.ErgoWalletActor._
import org.ergoplatform.nodeView.wallet._
import org.ergoplatform.nodeView.wallet.persistence.RegistryIndex
import org.ergoplatform.sanity.ErgoSanity.HT
import org.ergoplatform.settings.Constants.HashLength
import org.ergoplatform.settings._
import org.ergoplatform.utils.generators.{ChainGenerator, ErgoGenerators, ErgoTransactionGenerators}
import org.ergoplatform.wallet.boxes.{BoxCertainty, ChainStatus, TrackedBox}
import org.ergoplatform.wallet.interpreter.ErgoProvingInterpreter
import org.ergoplatform.wallet.secrets.DerivationPath
import org.ergoplatform.{ErgoAddressEncoder, P2PKAddress}
import scorex.core.app.Version
import scorex.core.network.NetworkController.ReceivableMessages.GetConnectedPeers
import scorex.core.network.peer.PeerManager.ReceivableMessages.{GetAllPeers, GetBlacklistedPeers}
import scorex.core.network.{Handshake, PeerSpec}
import scorex.core.settings.ScorexSettings
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import scorex.testkit.utils.FileUtils
import scorex.util.Random
import sigmastate.basics.DLogProtocol.{DLogProverInput, ProveDlog}

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Success

trait Stubs extends ErgoGenerators with ErgoTestHelpers with ChainGenerator with FileUtils {

  implicit val system: ActorSystem

  lazy val chain: Seq[ErgoFullBlock] = genChain(6)

  lazy val history: HT = applyChain(generateHistory(), chain)

  lazy val state: DigestState = {
    boxesHolderGen.map(WrappedUtxoState(_, createTempDir, None, settings)).map { wus =>
      DigestState.create(Some(wus.version), Some(wus.rootHash), createTempDir, stateConstants)
    }
  }.sample.value

  lazy val wallet = new WalletStub

  val txs: Seq[ErgoTransaction] = validTransactionsFromBoxHolder(boxesHolderGen.sample.get)._1

  lazy val memPool: ErgoMemPool = ErgoMemPool.empty(settings).put(txs).get
  lazy val readers = Readers(history, state, memPool, wallet)

  val protocolVersion = Version("1.1.1")

  val peerSpec: PeerSpec = defaultPeerSpec.copy(protocolVersion = protocolVersion)

  val connectedPeers: Seq[Handshake] = Seq(
    Handshake(peerSpec.copy(nodeName = "first"), ts1),
    Handshake(peerSpec.copy(nodeName = "second"), ts2)
  )

  val blacklistedPeers: Seq[String] = Seq("4.4.4.4:1111", "8.8.8.8:2222")

  val pk: ProveDlog = DLogProverInput(BigIntegers.fromUnsignedByteArray(Random.randomBytes(32))).publicImage
  val externalCandidateBlock = ExternalCandidateBlock(Array.fill(32)(2: Byte), BigInt(9999), pk)

  class PeersManagerStub extends Actor {
    def receive: Receive = {
      case GetAllPeers => sender() ! peers
      case GetBlacklistedPeers => sender() ! blacklistedPeers
    }
  }

  object PeersManagerStub {
    def props(): Props = Props(new PeersManagerStub)
  }

  class MinerStub extends Actor {
    def receive: Receive = {
      case ErgoMiner.PrepareCandidate => sender() ! Future.successful(externalCandidateBlock)
      case _: AutolykosSolution => sender() ! Future.successful(())
      case ErgoMiner.ReadMinerPk => sender() ! Some(pk)
    }
  }

  object MinerStub {
    def props(): Props = Props(new MinerStub)
  }

  class NodeViewStub extends Actor {
    def receive:Receive = {
      case _ =>
    }
  }

  object NodeViewStub {
    def props(): Props = Props(new NodeViewStub)
  }

  class NetworkControllerStub extends Actor {
    def receive: Receive = {
      case GetConnectedPeers => sender() ! connectedPeers
      case _ =>
    }
  }

  object NetworkControllerStub {
    def props(): Props = Props(new NetworkControllerStub)
  }

  class PeerManagerStub extends Actor {
    def receive: Receive = {
      case _ =>
    }
  }

  object PeerManagerStub {
    def props(): Props = Props(new PeerManagerStub)
  }

  class WalletActorStub extends Actor {

    import WalletActorStub.{walletBox10_10, walletBox20_30, walletBoxSpent21_31, walletTxs}

    private val prover: ErgoProvingInterpreter = defaultProver
    private val trackedAddresses: Seq[P2PKAddress] = prover.pubKeys.map(P2PKAddress.apply)

    def receive: Receive = {

      case _: InitWallet => sender() ! Success(WalletActorStub.mnemonic)

      case _: RestoreWallet => sender() ! Success(())

      case _: UnlockWallet => sender() ! Success(())

      case LockWallet => ()

      case GetBoxes(unspentOnly) =>
        val boxes = if (unspentOnly) {
          Seq(walletBox10_10, walletBox20_30)
        } else {
          Seq(walletBox10_10, walletBox20_30, walletBoxSpent21_31)
        }
        sender() ! boxes.sortBy(_.trackedBox.inclusionHeightOpt)

      case GetTransactions =>
        sender() ! walletTxs

      case DeriveKey(_) => sender() ! Success(WalletActorStub.address)

      case DeriveNextKey => sender() ! Success(WalletActorStub.path -> WalletActorStub.address)

      case ReadPublicKeys(from, until) =>
        sender() ! trackedAddresses.slice(from, until)

      case ReadBalances(chainStatus) =>
        sender ! RegistryIndex(0, WalletActorStub.balance(chainStatus), Map.empty, Seq.empty)

      case ReadTrackedAddresses =>
        sender ! trackedAddresses

      case GenerateTransaction(_, _) =>
        val input = ErgoTransactionGenerators.inputGen.sample.value
        val tx = ErgoTransaction(IndexedSeq(input), IndexedSeq(ergoBoxCandidateGen.sample.value))
        sender ! Success(tx)
    }
  }

  object WalletActorStub {

    val seed: String = "walletstub"
    val mnemonic: String = "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon"
    val path = DerivationPath(List(0, 1, 2), publicBranch = false)
    val address = P2PKAddress(proveDlogGen.sample.get)

    val walletBox10_10: WalletBox = WalletBox(
      TrackedBox(
        creationTxId = modifierIdGen.sample.get,
        creationOutIndex = 0,
        inclusionHeightOpt = Some(10),
        spendingTxIdOpt = Some(modifierIdGen.sample.get),
        spendingHeightOpt = None,
        box = ergoBoxGen.sample.get,
        certainty = BoxCertainty.Certain,
        applicationId = Constants.DefaultAppId
      ),
      confirmationsNumOpt = Some(10)
    )
    val walletBox20_30: WalletBox = walletBox10_10.copy(
      confirmationsNumOpt = Some(20),
      trackedBox = walletBox10_10.trackedBox.copy(inclusionHeightOpt = Some(30))
    )
    val walletBoxSpent21_31: WalletBox = walletBox10_10.copy(
      confirmationsNumOpt = Some(21),
      trackedBox = walletBox10_10.trackedBox.copy(
        inclusionHeightOpt = Some(31),
        spendingHeightOpt = Some(32),
        spendingTxIdOpt = Some(modifierIdGen.sample.get)
      )
    )
    val walletTxs: Seq[AugWalletTransaction] = Seq(augWalletTransactionGen.sample.get, augWalletTransactionGen.sample.get)

    def props(): Props = Props(new WalletActorStub)
    def balance(chainStatus: ChainStatus): Long = if (chainStatus.onChain) confirmedBalance else unconfirmedBalance
    def confirmedBalance: Long = 1L
    def unconfirmedBalance: Long = 2L
  }

  class WalletStub extends ErgoWalletReader {
    val walletActor: ActorRef = system.actorOf(WalletActorStub.props())
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
                      blocksToKeep: Int = 100,
                      epochLength: Int = 100000000,
                      useLastEpochs: Int = 10): ErgoHistory = {

    val miningDelay = 1.second
    val minimalSuffix = 2
    val poPowSettings = PoPowSettings(prove = false)
    val nodeSettings: NodeConfigurationSettings = NodeConfigurationSettings(stateType, verifyTransactions, blocksToKeep,
      minimalSuffix, mining = false, Constants.DefaultComplexityLimit, miningDelay, useExternalMiner = false, miningPubKeyHex = None,
      offlineGeneration = false, 200, 100000, 100000, 1.minute, 1000000, poPowSettings)
    val scorexSettings: ScorexSettings = null
    val testingSettings: TestingSettings = null
    val walletSettings: WalletSettings = null
    val chainSettings = settings.chainSettings.copy(epochLength = epochLength, useLastEpochs = useLastEpochs)

    val dir = createTempDir
    val fullHistorySettings: ErgoSettings = ErgoSettings(dir.getAbsolutePath, NetworkType.TestNet, chainSettings, testingSettings,
      nodeSettings, scorexSettings, walletSettings, CacheSettings.default)

    ErgoHistory.readOrGenerate(fullHistorySettings, timeProvider)
  }

  def syntacticallyValidModifier(history: HT): Header = {
    val bestTimestamp = history.bestHeaderOpt.map(_.timestamp + 1).getOrElse(timeProvider.time())

    powScheme.prove(
      history.bestHeaderOpt,
      Header.CurrentVersion,
      settings.chainSettings.initialNBits,
      ADDigest @@ Array.fill(HashLength + 1)(0.toByte),
      Digest32 @@ Array.fill(HashLength)(0.toByte),
      Digest32 @@ Array.fill(HashLength)(0.toByte),
      Math.max(timeProvider.time(), bestTimestamp),
      Digest32 @@ Array.fill(HashLength)(0.toByte),
      Array.fill(3)(0: Byte),
      defaultMinerSecretNumber
    ).value
  }
}
