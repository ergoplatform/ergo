package org.ergoplatform.utils

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.StatusReply
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.P2PKAddress
import org.ergoplatform.mining.CandidateGenerator.Candidate
import org.ergoplatform.mining.{AutolykosSolution, CandidateGenerator, ErgoMiner, WorkMessage}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetDataFromHistory, GetReaders, Readers}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.mempool.ErgoMemPool.{ProcessingOutcome, SortingOption}
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.nodeView.state.{DigestState, ErgoStateContext, StateType}
import org.ergoplatform.nodeView.wallet.ErgoWalletActor._
import org.ergoplatform.nodeView.wallet.ErgoWalletService.DeriveNextKeyResult
import org.ergoplatform.nodeView.wallet._
import org.ergoplatform.nodeView.wallet.persistence.WalletDigest
import org.ergoplatform.nodeView.wallet.scanning.Scan
import org.ergoplatform.sanity.ErgoSanity.HT
import org.ergoplatform.settings.Constants.HashLength
import org.ergoplatform.settings._
import org.ergoplatform.utils.generators.{ChainGenerator, ErgoGenerators, ErgoTransactionGenerators}
import org.ergoplatform.wallet.interface4j.SecretString
import org.ergoplatform.wallet.Constants.{PaymentsScanId, ScanId}
import org.ergoplatform.wallet.boxes.{ChainStatus, TrackedBox}
import org.ergoplatform.wallet.interpreter.ErgoProvingInterpreter
import org.ergoplatform.wallet.mnemonic.Mnemonic
import org.ergoplatform.wallet.secrets.{DerivationPath, ExtendedSecretKey}
import org.ergoplatform.wallet.utils.TestFileUtils
import org.scalacheck.Gen
import scorex.core.app.Version
import scorex.core.network.NetworkController.ReceivableMessages.GetConnectedPeers
import scorex.core.network.peer.PeerManager.ReceivableMessages.{GetAllPeers, GetBlacklistedPeers}
import scorex.core.network.{Handshake, PeerSpec}
import scorex.core.settings.ScorexSettings
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import scorex.db.ByteArrayWrapper
import scorex.util.Random
import sigmastate.basics.DLogProtocol.{DLogProverInput, ProveDlog}

import scala.collection.mutable
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

trait Stubs extends ErgoGenerators with ErgoTestHelpers with ChainGenerator with TestFileUtils {

  implicit val system: ActorSystem

  val chain: Seq[ErgoFullBlock] = genChain(6)

  val history: HT = applyChain(generateHistory(), chain)

  val digestState: DigestState = {
    boxesHolderGen.map(WrappedUtxoState(_, createTempDir, None, parameters, settings)).map { wus =>
      DigestState.create(Some(wus.version), Some(wus.rootHash), createTempDir, stateConstants)
    }
  }.sample.value

  val utxoSettings: ErgoSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(stateType = StateType.Utxo))

  val utxoState: WrappedUtxoState =
    boxesHolderGen.map(WrappedUtxoState(_, createTempDir, None, parameters, utxoSettings)).sample.value

  lazy val wallet = new WalletStub

  val txs: Seq[ErgoTransaction] = validTransactionsFromBoxHolder(boxesHolderGen.sample.get)._1
  val memPool: ErgoMemPool = ErgoMemPool.empty(settings).put(txs.map(tx => UnconfirmedTransaction(tx, None))).get

  val digestReaders = Readers(history, digestState, memPool, wallet)

  val utxoReaders = Readers(history, utxoState, memPool, wallet)

  val protocolVersion = Version("1.1.1")

  val peerSpec: PeerSpec = defaultPeerSpec.copy(protocolVersion = protocolVersion)

  val connectedPeers: Seq[Handshake] = Seq(
    Handshake(peerSpec.copy(nodeName = "first"), System.currentTimeMillis() - 100),
    Handshake(peerSpec.copy(nodeName = "second"), System.currentTimeMillis() + 100)
  )

  val blacklistedPeers: Seq[String] = Seq("4.4.4.4:1111", "8.8.8.8:2222")

  val pk: ProveDlog = DLogProverInput(BigIntegers.fromUnsignedByteArray(Random.randomBytes(32))).publicImage
  val externalWorkMessage = WorkMessage(Array.fill(32)(2: Byte), BigInt(9999), None, pk, None)

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
      case CandidateGenerator.GenerateCandidate(_, reply) =>
        if (reply) {
          val candidate = Candidate(null, externalWorkMessage, Seq.empty) // API does not use CandidateBlock
          sender() ! StatusReply.success(candidate)
        }
      case _: AutolykosSolution => sender() ! StatusReply.success(())
      case ErgoMiner.ReadMinerPk => sender() ! StatusReply.success(pk)
    }
  }

  object MinerStub {
    def props(): Props = Props(new MinerStub)
  }

  class NodeViewStub extends Actor {
    def receive: Receive = {
      case LocallyGeneratedTransaction(utx) =>
        sender() ! new ProcessingOutcome.Accepted(utx, System.currentTimeMillis())
      case _ =>
    }
  }

  class FailingNodeViewStub extends Actor {
    def receive: Receive = {
      case LocallyGeneratedTransaction(_) =>
        sender() ! new ProcessingOutcome.Invalidated(new Error("Transaction invalid"), System.currentTimeMillis())
      case _ =>
    }
  }

  object NodeViewStub {
    def props(): Props = Props(new NodeViewStub)
    def failingProps(): Props = Props(new FailingNodeViewStub)
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

    import WalletActorStub._

    private val prover: ErgoProvingInterpreter = defaultProver
    private val trackedAddresses: Seq[P2PKAddress] = prover.hdPubKeys.map(epk => P2PKAddress(epk.key))

    private val apps = mutable.Map[ScanId, Scan]()

    private val ergoWalletService = new ErgoWalletServiceImpl(settings)

    def receive: Receive = {

      case _: InitWallet => sender() ! Success(SecretString.create(WalletActorStub.mnemonic))

      case _: RestoreWallet => sender() ! Success(())

      case _: UnlockWallet => sender() ! Success(())

      case LockWallet => ()

      case RescanWallet(_) => sender ! Success(())

      case GetWalletStatus => sender() ! WalletStatus(true, true, None, ErgoHistory.GenesisHeight, error = None)

      case _: CheckSeed => sender() ! true

      case GetWalletBoxes(unspentOnly, _) =>
        val boxes = if (unspentOnly) {
          Seq(walletBox10_10, walletBox20_30)
        } else {
          Seq(walletBox10_10, walletBox20_30, walletBoxSpent21_31)
        }
        sender() ! boxes.sortBy(_.trackedBox.inclusionHeightOpt)

      case GetScanTransactions(scanId, includeUnconfirmed) =>
        if (includeUnconfirmed) {
          sender() ! ScanRelatedTxsResponse(walletTxsForScan(scanId, includeUnconfirmed = true))
        } else {
          sender() ! ScanRelatedTxsResponse(walletTxsForScan(scanId))
        }

      case GetTransactions =>
        sender() ! walletTxs

      case DeriveKey(_) => sender() ! Success(WalletActorStub.address)

      case DeriveNextKey => sender() !
        DeriveNextKeyResult(Success((WalletActorStub.path, WalletActorStub.address, WalletActorStub.secretKey)))

      case ReadPublicKeys(from, until) =>
        sender() ! trackedAddresses.slice(from, until)

      case ReadBalances(chainStatus) =>
        sender() ! WalletDigest(0, WalletActorStub.balance(chainStatus), mutable.WrappedArray.empty)

      case AddScan(req) =>
        val scanId = ScanId @@ (apps.lastOption.map(_._1).getOrElse(100: Short) + 1).toShort
        val app = req.toScan(scanId)
        apps += scanId -> app.get
        sender() ! AddScanResponse(app)

      case RemoveScan(scanId) =>
        val res: Try[Unit] = if(apps.exists(_._1 == scanId)) {
          apps.remove(scanId)
          Success(())
        } else {
          Failure(new Exception(""))
        }
        sender() ! RemoveScanResponse(res)

      case GetScanUnspentBoxes(_, considerUnconfirmed) =>
        val res = if(considerUnconfirmed) {
          Seq(walletBoxN_N, walletBox10_10, walletBox20_30, walletBoxSpent21_31)
        } else {
          Seq(walletBox10_10, walletBox20_30, walletBoxSpent21_31)
        }
        sender() ! res

      case GetScanSpentBoxes(_) =>
        sender() ! Seq(walletBox10_10, walletBox20_30, walletBoxSpent21_31)

      case StopTracking(_, _) =>
        sender() ! StopTrackingResponse(Success(()))

      case ReadScans =>
        sender() ! ReadScansResponse(apps.values.toSeq)

      case GenerateTransaction(_, _, _, _) =>
        val input = ErgoTransactionGenerators.inputGen.sample.value
        val tx = ErgoTransaction(IndexedSeq(input), IndexedSeq(ergoBoxCandidateGen.sample.value))
        sender() ! Success(tx)

      case SignTransaction(tx, secrets, hints, boxesToSpendOpt, dataBoxesOpt) =>
        val sc = ErgoStateContext.empty(stateConstants, parameters)
        sender() ! ergoWalletService.signTransaction(Some(prover), tx, secrets, hints, boxesToSpendOpt, dataBoxesOpt, parameters, sc) { boxId =>
          utxoState.versionedBoxHolder.get(ByteArrayWrapper(boxId))
        }
    }
  }

  object WalletActorStub {
    val mnemonic = "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon"
    val path = DerivationPath(List(0, 1, 2), publicBranch = false)
    val secretKey = ExtendedSecretKey.deriveMasterKey(Mnemonic.toSeed(SecretString.create(mnemonic))).derive(path)
    val address = P2PKAddress(proveDlogGen.sample.get)

    val walletBoxN_N: WalletBox = WalletBox(
      TrackedBox(
        creationTxId = modifierIdGen.sample.get,
        creationOutIndex = 0,
        inclusionHeightOpt = None,
        spendingTxIdOpt = Some(modifierIdGen.sample.get),
        spendingHeightOpt = None,
        box = ergoBoxGen.sample.get,
        scans = Set(PaymentsScanId)
      ),
      confirmationsNumOpt = None
    )

    val walletBox10_10: WalletBox = WalletBox(
      TrackedBox(
        creationTxId = modifierIdGen.sample.get,
        creationOutIndex = 0,
        inclusionHeightOpt = Some(10),
        spendingTxIdOpt = Some(modifierIdGen.sample.get),
        spendingHeightOpt = None,
        box = ergoBoxGen.sample.get,
        scans = Set(PaymentsScanId)
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
    val walletTxs: Seq[AugWalletTransaction] =
      Gen.listOf(augWalletTransactionGen).sample.get

    def walletTxsForScan(scanId: ScanId, includeUnconfirmed: Boolean = false): Seq[AugWalletTransaction] =
      Gen.listOf(augWalletTransactionForScanGen(scanId, includeUnconfirmed)).sample.get

    def props(): Props = Props(new WalletActorStub)

    def balance(chainStatus: ChainStatus): Long = if (chainStatus.onChain) confirmedBalance else unconfirmedBalance

    def confirmedBalance: Long = 1L

    def unconfirmedBalance: Long = 2L
  }

  class WalletStub extends ErgoWalletReader {
    val walletActor: ActorRef = system.actorOf(WalletActorStub.props())
  }


  class DigestReadersStub extends Actor {
    def receive: PartialFunction[Any, Unit] = {
      case GetReaders => sender() ! digestReaders
      case GetDataFromHistory(f) => sender() ! f(history)
    }
  }

  object DigestReadersStub {
    def props(): Props = Props(new DigestReadersStub)
  }

  class UtxoReadersStub extends Actor {
    def receive: PartialFunction[Any, Unit] = {
      case GetReaders => sender() ! utxoReaders
      case GetDataFromHistory(f) => sender() ! f(history)
    }
  }

  object UtxoReadersStub {
    def props(): Props = Props(new UtxoReadersStub)
  }


  lazy val digestReadersRef: ActorRef = system.actorOf(DigestReadersStub.props())
  lazy val utxoReadersRef: ActorRef = system.actorOf(UtxoReadersStub.props())

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

    val minimalSuffix = 2
    val txCostLimit     = initSettings.nodeSettings.maxTransactionCost
    val txSizeLimit      = initSettings.nodeSettings.maxTransactionSize
    val nodeSettings: NodeConfigurationSettings = NodeConfigurationSettings(stateType, verifyTransactions, blocksToKeep,
      PoPoWBootstrap, minimalSuffix, mining = false, txCostLimit, txSizeLimit, useExternalMiner = false,
      internalMinersCount = 1, internalMinerPollingInterval = 1.second,miningPubKeyHex = None,
      offlineGeneration = false, 200, 5.minutes, 100000, 1.minute, mempoolSorting = SortingOption.FeePerByte,
      rebroadcastCount = 200, 1000000, 100, adProofsSuffixLength = 112*1024
)
    val scorexSettings: ScorexSettings = null
    val walletSettings: WalletSettings = null
    val chainSettings = settings.chainSettings.copy(epochLength = epochLength, useLastEpochs = useLastEpochs)

    val dir = createTempDir
    val fullHistorySettings: ErgoSettings = ErgoSettings(dir.getAbsolutePath, NetworkType.TestNet, chainSettings,
      nodeSettings, scorexSettings, walletSettings, settings.cacheSettings)

    ErgoHistory.readOrGenerate(fullHistorySettings, timeProvider)
  }

  def syntacticallyValidModifier(history: HT): Header = {
    val bestTimestamp = history.bestHeaderOpt.map(_.timestamp + 1).getOrElse(timeProvider.time())

    powScheme.prove(
      history.bestHeaderOpt,
      Header.InitialVersion,
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
