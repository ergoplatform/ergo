package org.ergoplatform.nodeView.wallet

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import akka.testkit.TestProbe
import akka.util.Timeout
import org.ergoplatform.mining.DefaultFakePowScheme
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoNodeViewRef
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state._
import org.ergoplatform.settings.{Algos, Constants, ErgoSettings}
import org.ergoplatform.utils.{ChainGenerator, ErgoPropertyTest, ErgoTestHelpers}
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.scalacheck.Gen
import scorex.core.ModifierId
import scorex.core.NodeViewHolder.CurrentView
import scorex.core.NodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, LocallyGeneratedModifier}
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.SyntacticallySuccessfulModifier
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Digest32
import sigmastate.Values.Value
import sigmastate.interpreter.{ContextExtension, ProverResult}
import sigmastate.{NoProof, SBoolean, SigSerializer, Values}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, blocking}
import scala.reflect.ClassTag
import scala.util.Random


class ErgoWalletSpecification extends ErgoPropertyTest {

  property("successfully scans an offchain transaction") {

    implicit val actorSystem: ActorSystem = ActorSystem()
    val w: ErgoWallet = new ErgoWallet(null, null, settings)

    val bf0 = w.unconfirmedBalances()

    val bs0 = Await.result(bf0, 1.second)

    bs0.balance shouldBe 0
    bs0.assetBalances.isEmpty shouldBe true

    val af = w.walletAddresses()
    val as = Await.result(af, 1.second)

    val pubKey = as.head.asInstanceOf[P2PKAddress].pubkey

    def makeTx(balance: Int, script: Value[SBoolean.type] = pubKey) = {
      val input = Input(ADKey @@ Array.fill(32)(0: Byte), ProverResult(Array.emptyByteArray, ContextExtension(Map())))
      new ErgoTransaction(IndexedSeq(input), IndexedSeq(new ErgoBoxCandidate(balance, script)))
    }

    val balance1 = Random.nextInt(1000) + 1
    w.scanOffchain(makeTx(balance1))

    blocking(Thread.sleep(1000))

    val bf1 = w.unconfirmedBalances()
    val bs1 = Await.result(bf1, 1.second)
    bs1.balance shouldBe balance1
    bs1.assetBalances.isEmpty shouldBe true

    val balance2 = Random.nextInt(1000) + 1
    w.scanOffchain(makeTx(balance2))

    blocking(Thread.sleep(1000))

    val bf2 = w.unconfirmedBalances()
    val bs2 = Await.result(bf2, 1.second)
    bs2.balance shouldBe (balance1 + balance2)
    bs2.assetBalances.isEmpty shouldBe true

    w.watchFor(Pay2SAddress(Values.TrueLeaf))
    val balance3 = Random.nextInt(1000) + 1
    w.scanOffchain(makeTx(balance3, Values.TrueLeaf))

    Thread.sleep(1000)

    val bf3 = w.unconfirmedBalances()
    val bs3 = Await.result(bf3, 1.second)
    bs3.balance shouldBe (balance1 + balance2 + balance3)
    bs3.assetBalances.isEmpty shouldBe true

    //todo: enhance the test, e.g. add assets
  }

  property("Successfully scans an onchain transaction") {
    WithWalletFixture { fixture =>
      import fixture._
      val block = applyNextBlock
      wallet.scanPersistent(block)
      blocking(Thread.sleep(countOutputs(block) * scanningInterval + 500))
      val confirmedBalance = getConfirmedBalances.balance
      val sumBalance = sumOutputs(block)
      log.info(s"Confirmed balance $confirmedBalance")
      log.info(s"Sum balance: $sumBalance")
      confirmedBalance should be > 0L
      confirmedBalance shouldBe sumBalance
    }
  }

  property("Successfully does a rollback") {
    WithWalletFixture { fixture =>
      import fixture._
      val initialState = getCurrentState
      val versionId = scorex.core.versionToId(initialState.version)
      val initialHeight = getModifierHeight(versionId)

      val block = applyNextBlock
      wallet.scanPersistent(block)
      blocking(Thread.sleep(countOutputs(block) * scanningInterval + 500))
      val historyHeight = getHistoryHeight
      val confirmedBalance = getConfirmedBalances.balance
      wallet.rollback(initialState.version)
      blocking(Thread.sleep(100))
      val balanceAfterRollback = getConfirmedBalances.balance

      val sumBalance = sumOutputs(block)
      log.info(s"Initial height: $initialHeight")
      log.info(s"History height: $historyHeight")
      log.info(s"Confirmed balance $confirmedBalance")
      log.info(s"Sum balance: $sumBalance")
      log.info(s"Balance after rollback: $balanceAfterRollback")

      confirmedBalance should be > 0L
      confirmedBalance shouldBe sumBalance
      balanceAfterRollback shouldBe 0L
    }
  }

  property("successfully generates a transaction") {
    WithWalletFixture { fixture =>
      import fixture._
      val block = applyNextBlock
      wallet.scanPersistent(block)
      blocking(Thread.sleep(countOutputs(block) * scanningInterval + 500))
      val confirmedBalance = getConfirmedBalances.balance

      //pay out all the wallet balance:
      val req1 = PaymentRequest((Pay2SAddress(Values.FalseLeaf), confirmedBalance, Map(), Seq()))

      val tx1 = Await.result(wallet.generateTransaction(req1), awaitDuration).get
      tx1.outputs.size shouldBe 1
      tx1.outputs.head.value shouldBe confirmedBalance

      //change == 1:
      val req2 = PaymentRequest((Pay2SAddress(Values.FalseLeaf), confirmedBalance - 1, Map(), Seq()))

      val tx2 = Await.result(wallet.generateTransaction(req2), awaitDuration).get
      tx2.outputs.size shouldBe 2
      tx2.outputs.head.value shouldBe confirmedBalance - 1
      tx2.outputs(1).value shouldBe 1
    }
  }
}

class WithWalletFixture {

  object BlocksGenerator extends ErgoTestHelpers with ChainGenerator

  import BlocksGenerator._

  val nodeViewDir: java.io.File = createTempDir

  val settings: ErgoSettings = {
    val defaultSettings = ErgoSettings.read(None)
    defaultSettings.copy(
      directory = nodeViewDir.getAbsolutePath,
      chainSettings = defaultSettings.chainSettings.copy(powScheme = DefaultFakePowScheme),
      walletSettings = defaultSettings.walletSettings.copy(scanningInterval = 15.millis),
      nodeSettings = defaultSettings.nodeSettings.copy(
        stateType = StateType.Utxo,
        verifyTransactions = false,
        PoPoWBootstrap = false
      )
    )
  }

  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val ec: ExecutionContext = actorSystem.dispatcher
  val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(settings, timeProvider, emission)

  val testProbe = new TestProbe(actorSystem)
  implicit val sender: ActorRef = testProbe.ref

  implicit val timeout: Timeout = Timeout(5.seconds)
  val awaitDuration: Duration = timeout.duration + 1.second

  val wallet: ErgoWallet = dataFromCurrentView(_.vault)
  val addresses: Seq[ErgoAddress] = Await.result(wallet.walletAddresses(), awaitDuration)
  val pubKey: Value[SBoolean.type] = addresses.head.asInstanceOf[P2PKAddress].pubkey
  val scanningInterval: Long = settings.walletSettings.scanningInterval.toMillis
  val initialBlocks: Seq[ErgoFullBlock] = init()

  def init(): Seq[ErgoFullBlock] = {
    actorSystem.eventStream.subscribe(testProbe.ref, classOf[SyntacticallySuccessfulModifier[_]])
    genChain(3).map(applyBlock)
  }

  def getConfirmedBalances: BalancesSnapshot = Await.result(wallet.confirmedBalances(), awaitDuration)

  def getHistory: ErgoHistory = dataFromCurrentView(_.history)

  def getHistoryHeight: Int = dataFromCurrentView(_.history.headersHeight)

  def getModifierHeight(headerId: ModifierId): Option[Int] = dataFromCurrentView(_.history.heightOf(headerId))

  def getCurrentState: ErgoState[_] = dataFromCurrentView(_.state)

  def dataFromCurrentView[T: ClassTag](f: CurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool] => T): T = {
    val request = GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, T](f)
    Await.result((nodeViewHolderRef ? request).mapTo[T], awaitDuration)
  }

  def applyBlock(block: ErgoFullBlock): ErgoFullBlock = {
    nodeViewHolderRef ! LocallyGeneratedModifier(block.header)
    testProbe.expectMsgType[SyntacticallySuccessfulModifier[Header]]
    if (settings.nodeSettings.verifyTransactions) {
      nodeViewHolderRef ! LocallyGeneratedModifier(block.blockTransactions)
      nodeViewHolderRef ! LocallyGeneratedModifier(block.aDProofs.value)
      settings.nodeSettings.stateType match {
        case StateType.Digest =>
          testProbe.expectMsgType[SyntacticallySuccessfulModifier[ADProofs]]
          testProbe.expectMsgType[SyntacticallySuccessfulModifier[ADProofs]]
        case StateType.Utxo =>
          testProbe.expectMsgType[SyntacticallySuccessfulModifier[BlockTransactions]]
          testProbe.expectMsgType[SyntacticallySuccessfulModifier[BlockTransactions]]
      }
    }
    block
  }

  def applyNextBlock: ErgoFullBlock = applyBlock(makeNextBlock)

  def makeNextBlock: ErgoFullBlock = makeNextBlock(creationTxGen.sample.value)

  def makeNextBlock(txs: Seq[ErgoTransaction]): ErgoFullBlock = {
    val (state, parent) = dataFromCurrentView(v => (v.state, v.history.bestHeaderOpt))
    val (adProofs, stateDigest) = state.proofsForTransactions(txs).success.value
    val time = timeProvider.time()
    val extHash: Digest32 = Algos.hash(state.rootHash)
    DefaultFakePowScheme.proveBlock(parent, Constants.InitialNBits, stateDigest, adProofs, txs, time, extHash).value
  }

  private def creationTxGen: Gen[Seq[ErgoTransaction]] = {
    val noProof = ProverResult(SigSerializer.toBytes(NoProof), ContextExtension.empty)
    val input = Input(genesisEmissionBox.id, noProof)
    val boxCount = Gen.choose(1, 20).sample.getOrElse(5)
    val outputsGen = Gen.listOfN(boxCount, boxCandidateGen).map(_.toIndexedSeq)
    val txGen = outputsGen.map(outs => new ErgoTransaction(IndexedSeq(input), outs))
    val txCount = Gen.choose(1, 20).sample.getOrElse(5)
    Gen.listOfN(txCount, txGen)
  }

  private def boxCandidateGen: Gen[ErgoBoxCandidate] = {
    Gen.choose(10, 10000).map(v => new ErgoBoxCandidate(v, pubKey))
  }

  def sumOutputs(block: ErgoFullBlock): Long = outputs(block).map(_.value).sum

  def countOutputs(block: ErgoFullBlock): Int = outputs(block).size

  def outputs(block: ErgoFullBlock): Seq[ErgoBox] = {
    block.transactions.flatMap(_.outputs).filter(_.proposition == pubKey)
  }
}

object WithWalletFixture {
  def apply[T](test: WithWalletFixture => T): T = test(new WithWalletFixture)
}
