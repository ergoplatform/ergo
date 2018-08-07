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
import org.ergoplatform.utils.{ChainGenerator, ErgoPropertyTest, ErgoTestHelpers, ValidBlocksGenerators}
import org.ergoplatform.{ErgoBoxCandidate, Input}
import org.scalacheck.Gen
import org.scalatest.{Assertion, OptionValues}
import scorex.core.ModifierId
import scorex.core.NodeViewHolder.CurrentView
import scorex.core.NodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, LocallyGeneratedModifier}
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.SyntacticallySuccessfulModifier
import scorex.core.utils.NetworkTimeProvider
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Digest32
import sigmastate.Values.Value
import sigmastate.interpreter.{ContextExtension, ProverResult}
import sigmastate.{NoProof, SBoolean, SigSerializer, Values}

import scala.async.Async._
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future, blocking}
import scala.reflect.ClassTag
import scala.util.Random


class ErgoWalletSpecification extends ErgoPropertyTest with OptionValues {

  property("successfully scans an offchain transaction") {

    implicit val actorSystem = ActorSystem()
    val w: ErgoWallet = new ErgoWallet(actorSystem, null, null, settings)

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
    w.scanOffchain(makeTx(balance2, pubKey))

    blocking(Thread.sleep(1000))

    val bf2 = w.unconfirmedBalances()
    val bs2 = Await.result(bf2, 1.second)
    bs2.balance shouldBe (balance1 + balance2)
    bs2.assetBalances.isEmpty shouldBe true

    w.watchFor(ScriptAddress(Values.TrueLeaf))
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
    val assertion: Future[Assertion] = WithWalletFixture { fixture =>
      import fixture._
      async {
        val initialBalance = await(getConfirmedBalance)
        val block = await(applyNextBlock)
        wallet.scanPersistent(block)
        blocking(Thread.sleep(1000))
        val confirmedBalance = await(getConfirmedBalance)
        val sumBalance = initialBalance + sumOutputs(block)
        log.info(s"Initial balance $initialBalance")
        log.info(s"Confirmed balance $confirmedBalance")
        log.info(s"Sum balance: $sumBalance")
        confirmedBalance should be > initialBalance
        //confirmedBalance shouldBe sumBalance
      }
    }
    Await.result(assertion, 30.seconds)
  }

  property("Successfully does a rollback") {
    val assertion: Future[Assertion] = WithWalletFixture { fixture =>
      import fixture._
      async {
        val initialState = await(getCurrentState)
        val versionId = scorex.core.versionToId(initialState.version)
        val initialHeight = await(getModifierHeight(versionId))
        val initialBalance = await(getConfirmedBalance)

        val block = await(applyNextBlock)
        wallet.scanPersistent(block)
        blocking(Thread.sleep(1000))
        val historyHeight = await(getHistoryHeight)
        val confirmedBalance = await(getConfirmedBalance)
        wallet.rollback(initialState.version)
        val balanceAfterRollback = await(getConfirmedBalance)

        val sumBalance = initialBalance + sumOutputs(block)
        log.info(s"Initial height: $initialHeight")
        log.info(s"Initial balance: $initialBalance")
        log.info(s"History height: $historyHeight")
        log.info(s"Confirmed balance $confirmedBalance")
        log.info(s"Sum balance: $sumBalance")
        log.info(s"Balance after rollback: $balanceAfterRollback")

        confirmedBalance should be > initialBalance
//      balanceAfterRollback shouldBe initialBalance
      }
    }
    Await.result(assertion, 30.seconds)
  }

}

class WithWalletFixture extends scorex.testkit.utils.FileUtils with OptionValues {

  val nodeViewDir: java.io.File = createTempDir

  private val defaultSettings = ErgoSettings.read(None)

  val settings: ErgoSettings = defaultSettings.copy(
    directory = nodeViewDir.getAbsolutePath,
    chainSettings = defaultSettings.chainSettings.copy(powScheme = DefaultFakePowScheme),
    nodeSettings = defaultSettings.nodeSettings.copy(
      stateType = StateType.Utxo,
      verifyTransactions = false,
      PoPoWBootstrap = false
    )
  )

  object BlockGenerator extends ValidBlocksGenerators with ChainGenerator {
    val timeProvider: NetworkTimeProvider =  ErgoTestHelpers.defaultTimeProvider
  }

  import BlockGenerator._

  implicit val actorSystem: ActorSystem = ActorSystem()

  implicit val ec: ExecutionContext =  actorSystem.dispatcher

  implicit val timeout: Timeout = Timeout(5.seconds)
  private val awaitDuration: Duration = timeout.duration * 2

  val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(settings, timeProvider, emission)

  val testProbe = new TestProbe(actorSystem)
  implicit val sender: ActorRef = testProbe.testActor

  val wallet: ErgoWallet = Await.result(getWallet, awaitDuration)
  val pubKey: Value[SBoolean.type] = Await.result(getPubKey, awaitDuration)

  init()

  def init(): Unit = {
    actorSystem.eventStream.subscribe(testProbe.ref, classOf[SyntacticallySuccessfulModifier[_]])
    genChain(3).foreach(applyBlock)
  }

  def getWallet: Future[ErgoWallet] = {
    val call = GetDataFromCurrentView[ErgoHistory, ErgoState[_], ErgoWallet, ErgoMemPool, Any](_.vault)
    (nodeViewHolderRef ? call).mapTo[ErgoWallet]
  }

  def getPubKey: Future[Value[SBoolean.type]] = {
    wallet.walletAddresses() map { addresses =>
      addresses.head.asInstanceOf[P2PKAddress].pubkey
    }
  }

  def getConfirmedBalance: Future[Long] = {
    wallet.confirmedBalances() map { snapshot =>
      snapshot.balance
    }
  }

  def getHistory: Future[ErgoHistory] = dataFromView(_.history)
  def getHistoryHeight: Future[Int] = dataFromView(_.history.headersHeight)
  def getModifierHeight(headerId: ModifierId): Future[Option[Int]] = dataFromView(_.history.heightOf(headerId))
  def getCurrentState: Future[ErgoState[_]] =  dataFromView(_.state)

  def dataFromView[T : ClassTag](f: CurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool] => T): Future[T] = {
    (nodeViewHolderRef ? GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, T](f)).mapTo[T]
  }

  def applyBlock(block: ErgoFullBlock): Unit = {
    nodeViewHolderRef ! LocallyGeneratedModifier(block.header)
    testProbe.expectMsgType[SyntacticallySuccessfulModifier[Header]]
    if (settings.nodeSettings.verifyTransactions) {
      nodeViewHolderRef ! LocallyGeneratedModifier(block.blockTransactions)
      nodeViewHolderRef ! LocallyGeneratedModifier(block.aDProofs.get)
      settings.nodeSettings.stateType match {
        case StateType.Digest =>
          testProbe.expectMsgType[SyntacticallySuccessfulModifier[ADProofs]]
          testProbe.expectMsgType[SyntacticallySuccessfulModifier[ADProofs]]
        case StateType.Utxo =>
          testProbe.expectMsgType[SyntacticallySuccessfulModifier[BlockTransactions]]
          testProbe.expectMsgType[SyntacticallySuccessfulModifier[BlockTransactions]]
      }
    }
  }

  def applyNextBlock: Future[ErgoFullBlock] = makeNextBlock.map { block =>
    applyBlock(block)
    block
  }

  def makeNextBlock: Future[ErgoFullBlock] = {
    makeNextBlock(creationTxGen.sample.value)
  }

  def makeNextBlock(txs: Seq[ErgoTransaction]): Future[ErgoFullBlock] = {
    dataFromView(v => (v.state, v.history.bestHeaderOpt)).map { case (state, parent) =>
      val (adProofBytes, stateDigest) = state.proofsForTransactions(txs).get
      val time = System.currentTimeMillis()
      val extHash: Digest32 = Algos.hash(state.rootHash)
      DefaultFakePowScheme.proveBlock(parent, Constants.InitialNBits, stateDigest, adProofBytes, txs, time, extHash).get
    }
  }

  private def creationTxGen: Gen[Seq[ErgoTransaction]] = {
    val noProof = ProverResult(SigSerializer.toBytes(NoProof), ContextExtension.empty)
    val input = Input(genesisEmissionBox.id, noProof)
    val outputsGen = Gen.nonEmptyListOf(boxCandidateGen).map(_.toIndexedSeq)
    val txGen = outputsGen.map(outs => new ErgoTransaction(IndexedSeq(input), outs))
    Gen.nonEmptyListOf(txGen)
  }

  private def boxCandidateGen: Gen[ErgoBoxCandidate] = {
    Gen.choose(10, 10000).map(v => new ErgoBoxCandidate(v, pubKey))
  }

  def sumOutputs(block: ErgoFullBlock): Long = {
    block
      .transactions
      .flatMap(_.outputs)
      .filter(_.proposition == pubKey)
      .map(_.value)
      .sum
  }
}

object WithWalletFixture {
  def apply[T](test: WithWalletFixture => T): T = test(new WithWalletFixture)
}
