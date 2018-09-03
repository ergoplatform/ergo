package org.ergoplatform.nodeView.wallet

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import akka.testkit.TestProbe
import akka.util.Timeout
import org.ergoplatform.mining.DefaultFakePowScheme
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, ExtensionCandidate, Header}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoNodeViewRef
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state._
import org.ergoplatform.settings.{Constants, ErgoSettings}
import org.ergoplatform.utils.{ChainGenerator, ErgoPropertyTest, ErgoTestHelpers}
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.scalacheck.Gen
import scorex.core.ModifierId
import scorex.core.NodeViewHolder.CurrentView
import scorex.core.NodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, LocallyGeneratedModifier}
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.SyntacticallySuccessfulModifier
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Blake2b256
import sigmastate.Values.{ByteArrayConstant, Value}
import sigmastate.interpreter.{ContextExtension, ProverResult}
import sigmastate._

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, blocking}
import scala.util.Random


class ErgoWalletSpecification extends ErgoPropertyTest {
  private implicit val ergoAddressEncoder = new ErgoAddressEncoder(settings)

  property("successfully scans an offchain transaction") {
    withWalletFixture { fixture =>
      import fixture._

      val bs0 = getUnconfirmedBalances
      bs0.balance shouldBe 0
      bs0.assetBalances.isEmpty shouldBe true

      val balance1 = Random.nextInt(1000) + 1
      wallet.scanOffchain(makeTx(balance1))

      blocking(Thread.sleep(1000))

      val bs1 = getUnconfirmedBalances
      bs1.balance shouldBe balance1
      bs1.assetBalances.isEmpty shouldBe true

      val balance2 = Random.nextInt(1000) + 1
      wallet.scanOffchain(makeTx(balance2))

      blocking(Thread.sleep(1000))

      val bs2 = getUnconfirmedBalances
      bs2.balance shouldBe (balance1 + balance2)
      bs2.assetBalances.isEmpty shouldBe true

      wallet.watchFor(Pay2SAddress(Values.TrueLeaf))
      val balance3 = Random.nextInt(1000) + 1
      wallet.scanOffchain(makeTx(balance3, Values.TrueLeaf))

      blocking(Thread.sleep(1000))

      val bs3 = getUnconfirmedBalances
      bs3.balance shouldBe (balance1 + balance2 + balance3)
      bs3.assetBalances.isEmpty shouldBe true

      //todo: enhance the test, e.g. add assets
    }
  }

  property("Successfully spends boxes") {
    withWalletFixture { fixture =>
      import fixture._
      val tx = creationTxGen().sample.value
      wallet.scanOffchain(tx)
      val sumBalance = tx.outputs.map(_.value).sum
      blocking(Thread.sleep(scanTime(tx)))
      val unconfirmedBalance = getUnconfirmedBalances.balance
      unconfirmedBalance shouldEqual sumBalance

      val boxesToSpend = someOf(tx.outputs).sample.value
      val balanceToSpend = boxesToSpend.map(_.value).sum
      val balanceToReturn = Gen.choose(1, balanceToSpend - 1).sample.value
      val outputs = IndexedSeq(makeOutput(balanceToReturn, pubKey), makeOutput(balanceToSpend - balanceToReturn))
      val spendingTx = new ErgoTransaction(makeInputs(boxesToSpend, proofBytes), outputs)
      wallet.scanOffchain(spendingTx)
      blocking(Thread.sleep(scanTime(tx)))
      val balanceAfterSpending = getUnconfirmedBalances.balance
      log.info(s"Unconfirmed balance: $unconfirmedBalance")
      log.info(s"Balance to spent: $balanceToSpend")
      log.info(s"Balance to return back: $balanceToReturn")
      balanceAfterSpending shouldEqual (unconfirmedBalance - balanceToSpend + balanceToReturn)
    }
  }

  property("Successfully scans an onchain transaction") {
    withWalletFixture { fixture =>
      import fixture._
      val block = applyNextBlock(pubKey)
      wallet.scanPersistent(block)
      blocking(Thread.sleep(scanDuration(block)))
      val confirmedBalance = getConfirmedBalances.balance
      val sumBalance = sumOutputs(block)
      log.info(s"Confirmed balance $confirmedBalance")
      log.info(s"Sum balance: $sumBalance")
      confirmedBalance should be > 0L
      confirmedBalance shouldBe sumBalance
    }
  }

  property("offchain transaction becomes onchain") {
    withWalletFixture { fixture =>
      import fixture._
      val block = applyNextBlock(pubKey)
      val sumBalance = sumOutputs(block)

      block.transactions.foreach{tx =>
        wallet.scanOffchain(tx)
      }
      blocking(Thread.sleep(scanDuration(block)))
      val unconfirmedBalance = getUnconfirmedBalances.balance
      unconfirmedBalance shouldBe sumBalance

      wallet.scanPersistent(block)
      blocking(Thread.sleep(scanDuration(block)))
      val confirmedBalance = getConfirmedBalances.balance
      log.info(s"Confirmed balance $confirmedBalance")
      log.info(s"Sum balance: $sumBalance")
      confirmedBalance should be > 0L
      confirmedBalance shouldBe sumBalance
      confirmedBalance shouldBe unconfirmedBalance

      getUnconfirmedBalances.balance shouldBe 0L
    }
  }

  property("Successfully does a rollback") {
    withWalletFixture { fixture =>
      import fixture._
      val initialState = getCurrentState
      val versionId = scorex.core.versionToId(initialState.version)
      val initialHeight = getModifierHeight(versionId)

      val block = applyNextBlock(pubKey)
      wallet.scanPersistent(block)
      blocking(Thread.sleep(scanDuration(block)))
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
    withWalletFixture { fixture =>
      import fixture._
      val block = applyNextBlock(pubKey)
      wallet.scanPersistent(block)
      blocking(Thread.sleep(scanDuration(block)))
      val confirmedBalance = getConfirmedBalances.balance

      //pay out all the wallet balance:
      val req1 = PaymentRequest(Pay2SAddress(Values.FalseLeaf), confirmedBalance, None, None)

      val tx1 = Await.result(wallet.generateTransaction(Seq(req1)), awaitDuration).get
      tx1.outputs.size shouldBe 1
      tx1.outputs.head.value shouldBe confirmedBalance

      //change == 1:
      val req2 = PaymentRequest(Pay2SAddress(Values.FalseLeaf), confirmedBalance - 1, None, None)

      val tx2 = Await.result(wallet.generateTransaction(Seq(req2)), awaitDuration).get
      tx2.outputs.size shouldBe 2
      tx2.outputs.head.value shouldBe confirmedBalance - 1
      tx2.outputs(1).value shouldBe 1
    }
  }

  property("watchFor") {
    withWalletFixture { fixture =>
      import fixture._

      val preimage = ByteArrayConstant("hello world".getBytes("UTF-8"))
      val hash = Blake2b256(preimage.value)
      val p2s = Pay2SAddress(EQ(CalcBlake2b256(preimage), hash))

      val initialState = getCurrentState

      val block = applyNextBlock(p2s.script)
      wallet.scanPersistent(block)
      blocking(Thread.sleep(scanDuration(block)))
      val confirmedBalance = getConfirmedBalances.balance

      confirmedBalance should be < sumOutputs(block)

      wallet.rollback(initialState.version)
      blocking(Thread.sleep(100))

      wallet.watchFor(p2s)

      wallet.scanPersistent(block)
      blocking(Thread.sleep(scanDuration(block)))
      val confirmedBalance2 = getConfirmedBalances.balance
      confirmedBalance2 shouldBe sumOutputs(block)
    }
  }

  def withWalletFixture[T](test: WalletFixture => T): T = {
    new WalletFixture().runAndClose(test)
  }
}

class WalletFixture {

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

  val wallet: ErgoWallet = getCurrentView.vault
  def addresses: Seq[ErgoAddress] = Await.result(wallet.trackedAddresses(), awaitDuration)
  val defaultAddress: P2PKAddress = addresses.head.asInstanceOf[P2PKAddress]
  val pubKey: Value[SBoolean.type] = defaultAddress.pubkey
  val proofBytes: Array[Byte] = defaultAddress.contentBytes

  val scanningInterval: Long = settings.walletSettings.scanningInterval.toMillis
  val initialBlocks: Seq[ErgoFullBlock] = init()

  def init(): Seq[ErgoFullBlock] = {
    actorSystem.eventStream.subscribe(testProbe.ref, classOf[SyntacticallySuccessfulModifier[_]])
    genChain(3).map(applyBlock)
  }

  def runAndClose[T](test: WalletFixture => T): T = {
    val result = test(this)
    close()
    result
  }

  def close(): Unit = {
    actorSystem.terminate()
  }

  def getConfirmedBalances: BalancesSnapshot = Await.result(wallet.confirmedBalances(), awaitDuration)

  def getUnconfirmedBalances: BalancesSnapshot = Await.result(wallet.unconfirmedBalances(), awaitDuration)

  def getHistory: ErgoHistory = getCurrentView.history

  def getHistoryHeight: Int = getHistory.headersHeight

  def getModifierHeight(headerId: ModifierId): Option[Int] = getHistory.heightOf(headerId)

  def getCurrentState: ErgoState[_] = getCurrentView.state

  type CurView = CurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool]
  def getCurrentView: CurView = {
    val request = GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, CurView](view => view)
    Await.result((nodeViewHolderRef ? request).mapTo[CurView], awaitDuration)
  }

  def applyBlock(block: ErgoFullBlock): ErgoFullBlock = {
    nodeViewHolderRef ! LocallyGeneratedModifier(block.header)
    testProbe.expectMsgType[SyntacticallySuccessfulModifier[Header]]
    if (settings.nodeSettings.verifyTransactions) {
      nodeViewHolderRef ! LocallyGeneratedModifier(block.blockTransactions)
      nodeViewHolderRef ! LocallyGeneratedModifier(block.adProofs.value)
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

  def applyNextBlock(injectedScript: Value[SBoolean.type]): ErgoFullBlock = applyBlock(makeNextBlock(injectedScript))

  def makeNextBlock(injectedScript: Value[SBoolean.type]): ErgoFullBlock =
    makeNextBlock(creationTxSeqGen(Some(arbScriptBoxCandidateGen(injectedScript))).sample.value)

  def makeNextBlock(txs: Seq[ErgoTransaction]): ErgoFullBlock = {
    val currentView = getCurrentView
    val state =  currentView.state
    val parent = currentView.history.bestHeaderOpt
    val (adProofs, stateDigest) = state.proofsForTransactions(txs).success.value
    val time = timeProvider.time()
    val ext = ExtensionCandidate(Seq(), Seq())
    DefaultFakePowScheme.proveBlock(parent, Constants.InitialNBits, stateDigest, adProofs, txs, time, ext).value
  }

  def creationTxSeqGen(neededOutputGen: Option[Gen[ErgoBoxCandidate]] = None): Gen[Seq[ErgoTransaction]] = {
    val txCount = Gen.choose(1, 20).sample.getOrElse(5)
    Gen.listOfN(txCount, creationTxGen(neededOutputGen))
  }

  def creationTxGen(neededOutputGen: Option[Gen[ErgoBoxCandidate]] = None): Gen[ErgoTransaction] = {
    val noProof = ProverResult(SigSerializer.toBytes(NoProof), ContextExtension.empty)
    val input = Input(genesisEmissionBox.id, noProof)
    val boxCount = Gen.choose(1, 20).sample.getOrElse(5)
    val outputsGen = Gen.listOfN(boxCount, p2pkBoxCandidateGen)
      .map(_.toIndexedSeq ++ neededOutputGen.flatMap(_.sample).toIndexedSeq)
    outputsGen.map(outs => new ErgoTransaction(IndexedSeq(input), outs))
  }

  def someOf[T](items: Seq[T]): Gen[Seq[T]] = {
    val count = Gen.choose(1, items.size).sample.getOrElse(1)
    Gen.listOfN(count, Gen.oneOf(items)).map(_.distinct)
  }

  private def p2pkBoxCandidateGen: Gen[ErgoBoxCandidate] = arbScriptBoxCandidateGen(pubKey)

  def arbScriptBoxCandidateGen(script: Value[SBoolean.type]): Gen[ErgoBoxCandidate] = {
    Gen.choose(10, 10000).map(v => new ErgoBoxCandidate(v, script))
  }

  def makeTx(balance: Int, script: Value[SBoolean.type] = pubKey): ErgoTransaction = {
    val input = Input(ADKey @@ Array.fill(32)(0: Byte), ProverResult(Array.emptyByteArray, ContextExtension(Map.empty)))
    new ErgoTransaction(IndexedSeq(input), IndexedSeq(makeOutput(balance, script)))
  }

  def makeInputs(boxesToSpend: Seq[ErgoBox], proof: Array[Byte]): IndexedSeq[Input] = {
    boxesToSpend.map { box =>
      Input(box.id, ProverResult(proofBytes, ContextExtension.empty))
    }.toIndexedSeq
  }

  def makeOutput(balance: Long, script: Value[SBoolean.type] = Values.TrueLeaf): ErgoBoxCandidate = {
    new ErgoBoxCandidate(balance, script)
  }

  def sumOutputs(block: ErgoFullBlock): Long = outputs(block).map(_.value).sum

  def countOutputs(block: ErgoFullBlock): Int = outputs(block).size

  def outputs(block: ErgoFullBlock): Seq[ErgoBox] =  block.transactions.flatMap(_.outputs)

  def scanDuration(block: ErgoFullBlock): Long = countOutputs(block) * scanningInterval + 500

  def scanTime(tx: ErgoTransaction): Long = tx.outputs.size * 100 + 300

}
