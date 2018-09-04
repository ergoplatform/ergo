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
import org.ergoplatform.settings.{Algos, Constants, ErgoSettings}
import org.ergoplatform.utils.{ChainGenerator, ErgoPropertyTest, ErgoTestHelpers}
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.scalacheck.Gen
import org.scalatest.Inspectors
import scorex.core.NodeViewHolder.CurrentView
import scorex.core.NodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, LocallyGeneratedModifier}
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.SyntacticallySuccessfulModifier
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Blake2b256
import sigmastate.Values.{ByteArrayConstant, Value}
import sigmastate._
import sigmastate.interpreter.{ContextExtension, ProverResult}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, blocking}
import scala.util.{Failure, Random, Success}


class ErgoWalletSpecification extends ErgoPropertyTest {
  private implicit val ergoAddressEncoder = new ErgoAddressEncoder(settings)

  property("off-chain scan") {
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

  property("off-chain box spending") {
    withWalletFixture { fixture =>
      import fixture._
      val tx = creationTxGen().sample.value
      wallet.scanOffchain(tx)
      val sumBalance = tx.outputs.map(_.value).sum
      blocking(Thread.sleep(offlineScanTime(tx)))
      val unconfirmedBalance = getUnconfirmedBalances.balance
      unconfirmedBalance shouldEqual sumBalance

      val boxesToSpend = someOf(tx.outputs).sample.value
      val balanceToSpend = sum(boxesToSpend)
      val balanceToReturn = Gen.choose(1, balanceToSpend - 1).sample.value
      val spendingTx = makeSpendingTx(boxesToSpend, pubKey, proofBytes, balanceToReturn)
      wallet.scanOffchain(spendingTx)
      blocking(Thread.sleep(offlineScanTime(tx)))

      val balanceAfterSpending = getUnconfirmedBalances.balance
      log.info(s"Unconfirmed balance: $unconfirmedBalance")
      log.info(s"Balance to spent: $balanceToSpend")
      log.info(s"Balance to return back: $balanceToReturn")
      balanceAfterSpending shouldEqual (unconfirmedBalance - balanceToSpend + balanceToReturn)
    }
  }

  property("on-chain scan") {
    withWalletFixture { fixture =>
      import fixture._
      val block = sendNextBlock(pubKey)
      wallet.scanPersistent(block)
      blocking(Thread.sleep(scanTime(block)))
      val confirmedBalance = getConfirmedBalances.balance
      val sumBalance = sumOutputs(block)
      log.info(s"Confirmed balance $confirmedBalance")
      log.info(s"Sum balance: $sumBalance")
      confirmedBalance should be > 0L
      confirmedBalance shouldBe sumBalance
    }
  }

  property("on-chain box spending") {
    withWalletFixture { fixture =>
      import fixture._
      val block = sendNextBlock(pubKey)
      wallet.scanPersistent(block)
      blocking(Thread.sleep(scanTime(block)))
      val confirmedBalance = getConfirmedBalances.balance
      val sumBalance = sumOutputs(block)
      log.info(s"Confirmed balance $confirmedBalance")
      log.info(s"Sum balance: $sumBalance")
      confirmedBalance should be > 0L
      confirmedBalance shouldBe sumBalance

      val utxoState = getCurrentState
      Inspectors.forAll(outputs(block)) { box =>
        val found = utxoState.boxById(box.id)
        log.error(s"Looking for Box ${Algos.encode(box.id)} in UTXO state: ${found.nonEmpty}")
        found should not be empty
      }

      val boxesToSpend = someOf(outputs(block)).sample.value
      val balanceToSpend = sum(boxesToSpend)
      val balanceToReturn = Gen.choose(1, balanceToSpend - 1).sample.value
      val spendingTx = makeSpendingTx(boxesToSpend, pubKey, proofBytes, balanceToReturn)

      //throws java.lang.Exception: Key 0b00769ac389c8a3fdd457e4ddfcef4558105e6ec82e719b02d94750c1acf2db does not exist
      val spendingBlock = sendBlock(makeNextBlock(Seq(spendingTx)))
      wallet.scanPersistent(spendingBlock)
      blocking(Thread.sleep(scanTime(spendingBlock)))

      val balanceAfterSpending = getConfirmedBalances.balance
      log.info(s"Unconfirmed balance: $confirmedBalance")
      log.info(s"Balance to spent: $balanceToSpend")
      log.info(s"Balance to return back: $balanceToReturn")
      balanceAfterSpending shouldEqual (confirmedBalance - balanceToSpend + balanceToReturn)
    }
  }

  property("off-chain transaction becomes onchain") {
    withWalletFixture { fixture =>
      import fixture._
      val block = sendNextBlock(pubKey)
      val sumBalance = sumOutputs(block)

      block.transactions.foreach { tx =>
        wallet.scanOffchain(tx)
      }
      blocking(Thread.sleep(scanTime(block)))
      val unconfirmedBalance = getUnconfirmedBalances.balance
      unconfirmedBalance shouldBe sumBalance

      wallet.scanPersistent(block)
      blocking(Thread.sleep(scanTime(block)))
      val confirmedBalance = getConfirmedBalances.balance
      log.info(s"Confirmed balance $confirmedBalance")
      log.info(s"Sum balance: $sumBalance")
      confirmedBalance should be > 0L
      confirmedBalance shouldBe sumBalance
      confirmedBalance shouldBe unconfirmedBalance

      getUnconfirmedBalances.balance shouldBe 0L
    }
  }

  property("unspent on-chain rollback") {
    withWalletFixture { fixture =>
      import fixture._
      val initialState = getCurrentState
      val versionId = scorex.core.versionToId(initialState.version)
      val initialHeight = getHistory.heightOf(versionId)

      val block = sendNextBlock(pubKey)
      wallet.scanPersistent(block)
      blocking(Thread.sleep(scanTime(block)))
      val historyHeight = getHistory.headersHeight
      val confirmedBalance = getConfirmedBalances.balance
      wallet.rollback(initialState.version)
      blocking(Thread.sleep(100))
      val balanceAfterRollback = getConfirmedBalances.balance
      val unconfirmedAfterRollback = getUnconfirmedBalances.balance

      val sumBalance = sumOutputs(block)
      log.info(s"Initial height: $initialHeight")
      log.info(s"History height: $historyHeight")
      log.info(s"Confirmed balance $confirmedBalance")
      log.info(s"Sum balance: $sumBalance")
      log.info(s"Balance after rollback: $balanceAfterRollback")
      log.info(s"Unconfirmed balance after rollback: $unconfirmedAfterRollback")

      confirmedBalance should be > 0L
      confirmedBalance shouldBe sumBalance
      balanceAfterRollback shouldBe 0L
      unconfirmedAfterRollback shouldBe sumBalance
    }
  }

  property("successfully generates a transaction") {
    withWalletFixture { fixture =>
      import fixture._
      val block = sendNextBlock(pubKey)
      wallet.scanPersistent(block)
      blocking(Thread.sleep(scanTime(block)))
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

      val block = sendNextBlock(p2s.script)
      wallet.scanPersistent(block)
      blocking(Thread.sleep(scanTime(block)))
      val confirmedBalance = getConfirmedBalances.balance

      confirmedBalance should be < sumOutputs(block)

      wallet.rollback(initialState.version)
      blocking(Thread.sleep(100))

      wallet.watchFor(p2s)

      wallet.scanPersistent(block)
      blocking(Thread.sleep(scanTime(block)))
      val confirmedBalance2 = getConfirmedBalances.balance
      confirmedBalance2 shouldBe sumOutputs(block)
    }
  }

  def withWalletFixture[T](test: WalletFixture => T): T = {
    new WalletFixture().runAndClose(test)
  }
}

class WalletFixture extends WalletFixtureUtil with ScorexLogging {

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

  val initialBlocks: Seq[ErgoFullBlock] = init()

  def init(): Seq[ErgoFullBlock] = {
    actorSystem.eventStream.subscribe(testProbe.ref, classOf[SyntacticallySuccessfulModifier[_]])
    genChain(3).map(sendBlock)
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

  def getCurrentState: UtxoState = getCurrentView.state

  type CurView = CurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool]

  def getCurrentView: CurView = {
    val request = GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, CurView](view => view)
    Await.result((nodeViewHolderRef ? request).mapTo[CurView], awaitDuration)
  }


  def sendNextBlock(injectedScript: Value[SBoolean.type]): ErgoFullBlock = {
    val txGen = creationTxGen(Some(arbScriptBoxCandidateGen(injectedScript)))
    val nextBlock = makeNextBlock(Seq(txGen.sample.value))
    sendBlock(nextBlock)
  }

  def sendBlock(block: ErgoFullBlock): ErgoFullBlock = {
    nodeViewHolderRef ! LocallyGeneratedModifier(block.header)
    testProbe.expectMsgType[SyntacticallySuccessfulModifier[Header]]
    if (settings.nodeSettings.verifyTransactions) {
      nodeViewHolderRef ! LocallyGeneratedModifier(block.blockTransactions)
      nodeViewHolderRef ! LocallyGeneratedModifier(block.adProofs.get)
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

  def makeNextBlock(txs: Seq[ErgoTransaction]): ErgoFullBlock = {
    val currentView = getCurrentView
    currentView.state.proofsForTransactions(txs) match {
      case Failure(e) =>
        log.error(s"Failed to prove transactions while creating new block: $txs", e)
        throw new AssertionError(s"Transaction prove failure: $e", e)
      case Success((adProofs, stateDigest)) =>
        val time = timeProvider.time()
        val ext = ExtensionCandidate(Seq(), Seq())
        val parent = currentView.history.bestHeaderOpt
        DefaultFakePowScheme.proveBlock(parent, Constants.InitialNBits, stateDigest, adProofs, txs, time, ext).get
    }
  }

  def creationTxGen(neededOutputGen: Option[Gen[ErgoBoxCandidate]] = None): Gen[ErgoTransaction] = {
    val noProof = ProverResult(SigSerializer.toBytes(NoProof), ContextExtension.empty)
    val input = Input(genesisEmissionBox.id, noProof)
    val boxCount = Gen.choose(1, 20).sample.getOrElse(5)
    val outputsGen = Gen.listOfN(boxCount, p2pkBoxCandidateGen)
      .map(_.toIndexedSeq ++ neededOutputGen.flatMap(_.sample).toIndexedSeq)
    outputsGen.map(outs => new ErgoTransaction(IndexedSeq(input), outs))
  }

  def makeSpendingTx(boxesToSpend: Seq[ErgoBox],
                     script: Value[SBoolean.type],
                     proof: Array[Byte],
                     balanceToReturn: Long = 0): ErgoTransaction = {
    val balanceToSpend = sum(boxesToSpend)
    val outputs = IndexedSeq(makeOutput(balanceToReturn, script), makeOutput(balanceToSpend - balanceToReturn))
    new ErgoTransaction(makeInputs(boxesToSpend, proof), outputs)
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

}


trait WalletFixtureUtil {

  def settings: ErgoSettings

  def scanningInterval: Long = settings.walletSettings.scanningInterval.toMillis

  def sumOutputs(block: ErgoFullBlock): Long = sum(outputs(block))

  def sum(boxes: Seq[ErgoBox]): Long = boxes.map(_.value).sum

  def outputs(block: ErgoFullBlock): Seq[ErgoBox] = block.transactions.flatMap(_.outputs)

  def scanTime(block: ErgoFullBlock): Long = scanTime(outputs(block).size)

  def scanTime(boxCount: Int): Long = boxCount * scanningInterval + 1000

  def offlineScanTime(tx: ErgoTransaction): Long = tx.outputs.size * 100 + 300

}
