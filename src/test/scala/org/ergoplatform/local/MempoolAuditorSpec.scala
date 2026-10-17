package org.ergoplatform.local

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestActorRef, TestProbe}
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.modifiers.mempool.UnconfirmedTransaction
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{FailedTransaction, RecheckMempool, SuccessfulTransaction}
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.{LocallyGeneratedTransaction, RecheckedTransactions}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.ProcessingOutcome
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.ergoplatform.utils.ErgoCoreTestConstants.parameters
import org.ergoplatform.utils.fixtures.NodeViewFixture
import org.ergoplatform.utils.{ErgoTestHelpers, MempoolTestHelpers, NodeViewTestOps, RandomWrapper}
import org.scalatest.flatspec.AnyFlatSpec
import scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork
import sigma.ast.ErgoTree
import sigma.ast.syntax.ValueOps
import sigma.compiler.SigmaCompiler
import sigma.compiler.ir.CompiletimeIRContext
import sigma.serialization.ErgoTreeSerializer
import sigmastate.interpreter.Interpreter.emptyEnv
import org.ergoplatform.settings.Constants.TrueTree

import scala.concurrent.duration._

class MempoolAuditorSpec extends AnyFlatSpec with NodeViewTestOps with ErgoTestHelpers with MempoolTestHelpers {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._
  import org.ergoplatform.utils.generators.ValidBlocksGenerators._

  val cleanupDuration: FiniteDuration = 200.millis
  val settingsToTest: ErgoSettings = settings.copy(
    nodeSettings = settings.nodeSettings.copy(
      mempoolCleanupDuration = cleanupDuration,
      rebroadcastCount = 1
    ))
  val fixture = new NodeViewFixture(settingsToTest, parameters)
  val newTx: Class[SuccessfulTransaction] = classOf[SuccessfulTransaction]

  it should "remove transactions which become invalid" in {
    import fixture._

    val testProbe = new TestProbe(actorSystem)
    actorSystem.eventStream.subscribe(testProbe.ref, newTx)

    val (us, bh) = createUtxoState(settingsToTest)
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis =
      WrappedUtxoState(us, bh, settingsToTest).applyModifier(genesis) { mod =>
        nodeViewHolderRef ! mod
      } .get

    applyBlock(genesis) shouldBe 'success
    getRootHash shouldBe Algos.encode(wusAfterGenesis.rootDigest)

    val boxes = ErgoState.newBoxes(genesis.transactions).find(_.ergoTree == TrueTree)
    boxes.nonEmpty shouldBe true

    val script = s"{sigmaProp(HEIGHT == ${genesis.height} + 1)}"
    val compiler = new SigmaCompiler(ErgoAddressEncoder.MainnetNetworkPrefix)
    val prop = compiler.compile(emptyEnv, script)(new CompiletimeIRContext).buildTree
    val tree = ErgoTree.fromProposition(prop.asSigmaProp)

    val bs = ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(tree)
    ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(bs) shouldBe tree

    val validTx = validTransactionFromBoxes(boxes.toIndexedSeq, outputsProposition = tree)

    val temporarilyValidTx = validTransactionFromBoxes(validTx.outputs, outputsProposition = ErgoTree.fromProposition(proveDlogGen.sample.get))

    subscribeEvents(classOf[FailedTransaction])
    nodeViewHolderRef ! LocallyGeneratedTransaction(UnconfirmedTransaction(validTx, None))
    testProbe.expectMsgClass(cleanupDuration, newTx)
    expectMsgType[ProcessingOutcome.Accepted]

    nodeViewHolderRef ! LocallyGeneratedTransaction(UnconfirmedTransaction(temporarilyValidTx, None))
    testProbe.expectMsgClass(cleanupDuration, newTx)
    expectMsgType[ProcessingOutcome.Accepted]

    getPoolSize shouldBe 2

    val _: ActorRef = MempoolAuditorRef(nodeViewHolderRef, nodeViewHolderRef, settingsToTest)

    Thread.sleep(200) // give transactions in the pool enough time to become candidates for re-checking

    // include first transaction in the block
    val block = validFullBlock(Some(genesis), wusAfterGenesis, Seq(validTx))

    applyBlock(block) shouldBe 'success

    org.ergoplatform.utils.untilTimeout(cleanupDuration * 4, 100.millis) {
      // first tx removed from pool during node view update
      // another tx invalidated by `MempoolAuditor`
      getPoolSize shouldBe 0
    }
  }

  it should "rebroadcast transactions correctly" in {

    val (us0, bh0) = createUtxoState(settingsToTest)
    val (txs0, bh1) = validTransactionsFromBoxHolder(bh0)
    val b1 = validFullBlock(None, us0, txs0)

    val us = us0.applyModifier(b1, None)(_ => ()).get

    val bxs = bh1.boxes.values.toList.filter(_.proposition != genesisEmissionBox.proposition)
    val txs = validTransactionsFromBoxes(200000, bxs, new RandomWrapper)._1
      .map(tx => UnconfirmedTransaction(tx, None))

    implicit val system = ActorSystem()
    val probe = TestProbe()

    val auditor: ActorRef = TestActorRef(new MempoolAuditor(probe.ref, probe.ref, settingsToTest))


    auditor ! RecheckMempool(us, new FakeMempool(txs))

    probe.fishForMessage(3.seconds) {
      case _: SendToNetwork => true
      case _: RecheckedTransactions => false
    }.isInstanceOf[SendToNetwork] shouldBe true
  }

  it should "label blockchain context usage and simplify revalidation in CleanupWorker" in {
    val (us0, bh0) = createUtxoState(settingsToTest)
    val (txs0, bh1) = validTransactionsFromBoxHolder(bh0)
    val b1 = validFullBlock(None, us0, txs0)
    val us = us0.applyModifier(b1, None)(_ => ()).get

    val boxes = bh1.boxes.values.toList.filter(_.proposition != genesisEmissionBox.proposition)
    val contextFreeTx = validTransactionsFromBoxes(200000, boxes, new RandomWrapper)._1.head

    val pool0 = org.ergoplatform.nodeView.mempool.ErgoMemPool.empty(settingsToTest)
    val (pool1, outcome1) = pool0.process(UnconfirmedTransaction(contextFreeTx, None), us)
    outcome1 shouldBe a[ProcessingOutcome.Accepted]
    val acceptedContextFreeUt = pool1.getAll.find(_.id == contextFreeTx.id).get
    acceptedContextFreeUt.isUsingBlockchainContext shouldBe Some(false)
    acceptedContextFreeUt.validationResult shouldBe Some(true)
    acceptedContextFreeUt.lastCost.isDefined shouldBe true

    // Create a transaction whose output uses HEIGHT (blockchain context) and a child transaction spending it
    val script = s"{sigmaProp(HEIGHT >= 0)}"
    val compiler = new SigmaCompiler(ErgoAddressEncoder.MainnetNetworkPrefix)
    val prop = compiler.compile(emptyEnv, script)(new CompiletimeIRContext).buildTree
    val heightTree = ErgoTree.fromProposition(prop.asSigmaProp)
    heightTree.isUsingBlockchainContext shouldBe true

    val parentWithHeightOut = validTransactionFromBoxes(boxes.take(1).toIndexedSeq, outputsProposition = heightTree)
    val childSpendingHeightBox = validTransactionFromBoxes(
      parentWithHeightOut.outputs,
      outputsProposition = TrueTree
    )

    val (pool2, parentOutcome) = pool0.process(UnconfirmedTransaction(parentWithHeightOut, None), us)
    parentOutcome shouldBe a[ProcessingOutcome.Accepted]
    pool2.getAll.find(_.id == parentWithHeightOut.id).get.isUsingBlockchainContext shouldBe Some(false)

    val (pool3, childOutcome) = pool2.process(UnconfirmedTransaction(childSpendingHeightBox, None), us)
    childOutcome shouldBe a[ProcessingOutcome.Accepted]
    val acceptedContextDepUt = pool3.getAll.find(_.id == childSpendingHeightBox.id).get
    acceptedContextDepUt.isUsingBlockchainContext shouldBe Some(true)
    acceptedContextDepUt.validationResult shouldBe Some(true)

    // Verify CleanupWorker skips validateWithCost when isUsingBlockchainContext == Some(false) && validationResult == Some(true)
    val validateCalls = new java.util.concurrent.atomic.AtomicInteger(0)
    val countingState = new org.ergoplatform.nodeView.state.UtxoState(
      us.persistentProver,
      us.version,
      us.store,
      settingsToTest
    ) {
      override def withUnconfirmedTransactions(
        unconfirmedTxs: Seq[UnconfirmedTransaction]
      ): org.ergoplatform.nodeView.state.UtxoState = {
        val base = super.withUnconfirmedTransactions(unconfirmedTxs)
        new org.ergoplatform.nodeView.state.UtxoState(
          base.persistentProver,
          base.version,
          base.store,
          settingsToTest
        ) {
          override def boxById(id: scorex.crypto.authds.ADKey): Option[org.ergoplatform.ErgoBox] =
            base.boxById(id)

          override def validateWithCost(
            tx: org.ergoplatform.modifiers.mempool.ErgoTransaction,
            context: org.ergoplatform.nodeView.state.ErgoStateContext,
            costLimit: Int,
            interpreterOpt: Option[org.ergoplatform.wallet.interpreter.ErgoInterpreter]
          ): scala.util.Try[Int] = {
            validateCalls.incrementAndGet()
            base.validateWithCost(tx, context, costLimit, interpreterOpt)
          }
        }
      }
    }

    val staleTime = System.currentTimeMillis() - cleanupDuration.toMillis * 10
    val staleContextFreeUt = new UnconfirmedTransaction(
      acceptedContextFreeUt.transaction,
      acceptedContextFreeUt.lastCost,
      staleTime,
      staleTime,
      acceptedContextFreeUt.transactionBytes,
      None,
      isUsingBlockchainContext = Some(false),
      validationResult = Some(true)
    )

    val (validated1, invalidated1) = scala.concurrent.Await.result(
      CleanupWorker.validatePool(countingState, new FakeMempool(Seq(staleContextFreeUt)), settingsToTest.nodeSettings),
      5.seconds
    )
    validateCalls.get() shouldBe 0
    invalidated1 shouldBe empty
    validated1.map(_.id) shouldBe Seq(staleContextFreeUt.id)
    validated1.head.isUsingBlockchainContext shouldBe Some(false)
    validated1.head.validationResult shouldBe Some(true)

    // Missing input for context-free tx should invalidate without calling validateWithCost
    val missingInputTx = invalidErgoTransactionGen.sample.get
    val staleMissingInputUt = new UnconfirmedTransaction(
      missingInputTx,
      Some(1000),
      staleTime,
      staleTime,
      Some(missingInputTx.bytes),
      None,
      isUsingBlockchainContext = Some(false),
      validationResult = Some(true)
    )
    val (validated2, invalidated2) = scala.concurrent.Await.result(
      CleanupWorker.validatePool(countingState, new FakeMempool(Seq(staleMissingInputUt)), settingsToTest.nodeSettings),
      5.seconds
    )
    validateCalls.get() shouldBe 0
    validated2 shouldBe empty
    invalidated2 shouldBe Seq(missingInputTx.id)

    // Context-dependent tx (isUsingBlockchainContext = Some(true)) must still run full validateWithCost
    val staleParentUt = new UnconfirmedTransaction(
      parentWithHeightOut,
      Some(1000),
      staleTime,
      staleTime,
      Some(parentWithHeightOut.bytes),
      None,
      isUsingBlockchainContext = Some(false),
      validationResult = Some(true)
    )
    val staleContextDepUt = new UnconfirmedTransaction(
      childSpendingHeightBox,
      acceptedContextDepUt.lastCost,
      staleTime,
      staleTime,
      acceptedContextDepUt.transactionBytes,
      None,
      isUsingBlockchainContext = Some(true),
      validationResult = Some(true)
    )
    val (validated3, invalidated3) = scala.concurrent.Await.result(
      CleanupWorker.validatePool(
        countingState,
        new FakeMempool(Seq(staleParentUt, staleContextDepUt)),
        settingsToTest.nodeSettings
      ),
      5.seconds
    )
    validateCalls.get() shouldBe 1
    invalidated3 shouldBe empty
    validated3.map(_.id) should contain theSameElementsAs Seq(staleParentUt.id, staleContextDepUt.id)
  }
}
