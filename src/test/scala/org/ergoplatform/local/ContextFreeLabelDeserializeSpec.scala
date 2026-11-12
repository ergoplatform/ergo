package org.ergoplatform.local

import org.ergoplatform.{ErgoAddressEncoder, ErgoBox, ErgoBoxCandidate, Input}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.ProcessingOutcome
import org.ergoplatform.settings.{Constants, ErgoSettings}
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.utils.{ErgoTestHelpers, MempoolTestHelpers}
import org.scalatest.flatspec.AnyFlatSpec
import sigma.ast.{ByteArrayConstant, ByteConstant, ErgoTree}
import sigma.ast.syntax.ValueOps
import sigma.compiler.SigmaCompiler
import sigma.compiler.ir.CompiletimeIRContext
import sigma.interpreter.{ContextExtension, ProverResult}
import sigma.serialization.ValueSerializer
import sigmastate.interpreter.Interpreter.emptyEnv

import scala.concurrent.Await
import scala.concurrent.duration._

/**
  * Regression tests verifying that transactions whose validity depends on blockchain context
  * via spend-time deserialization (`ergoTree.hasDeserialize`) or storage-rent collection
  * (`ErgoMemPool.containsStorageRentClaim`) are labelled `isUsingBlockchainContext = Some(true)`
  * both at mempool admission (`ErgoMemPool.process`) and during `CleanupWorker.validatePool`
  * fallback, and are dropped on cleanup once a new block makes them invalid.
  */
class ContextFreeLabelDeserializeSpec extends AnyFlatSpec with ErgoTestHelpers with MempoolTestHelpers {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._
  import org.ergoplatform.utils.generators.ValidBlocksGenerators._

  val cleanupDuration: FiniteDuration = 200.millis
  val settingsToTest: ErgoSettings = settings.copy(
    nodeSettings = settings.nodeSettings.copy(
      mempoolCleanupDuration = cleanupDuration,
      rejectStorageRentTxs = false
    )
  )

  private def compile(script: String): ErgoTree = {
    val compiler = new SigmaCompiler(ErgoAddressEncoder.MainnetNetworkPrefix)
    compiler.compile(emptyEnv, script)(new CompiletimeIRContext).buildTree
  }

  it should "drop on cleanup a transaction whose input runs supplied code reading HEIGHT" in {
    val (us0, bh0) = createUtxoState(settingsToTest)
    val (txs0, bh1) = validTransactionsFromBoxHolder(bh0)
    val b1 = validFullBlock(None, us0, txs0)
    val us1 = us0.applyModifier(b1, None)(_ => ()).get
    val boxes = bh1.boxes.values.toList.filter(_.proposition != genesisEmissionBox.proposition)

    // Box script: run whatever SigmaProp the spender supplies in context variable 1.
    val deserTree = ErgoTree.fromProposition(compile("{ executeFromVar[SigmaProp](1) }").asSigmaProp)
    val parent = validTransactionFromBoxes(boxes.take(1).toIndexedSeq, outputsProposition = deserTree)
    val b2 = validFullBlock(Some(b1), us1, Seq(parent))
    val us2 = us1.applyModifier(b2, None)(_ => ()).get

    val box: ErgoBox = us2.boxById(parent.outputs.head.id).get
    box.ergoTree.isUsingBlockchainContext shouldBe false
    box.ergoTree.hasDeserialize shouldBe true

    // Supplied code: valid only up to the height a pool transaction is checked at now.
    val h = us2.stateContext.simplifiedUpcoming().currentHeight
    val suppliedBytes = ValueSerializer.serialize(compile(s"{ sigmaProp(HEIGHT <= $h) }"))
    val extension = ContextExtension(Map(1.toByte -> ByteArrayConstant(suppliedBytes)))
    val child = ErgoTransaction(
      IndexedSeq(Input(box.id, ProverResult(Array.emptyByteArray, extension))),
      IndexedSeq.empty,
      IndexedSeq(new ErgoBoxCandidate(box.value, TrueTree, h))
    )

    val (pool1, outcome) = ErgoMemPool.empty(settingsToTest).process(UnconfirmedTransaction(child, None), us2)
    outcome shouldBe a[ProcessingOutcome.Accepted]
    val admitted = pool1.getAll.find(_.id == child.id).get
    admitted.isUsingBlockchainContext shouldBe Some(true)

    // Also verify CleanupWorker fallback when isUsingBlockchainContext is None while still valid at us2
    val staleTime = System.currentTimeMillis() - cleanupDuration.toMillis * 10
    val unlabelledAtUs2 = new UnconfirmedTransaction(
      child, admitted.lastCost, staleTime, staleTime, Some(child.bytes), None,
      isUsingBlockchainContext = None, validationResult = None
    )
    val recheckedAtUs2 = Await.result(
      CleanupWorker.validatePool(us2, new FakeMempool(Seq(unlabelledAtUs2)), java.util.UUID.randomUUID(),
        settingsToTest.nodeSettings),
      10.seconds
    )
    recheckedAtUs2.unconfirmedTxs.head.isUsingBlockchainContext shouldBe Some(true)

    // One more block, not touching the child's input.
    val other = validTransactionFromBoxes(boxes.slice(1, 2).toIndexedSeq)
    val b3 = validFullBlock(Some(b2), us2, Seq(other))
    val us3 = us2.applyModifier(b3, None)(_ => ()).get

    // Control: the input still exists, and the child no longer validates at the new tip.
    us3.boxById(box.id).isDefined shouldBe true
    us3.validateWithCost(child, us3.stateContext.simplifiedUpcoming(),
      settingsToTest.nodeSettings.maxTransactionCost, None).isFailure shouldBe true

    val stale = new UnconfirmedTransaction(
      child, admitted.lastCost, staleTime, staleTime, Some(child.bytes), None,
      admitted.isUsingBlockchainContext, admitted.validationResult
    )
    val rechecked = Await.result(
      CleanupWorker.validatePool(us3, new FakeMempool(Seq(stale)), java.util.UUID.randomUUID(),
        settingsToTest.nodeSettings),
      10.seconds
    )

    withClue(s"label at admission: isUsingBlockchainContext=${admitted.isUsingBlockchainContext}, " +
      s"validationResult=${admitted.validationResult}; ") {
      rechecked.invalidatedIds shouldBe Seq(child.id)
    }
  }

  it should "label storage-rent claim transactions as context-dependent in ErgoMemPool and CleanupWorker" in {
    val (us0, bh0) = createUtxoState(settingsToTest)
    val (txs0, bh1) = validTransactionsFromBoxHolder(bh0)
    val b1 = validFullBlock(None, us0, txs0)
    val us1 = us0.applyModifier(b1, None)(_ => ()).get
    val boxes = bh1.boxes.values.toList.filter(_.proposition != genesisEmissionBox.proposition)

    // Context-free box (TrueTree: both isUsingBlockchainContext and hasDeserialize are false)
    val parent = validTransactionFromBoxes(boxes.take(1).toIndexedSeq, outputsProposition = TrueTree)
    val b2 = validFullBlock(Some(b1), us1, Seq(parent))
    val us2 = us1.applyModifier(b2, None)(_ => ()).get

    val box: ErgoBox = us2.boxById(parent.outputs.head.id).get
    box.ergoTree.isUsingBlockchainContext shouldBe false
    box.ergoTree.hasDeserialize shouldBe false

    // Construct a transaction carrying the storage-rent claim marker (empty proof + StorageIndexVarId)
    val h = us2.stateContext.simplifiedUpcoming().currentHeight
    val rentExt = ContextExtension(Map(Constants.StorageIndexVarId -> ByteConstant(0)))
    val rentLikeTx = ErgoTransaction(
      IndexedSeq(Input(box.id, ProverResult(Array.emptyByteArray, rentExt))),
      IndexedSeq.empty,
      IndexedSeq(new ErgoBoxCandidate(box.value, box.ergoTree, h))
    )
    ErgoMemPool.containsStorageRentClaim(rentLikeTx) shouldBe true

    // Verify ErgoMemPool.process labels it isUsingBlockchainContext = Some(true)
    val (pool1, _) = ErgoMemPool.empty(settingsToTest).process(UnconfirmedTransaction(rentLikeTx, None), us2)
    val admittedOpt = pool1.getAll.find(_.id == rentLikeTx.id)
    admittedOpt.foreach(_.isUsingBlockchainContext shouldBe Some(true))

    // Verify CleanupWorker.validatePool fallback labels an unlabelled storage-rent claim tx as Some(true)
    // when validation succeeds
    val alwaysValidState = new org.ergoplatform.nodeView.state.UtxoState(
      us2.persistentProver, us2.version, us2.store, settingsToTest
    ) {
      override def withUnconfirmedTransactions(unconfirmedTxs: Seq[UnconfirmedTransaction]): org.ergoplatform.nodeView.state.UtxoState =
        new org.ergoplatform.nodeView.state.UtxoState(us2.persistentProver, us2.version, us2.store, settingsToTest) {
          override def boxById(id: scorex.crypto.authds.ADKey): Option[ErgoBox] = us2.boxById(id)
          override def validateWithCost(tx: ErgoTransaction,
                                        context: org.ergoplatform.nodeView.state.ErgoStateContext,
                                        costLimit: Int,
                                        interpreterOpt: Option[org.ergoplatform.wallet.interpreter.ErgoInterpreter]): scala.util.Try[Int] =
            scala.util.Success(1000)
        }
    }
    val staleTime = System.currentTimeMillis() - cleanupDuration.toMillis * 10
    val unlabelledRentUt = new UnconfirmedTransaction(
      rentLikeTx, Some(1000), staleTime, staleTime, Some(rentLikeTx.bytes), None,
      isUsingBlockchainContext = None, validationResult = None
    )
    val recheckedFallback = Await.result(
      CleanupWorker.validatePool(alwaysValidState, new FakeMempool(Seq(unlabelledRentUt)),
        java.util.UUID.randomUUID(), settingsToTest.nodeSettings),
      10.seconds
    )
    recheckedFallback.unconfirmedTxs.head.isUsingBlockchainContext shouldBe Some(true)
  }
}
