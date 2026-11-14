package org.ergoplatform.local

import org.ergoplatform.{ErgoAddressEncoder, ErgoBox, ErgoBoxCandidate, Input}
import org.ergoplatform.mining.emission.EmissionRules.CoinsInOneErgo
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.ProcessingOutcome
import org.ergoplatform.nodeView.state.{BoxHolder, ErgoStateReader}
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.settings.{Constants, ErgoSettings}
import org.ergoplatform.utils.{ErgoStateContextHelpers, ErgoTestHelpers, MempoolTestHelpers}
import org.scalatest.flatspec.AnyFlatSpec
import scorex.util.bytesToId
import sigma.Colls
import sigma.ast.{ByteArrayConstant, ErgoTree, ShortConstant}
import sigma.ast.syntax.ValueOps
import sigma.compiler.SigmaCompiler
import sigma.compiler.ir.CompiletimeIRContext
import sigma.interpreter.{ContextExtension, ProverResult}
import sigma.serialization.ValueSerializer
import sigmastate.interpreter.Interpreter.emptyEnv

import scala.concurrent.Await
import scala.concurrent.duration._

/**
  * Verifies context-dependence labeling and CleanupWorker invalidation for:
  * 1. Inputs whose ErgoTree deserializes code at spend time (executeFromVar / DeserializeContext)
  *    that reads HEIGHT while `ergoTree.isUsingBlockchainContext` is false.
  * 2. Storage-rent claims (empty proof + context variable #127) on ordinary P2PK boxes whose
  *    validity depends on upcoming block height (`creationHeight == upcomingHeight`).
  */
class ContextFreeLabelDeserializeSpec extends AnyFlatSpec
  with ErgoTestHelpers
  with MempoolTestHelpers
  with ErgoStateContextHelpers {

  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._
  import org.ergoplatform.utils.generators.ValidBlocksGenerators._

  val cleanupDuration: FiniteDuration = 200.millis
  val settingsToTest: ErgoSettings = settings.copy(
    nodeSettings = settings.nodeSettings.copy(mempoolCleanupDuration = cleanupDuration))

  private def compile(script: String) = {
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
      IndexedSeq(new ErgoBoxCandidate(box.value, TrueTree, h)))

    val (pool1, outcome) = ErgoMemPool.empty(settingsToTest).process(UnconfirmedTransaction(child, None), us2)
    outcome shouldBe a[ProcessingOutcome.Accepted]
    val admitted = pool1.getAll.find(_.id == child.id).get
    admitted.isUsingBlockchainContext shouldBe Some(true)
    admitted.validationResult shouldBe Some(true)

    val staleTime = System.currentTimeMillis() - cleanupDuration.toMillis * 10
    val unlabeled = new UnconfirmedTransaction(
      child, admitted.lastCost, staleTime, staleTime, Some(child.bytes), None,
      isUsingBlockchainContext = None, validationResult = None)
    val recheckedAtUs2 = Await.result(
      CleanupWorker.validatePool(us2, new FakeMempool(Seq(unlabeled)), java.util.UUID.randomUUID(),
        settingsToTest.nodeSettings),
      10.seconds)
    recheckedAtUs2.invalidatedIds shouldBe empty
    recheckedAtUs2.unconfirmedTxs.map(_.isUsingBlockchainContext) shouldBe Seq(Some(true))

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
      admitted.isUsingBlockchainContext, admitted.validationResult)
    val rechecked = Await.result(
      CleanupWorker.validatePool(us3, new FakeMempool(Seq(stale)), java.util.UUID.randomUUID(),
        settingsToTest.nodeSettings),
      10.seconds)

    withClue(s"label at admission: isUsingBlockchainContext=${admitted.isUsingBlockchainContext}, " +
      s"validationResult=${admitted.validationResult}; ") {
      rechecked.invalidatedIds shouldBe Seq(child.id)
    }
  }

  it should "label storage-rent claims as context-dependent and drop them after one block" in {
    val rentAllowedSettings: ErgoSettings = settingsToTest.copy(
      nodeSettings = settingsToTest.nodeSettings.copy(rejectStorageRentTxs = false))

    val p2pkTree: ErgoTree = ErgoTree.fromProposition(defaultMinerSecret.publicImage)
    val expiredBox = new ErgoBox(
      value = 10L * CoinsInOneErgo,
      ergoTree = p2pkTree,
      additionalTokens = Colls.emptyColl,
      additionalRegisters = Map.empty,
      transactionId = bytesToId(Array.fill(32)(1.toByte)),
      index = 0,
      creationHeight = 1
    )
    expiredBox.ergoTree.isUsingBlockchainContext shouldBe false
    expiredBox.ergoTree.hasDeserialize shouldBe false

    val h: Int = expiredBox.creationHeight + Constants.StoragePeriod
    val storageFee: Long = parameters.storageFeeFactor.toLong * expiredBox.bytes.length
    val rentTx = ErgoTransaction(
      IndexedSeq(Input(expiredBox.id, ProverResult(
        Array.emptyByteArray,
        ContextExtension(Map(Constants.StorageIndexVarId -> ShortConstant(0)))
      ))),
      IndexedSeq.empty,
      IndexedSeq(
        new ErgoBoxCandidate(expiredBox.value - storageFee, p2pkTree, h),
        new ErgoBoxCandidate(storageFee, rentAllowedSettings.chainSettings.monetary.feeProposition, h)
      )
    )

    val usHMinus1 = WrappedUtxoState(BoxHolder(Seq(expiredBox)), createTempDir, parameters, rentAllowedSettings)
    usHMinus1.store.update(
      Array(1.toByte),
      Seq.empty,
      Seq(ErgoStateReader.ContextKey -> stateContext(h - 1, Header.Interpreter60Version, rentAllowedSettings).bytes)
    ).get
    usHMinus1.stateContext.simplifiedUpcoming().currentHeight shouldBe h

    val (pool1, outcome) = ErgoMemPool.empty(rentAllowedSettings).process(UnconfirmedTransaction(rentTx, None), usHMinus1)
    outcome shouldBe a[ProcessingOutcome.Accepted]
    val admitted = pool1.getAll.find(_.id == rentTx.id).get
    admitted.isUsingBlockchainContext shouldBe Some(true)
    admitted.validationResult shouldBe Some(true)

    // Verify CleanupWorker's fallback labeling when isUsingBlockchainContext is None.
    val staleTime = System.currentTimeMillis() - cleanupDuration.toMillis * 10
    val unlabeled = new UnconfirmedTransaction(
      rentTx, admitted.lastCost, staleTime, staleTime, Some(rentTx.bytes), None,
      isUsingBlockchainContext = None, validationResult = None)
    val recheckedAtH = Await.result(
      CleanupWorker.validatePool(usHMinus1, new FakeMempool(Seq(unlabeled)), java.util.UUID.randomUUID(),
        rentAllowedSettings.nodeSettings),
      10.seconds)
    recheckedAtH.invalidatedIds shouldBe empty
    recheckedAtH.unconfirmedTxs.map(_.isUsingBlockchainContext) shouldBe Seq(Some(true))

    // Advance height by one block while keeping the expired P2PK box unspent.
    val usH = WrappedUtxoState(BoxHolder(Seq(expiredBox)), createTempDir, parameters, rentAllowedSettings)
    usH.store.update(
      Array(2.toByte),
      Seq.empty,
      Seq(ErgoStateReader.ContextKey -> stateContext(h, Header.Interpreter60Version, rentAllowedSettings).bytes)
    ).get
    usH.stateContext.simplifiedUpcoming().currentHeight shouldBe (h + 1)
    usH.boxById(expiredBox.id).isDefined shouldBe true
    usH.validateWithCost(rentTx, usH.stateContext.simplifiedUpcoming(),
      rentAllowedSettings.nodeSettings.maxTransactionCost, None).isFailure shouldBe true

    val stale = new UnconfirmedTransaction(
      rentTx, admitted.lastCost, staleTime, staleTime, Some(rentTx.bytes), None,
      admitted.isUsingBlockchainContext, admitted.validationResult)
    val recheckedAtNext = Await.result(
      CleanupWorker.validatePool(usH, new FakeMempool(Seq(stale)), java.util.UUID.randomUUID(),
        rentAllowedSettings.nodeSettings),
      10.seconds)

    withClue(s"label at admission: isUsingBlockchainContext=${admitted.isUsingBlockchainContext}, " +
      s"validationResult=${admitted.validationResult}; ") {
      recheckedAtNext.invalidatedIds shouldBe Seq(rentTx.id)
    }
  }
}
