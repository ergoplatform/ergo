package org.ergoplatform.local

import org.ergoplatform.{ErgoAddressEncoder, ErgoBox, ErgoBoxCandidate, Input}
import org.ergoplatform.mining.emission.EmissionRules.CoinsInOneErgo
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.ProcessingOutcome
import org.ergoplatform.nodeView.state.BoxHolder
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.{Constants, ErgoSettings}
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.utils.{ErgoTestHelpers, MempoolTestHelpers}
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
  * Inputs whose ErgoTree runs code supplied at spend time (executeFromVar / DeserializeContext /
  * DeserializeRegister) or transactions claiming storage rent can depend on HEIGHT even when
  * `ergoTree.isUsingBlockchainContext` is false. Mempool admission and cleanup must treat both
  * as context-dependent so full revalidation runs when new blocks arrive.
  */
class ContextFreeLabelDeserializeSpec extends AnyFlatSpec with ErgoTestHelpers with MempoolTestHelpers {
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

    // One more block, not touching the child's input.
    val other = validTransactionFromBoxes(boxes.slice(1, 2).toIndexedSeq)
    val b3 = validFullBlock(Some(b2), us2, Seq(other))
    val us3 = us2.applyModifier(b3, None)(_ => ()).get

    // Control: the input still exists, and the child no longer validates at the new tip.
    us3.boxById(box.id).isDefined shouldBe true
    us3.validateWithCost(child, us3.stateContext.simplifiedUpcoming(),
      settingsToTest.nodeSettings.maxTransactionCost, None).isFailure shouldBe true

    val staleTime = System.currentTimeMillis() - cleanupDuration.toMillis * 10
    val stale = new UnconfirmedTransaction(
      child, admitted.lastCost, staleTime, staleTime, Some(child.bytes), None,
      admitted.isUsingBlockchainContext, admitted.validationResult)
    val rechecked = Await.result(
      CleanupWorker.validatePool(us3, new FakeMempool(Seq(stale)), java.util.UUID.randomUUID(), settingsToTest.nodeSettings),
      10.seconds)

    withClue(s"label at admission: isUsingBlockchainContext=${admitted.isUsingBlockchainContext}, " +
      s"validationResult=${admitted.validationResult}; ") {
      admitted.isUsingBlockchainContext shouldBe Some(true)
      rechecked.invalidatedIds shouldBe Seq(child.id)
    }
  }

  it should "label storage-rent claims as using blockchain context on admission and cleanup fallback" in {
    val rentSettings = settingsToTest.copy(
      nodeSettings = settingsToTest.nodeSettings.copy(rejectStorageRentTxs = false))
    val boxValue = 10L * CoinsInOneErgo
    val rentBox = new ErgoBox(
      boxValue,
      TrueTree,
      Colls.emptyColl,
      Map.empty,
      bytesToId(Array.fill(32)(7.toByte)),
      0: Short,
      creationHeight = 0
    )
    rentBox.ergoTree.isUsingBlockchainContext shouldBe false
    rentBox.ergoTree.hasDeserialize shouldBe false

    val state = WrappedUtxoState(BoxHolder(Seq(rentBox)), createTempDir, rentSettings.launchParameters, rentSettings)
    val rentTx = ErgoTransaction(
      IndexedSeq(Input(
        rentBox.id,
        ProverResult(
          Array.emptyByteArray,
          ContextExtension(Map(Constants.StorageIndexVarId -> ShortConstant(0)))
        )
      )),
      IndexedSeq.empty,
      IndexedSeq(new ErgoBoxCandidate(boxValue, TrueTree, 0, Colls.emptyColl, Map.empty))
    )
    ErgoMemPool.containsStorageRentClaim(rentTx) shouldBe true

    val (pool1, outcome) = ErgoMemPool.empty(rentSettings).process(UnconfirmedTransaction(rentTx, None), state)
    outcome shouldBe a[ProcessingOutcome.Accepted]
    val admitted = pool1.getAll.find(_.id == rentTx.id).get
    admitted.isUsingBlockchainContext shouldBe Some(true)
    admitted.validationResult shouldBe Some(true)

    val staleTime = System.currentTimeMillis() - cleanupDuration.toMillis * 10
    val unlabelledStale = new UnconfirmedTransaction(
      rentTx, admitted.lastCost, staleTime, staleTime, Some(rentTx.bytes), None,
      isUsingBlockchainContext = None, validationResult = None)
    val rechecked = Await.result(
      CleanupWorker.validatePool(state, new FakeMempool(Seq(unlabelledStale)), java.util.UUID.randomUUID(), rentSettings.nodeSettings),
      10.seconds)
    rechecked.invalidatedIds shouldBe empty
    rechecked.unconfirmedTxs.head.isUsingBlockchainContext shouldBe Some(true)
    rechecked.unconfirmedTxs.head.validationResult shouldBe Some(true)
  }
}
