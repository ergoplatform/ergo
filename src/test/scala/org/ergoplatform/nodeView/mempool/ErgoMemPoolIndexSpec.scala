package org.ergoplatform.nodeView.mempool

import org.ergoplatform.{ErgoBoxCandidate, Input}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.{ProcessingOutcome, SortingOption}
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.utils.ErgoTestHelpers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sigma.ast.ByteArrayConstant
import sigma.interpreter.{ContextExtension, ProverResult}

class ErgoMemPoolIndexSpec extends AnyFlatSpec
  with ErgoTestHelpers
  with ScalaCheckPropertyChecks {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._
  import org.ergoplatform.utils.generators.ValidBlocksGenerators._

  it should "maintain TxFamilyGraph consistent with outputs map across put/invalidate" in {
    val feeProposition = settings.chainSettings.monetary.feeProposition

    val (us, bh) = createUtxoState(settings)
    val genesis = validFullBlock(None, us, bh)
    val wus = WrappedUtxoState(us, bh, settings).applyModifier(genesis)(_ => ()).get
    var txs = validTransactionsFromUtxoState(wus).map(tx => UnconfirmedTransaction(tx, None))
    val family_depth = 5
    val limitedPoolSettings = settings.copy(
      nodeSettings = settings.nodeSettings.copy(mempoolCapacity = (family_depth + 1) * txs.size))
    var pool = ErgoMemPool.empty(limitedPoolSettings)

    def checkInvariant(): Unit = {
      val p = pool.pool
      val expectedParents = p.orderedTransactions.values.flatMap { utx =>
        val pids = utx.transaction.inputs.flatMap(in => p.outputs.get(in.boxId)).toSet
        if (pids.isEmpty) None else Some(utx.transaction.id -> pids)
      }.toMap
      val expectedChildren = expectedParents.toSeq
        .flatMap { case (child, parents) => parents.map(_ -> child) }
        .groupBy(_._1)
        .map { case (parent, edges) => parent -> edges.map(_._2).toSet }
      val expectedReadParents = p.orderedTransactions.values.flatMap { utx =>
        val pids = utx.transaction.dataInputs.flatMap(in => p.outputs.get(in.boxId)).toSet
        if (pids.isEmpty) None else Some(utx.transaction.id -> pids)
      }.toMap
      val expectedReadChildren = expectedReadParents.toSeq
        .flatMap { case (child, parents) => parents.map(_ -> child) }
        .groupBy(_._1)
        .map { case (parent, edges) => parent -> edges.map(_._2).toSet }
      val expectedDataInputReaders = p.orderedTransactions.values
        .flatMap(utx => utx.transaction.dataInputs.map(_.boxId -> utx.id))
        .groupBy(_._1)
        .map { case (boxId, readers) => boxId -> readers.map(_._2).toSet }
      p.family.parents shouldBe expectedParents
      p.family.children shouldBe expectedChildren
      p.family.readParents shouldBe expectedReadParents
      p.family.readChildren shouldBe expectedReadChildren
      p.dataInputReaders shouldBe expectedDataInputReaders
      val orderedIds = p.orderedTransactions.valuesIterator.map(_.id).toVector
      orderedIds.distinct.size shouldBe orderedIds.size
      p.transactionsRegistry.keySet shouldBe orderedIds.toSet
    }

    txs.foreach { tx =>
      pool = pool.put(tx)
      checkInvariant()
    }

    for (_ <- 1 to family_depth) {
      txs = txs.map { tx =>
        val spendingBox = tx.transaction.outputs.head
        val sc = spendingBox.toCandidate
        val out0 = new ErgoBoxCandidate(sc.value - 55000, sc.ergoTree, sc.creationHeight)
        val out1 = new ErgoBoxCandidate(55000, feeProposition, sc.creationHeight)
        val newTx = UnconfirmedTransaction(tx.transaction.copy(
          inputs = IndexedSeq(new Input(spendingBox.id, emptyProverResult)),
          outputCandidates = IndexedSeq(out0, out1)), None)
        val (newPool, outcome) = pool.process(newTx, us)
        outcome.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true
        pool = newPool
        checkInvariant()
        newTx
      }
    }

    while (pool.size > 0) {
      val victim = pool.getAll.head
      pool = pool.invalidate(victim)
      checkInvariant()
    }

    pool.pool.family.parents shouldBe empty
    pool.pool.family.children shouldBe empty
  }

  it should "preserve the inputs index after replace-by-fee" in {
    // With put-then-remove ordering, the new tx's inputs entry overwrites the loser's,
    // and the subsequent loser-remove deletes the shared box id from `inputs`,
    // leaving the new (winning) tx with NO inputs index entry for the shared box.
    // Downstream double-spend detection would then miss further conflicts.
    forAll(smallPositiveInt, smallPositiveInt) { case (n1, n2) =>
      whenever(n1 != n2 && n1 < n2) {
        val testSettings = settings.copy(
          nodeSettings = settings.nodeSettings.copy(mempoolSorting = SortingOption.FeePerByte)
        )
        val (us, bh) = createUtxoState(testSettings)
        val genesis = validFullBlock(None, us, bh)
        val wus = WrappedUtxoState(us, bh, testSettings).applyModifier(genesis)(_ => ()).get

        val feeProp = testSettings.chainSettings.monetary.feeProposition
        val inputBox = wus.takeBoxes(100).collectFirst {
          case box if box.ergoTree == TrueTree => box
        }.get
        val feeOut = new ErgoBoxCandidate(
          inputBox.value,
          feeProp,
          creationHeight = 0,
          additionalTokens = inputBox.additionalTokens
        )

        def ctx(n: Int): ContextExtension =
          ContextExtension(Map((1: Byte) -> ByteArrayConstant(Array.fill(1 + n)(0: Byte))))

        // A larger context makes the transaction larger, so n1 < n2 gives the
        // replacement transaction a higher fee-per-byte ratio at the same fee.
        val txLargeLike = ErgoTransaction(
          IndexedSeq(new Input(inputBox.id, new ProverResult(Array.emptyByteArray, ctx(n2)))),
          IndexedSeq(feeOut))
        val txSmallLike = ErgoTransaction(
          IndexedSeq(new Input(inputBox.id, new ProverResult(Array.emptyByteArray, ctx(n1)))),
          IndexedSeq(feeOut))

        val txLarge = UnconfirmedTransaction(ErgoTransaction(txLargeLike.inputs, txLargeLike.outputCandidates), None)
        val txSmall = UnconfirmedTransaction(ErgoTransaction(txSmallLike.inputs, txSmallLike.outputCandidates), None)

        val pool0 = ErgoMemPool.empty(testSettings)
        val (poolWithLarge, oLarge) = pool0.process(txLarge, us)
        oLarge.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true

        val (poolWithSmall, oSmall) = poolWithLarge.process(txSmall, us)
        oSmall.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true
        poolWithSmall.size shouldBe 1
        poolWithSmall.take(1).head.transaction.id shouldBe txSmall.transaction.id

        // The crux: inputs index must point to the winner (txSmall) for the shared input box.
        // With the buggy put-then-remove order this is `None`.
        poolWithSmall.pool.inputs.get(inputBox.id) shouldBe Some(txSmall.transaction.id)
      }
    }
  }

  it should "resolve an unregistered ordered conflict without dividing by zero" in {
    val testSettings = settings.copy(
      nodeSettings = settings.nodeSettings.copy(mempoolSorting = SortingOption.FeePerByte)
    )
    val (us, bh) = createUtxoState(testSettings)
    val genesis = validFullBlock(None, us, bh)
    val wus = WrappedUtxoState(us, bh, testSettings).applyModifier(genesis)(_ => ()).get
    val feeProp = testSettings.chainSettings.monetary.feeProposition
    val inputBox = wus.takeBoxes(100).collectFirst {
      case box if box.ergoTree == TrueTree => box
    }.get
    val feeOut = new ErgoBoxCandidate(
      inputBox.value,
      feeProp,
      creationHeight = 0,
      additionalTokens = inputBox.additionalTokens
    )

    def ctx(size: Int): ContextExtension =
      ContextExtension(Map((1: Byte) -> ByteArrayConstant(Array.fill(size)(0: Byte))))

    def spendingTx(contextSize: Int): UnconfirmedTransaction = {
      val txLike = ErgoTransaction(
        IndexedSeq(new Input(inputBox.id, new ProverResult(Array.emptyByteArray, ctx(contextSize)))),
        IndexedSeq(feeOut)
      )
      UnconfirmedTransaction(ErgoTransaction(txLike.inputs, txLike.outputCandidates), None)
    }

    val loser = spendingTx(contextSize = 64)
    val winner = spendingTx(contextSize = 1)
    val (healthy, accepted) = ErgoMemPool.empty(testSettings).process(loser, us)
    accepted.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true

    val p = healthy.pool
    val orphanedPool = new OrderedTxPool(
      p.orderedTransactions,
      p.transactionsRegistry - loser.id,
      p.invalidatedTxIds,
      p.outputs,
      p.inputs,
      p.dataInputReaders,
      p.family
    )(testSettings)
    val orphaned = new ErgoMemPool(
      orphanedPool,
      healthy.stats,
      healthy.sortingOption
    )(testSettings)

    val (replaced, outcome) = orphaned.process(winner, us)
    outcome.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true
    replaced.getAll.map(_.id) shouldBe Seq(winner.id)
    replaced.pool.inputs(inputBox.id) shouldBe winner.id
  }

  it should "ignore an unresolvable inputs-index ghost without dividing by zero" in {
    val (us, bh) = createUtxoState(settings)
    val genesis = validFullBlock(None, us, bh)
    val wus = WrappedUtxoState(us, bh, settings).applyModifier(genesis)(_ => ()).get
    val inputBox = wus.takeBoxes(100).collectFirst {
      case box if box.ergoTree == TrueTree => box
    }.get
    val feeOut = new ErgoBoxCandidate(
      inputBox.value,
      feeProp,
      creationHeight = 0,
      additionalTokens = inputBox.additionalTokens
    )

    def spendingTx(contextSize: Int): UnconfirmedTransaction = {
      val context = ContextExtension(
        Map((1: Byte) -> ByteArrayConstant(Array.fill(contextSize)(0: Byte)))
      )
      val txLike = ErgoTransaction(
        IndexedSeq(new Input(
          inputBox.id,
          new ProverResult(Array.emptyByteArray, context)
        )),
        IndexedSeq(feeOut)
      )
      UnconfirmedTransaction(ErgoTransaction(txLike.inputs, txLike.outputCandidates), None)
    }

    val ghost = spendingTx(contextSize = 64)
    val winner = spendingTx(contextSize = 1)
    val empty = ErgoMemPool.empty(settings)
    val p = empty.pool
    val corruptedPool = new OrderedTxPool(
      p.orderedTransactions,
      p.transactionsRegistry,
      p.invalidatedTxIds,
      p.outputs,
      p.inputs.updated(inputBox.id, ghost.id),
      p.dataInputReaders,
      p.family
    )(settings)
    val corrupted = new ErgoMemPool(
      corruptedPool,
      empty.stats,
      empty.sortingOption
    )(settings)

    corrupted.pool.currentTransaction(ghost.id) shouldBe None
    val (acceptedPool, outcome) = corrupted.process(winner, us)

    outcome.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true
    acceptedPool.getAll.map(_.id) shouldBe Seq(winner.id)
    acceptedPool.pool.inputs(inputBox.id) shouldBe winner.id
  }
}
