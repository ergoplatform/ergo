package org.ergoplatform.mining

import org.ergoplatform.consensus.ProgressInfo
import org.ergoplatform.core.idToVersion
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock}
import org.ergoplatform.modifiers.history.extension.ExtensionCandidate
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{BoxHolder, StateType, UtxoState}
import org.ergoplatform.settings.Constants
import org.ergoplatform.utils.{HistoryTestHelpers, RandomWrapper}
import org.ergoplatform.utils.generators.ChainGenerator.applyChain
import org.ergoplatform.utils.generators.ValidBlocksGenerators.{createUtxoState, validFullBlock, validTransactionsFromBoxHolder}
import org.ergoplatform.{ErgoBoxCandidate, Input}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.crypto.authds.{ADDigest, ADKey, SerializedAdProof}

import scala.util.Try

/** Deterministically interleaves real history/state operations at the proof API boundary.
  * The fixture does not simulate actor scheduling or manufacture a proof failure.
  */
class CandidateRetryReorgSpec extends AnyFlatSpec with Matchers {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants.settings

  private class Fixture {
    private val initial = createUtxoState(settings)
    var state: UtxoState = initial._1
    var history: ErgoHistory = HistoryTestHelpers.generateHistory(
      verifyTransactions = true, stateType = StateType.Utxo,
      PoPoWBootstrap = false, blocksToKeep = 100
    )

    private def build(parent: Option[ErgoFullBlock], boxes: BoxHolder, seed: Int,
                      time: Long): (ErgoFullBlock, BoxHolder) = {
      val (txs, nextBoxes) = validTransactionsFromBoxHolder(boxes, new RandomWrapper(Some(seed)))
      val block = validFullBlock(parent, state, txs, Some(time))
      state = state.applyModifier(block, None)(_ => ()).get
      block -> nextBoxes
    }

    private val start = System.currentTimeMillis() - 10000
    private val rootPair = build(None, initial._2, 1, start)
    val root: ErgoFullBlock = rootPair._1
    private val a2Pair = build(Some(root), rootPair._2, 2, start + 1)
    val a2: ErgoFullBlock = a2Pair._1
    private val a2OutputIds = a2.blockTransactions.txs.flatMap(_.outputs).map(_.id.toSeq).toSet
    val input = a2Pair._2.boxes.values.find { box =>
      box.ergoTree == Constants.TrueTree && a2OutputIds.contains(box.id.toSeq)
    }.get
    val transaction: ErgoTransaction = ErgoTransaction(
      IndexedSeq(Input(input.id, emptyProverResult)), IndexedSeq.empty,
      IndexedSeq(new ErgoBoxCandidate(input.value, Constants.TrueTree, a2.height, input.additionalTokens))
    )
    transaction.statelessValidity().get
    private val absentInputId = ADKey @@ Array.fill(32)(0: Byte)
    state.boxById(absentInputId) shouldBe None
    val invalidTransaction: ErgoTransaction = ErgoTransaction(
      IndexedSeq(Input(absentInputId, emptyProverResult)), IndexedSeq.empty,
      IndexedSeq(new ErgoBoxCandidate(input.value, Constants.TrueTree, a2.height, input.additionalTokens))
    )
    private val emissionTransaction = CandidateGenerator.collectEmission(
      state, defaultMinerSecret.publicImage, state.stateContext
    ).get
    val pool: ErgoMemPool = ErgoMemPool.empty(settings).put(
      Seq(transaction, invalidTransaction).map(UnconfirmedTransaction(_, None))
    )
    history = applyChain(history, Seq(root, a2))
    private val beforeRace = assemble(state).get.get
    beforeRace._1.candidateBlock.transactions.map(_.id) should contain(transaction.id)
    beforeRace._2.ids should not contain transaction.id
    beforeRace._2.ids should contain(invalidTransaction.id)
    // Use the real candidate so the persistent-tip control certainly spends this input.
    val a3: ErgoFullBlock = materialize(beforeRace._1)
    a3.blockTransactions.txs.flatMap(_.inputs).exists(_.boxId.sameElements(input.id)) shouldBe true

    state = state.rollbackTo(idToVersion(root.id)).get
    private val b2Pair = build(Some(root), rootPair._2, 12, start + 4)
    private val validB2 = b2Pair._1
    private val wrongRoot = validB2.header.stateRoot.clone()
    wrongRoot(0) = (wrongRoot(0) ^ 1).toByte
    // Re-prove the header, keeping section commitments coherent. Only the declared
    // resulting state root is wrong; history can select this branch before execution.
    val b2: ErgoFullBlock = powScheme.proveBlock(
      Some(root.header), validB2.header.version, validB2.header.nBits,
      ADDigest @@ wrongRoot, validB2.adProofs.get.proofBytes,
      validB2.blockTransactions.txs, validB2.header.timestamp,
      ExtensionCandidate(validB2.extension.fields), validB2.header.votes,
      defaultMinerSecretNumber
    ).get
    private val c3Transactions = validTransactionsFromBoxHolder(b2Pair._2, new RandomWrapper(Some(13)))._1
    val c3: ErgoFullBlock = validFullBlock(Some(b2), state, c3Transactions, Some(start + 5))

    restoreOriginalState()
    history.bestFullBlockOpt.map(_.id) shouldBe Some(a2.id)
    state.stateContext.lastHeaderOpt.map(_.id) shouldBe Some(a2.id)

    var genuineProofFailures = 0
    var genuineBlockFailures = 0

    private def restoreOriginalState(): Unit = {
      state = state.rollbackTo(idToVersion(root.id)).get
      state = state.applyModifier(a2, None)(_ => ()).get
    }

    private def selectFork(): ProgressInfo[BlockSection] = {
      history = applyChain(history, Seq(b2))
      history.bestFullBlockOpt.map(_.id) shouldBe Some(a2.id)
      var progress: Option[ProgressInfo[BlockSection]] = None
      val sections: Seq[BlockSection] = Seq(c3.header, c3.extension, c3.blockTransactions) ++ c3.adProofs.toSeq
      sections.foreach { section =>
        val (updated, step) = history.append(section).get
        history = updated
        if (step.toApply.nonEmpty) progress = Some(step)
      }
      history.bestFullBlockOpt.map(_.id) shouldBe Some(c3.id)
      val selected = progress.get
      selected.branchPoint shouldBe Some(root.id)
      selected.toRemove.map(_.id) shouldBe Seq(a2.id)
      selected.toApply.map(_.id) shouldBe Seq(b2.id, c3.id)
      selected
    }

    def coordinatedState(returnToOriginal: Boolean, rollbackDuringCollection: Boolean = false,
                         failRetryProof: Boolean = false, rollbackDuringRetryCollection: Boolean = false): UtxoState =
      new UtxoState(state.persistentProver, state.version, state.store, settings) {
        private var first = true
        private var failNextRetry = failRetryProof
        private var retryCollectionRolledBack = false
        private var progress: Option[ProgressInfo[BlockSection]] = None

        private def rollBackFork(): Unit = {
          progress = Some(selectFork())
          state = state.rollbackTo(idToVersion(root.id)).get
          state.boxById(input.id) shouldBe None
        }

        override def withTransactions(transactions: Seq[ErgoTransaction]): UtxoState = {
          // The emission transaction has been selected, but the pool transaction
          // has not been checked yet. Rollback removes the latter's actual input.
          val afterEmission = transactions.exists(_.id == emissionTransaction.id) &&
            !transactions.exists(_.id == transaction.id)
          if (first && rollbackDuringCollection && progress.isEmpty && afterEmission) {
            rollBackFork()
          }
          if (!first && rollbackDuringRetryCollection && !retryCollectionRolledBack && afterEmission) {
            state = state.rollbackTo(idToVersion(root.id)).get
            state.boxById(input.id) shouldBe None
            retryCollectionRolledBack = true
          }
          super.withTransactions(transactions)
        }

        override def proofsForTransactions(txs: Seq[ErgoTransaction]): Try[(SerializedAdProof, ADDigest)] = {
          if (!first && failNextRetry) {
            failNextRetry = false
            if (rollbackDuringRetryCollection) {
              retryCollectionRolledBack shouldBe true
              txs.map(_.id) should contain(emissionTransaction.id)
              txs.map(_.id) should not contain transaction.id
            } else {
              txs.map(_.id) should contain(transaction.id)
              state = state.rollbackTo(idToVersion(root.id)).get
            }
            state.boxById(input.id) shouldBe None
            val retryResult = super.proofsForTransactions(txs)
            retryResult.isFailure shouldBe true
            genuineProofFailures += 1
            restoreOriginalState()
            state.stateContext.lastHeaderOpt.map(_.id) shouldBe Some(a2.id)
            retryResult
          } else if (!first) super.proofsForTransactions(txs)
          else {
            first = false
            if (returnToOriginal) {
              if (progress.isEmpty) rollBackFork()
              if (rollbackDuringCollection) {
                txs.map(_.id) should contain(emissionTransaction.id)
                txs.map(_.id) should not contain transaction.id
                state.boxById(emissionTransaction.inputs.head.boxId) shouldBe None
              }
            } else {
              history = applyChain(history, Seq(a3))
              state = state.applyModifier(a3, None)(_ => ()).get
              state.boxById(input.id) shouldBe None
            }
            // This is the actual production proof operation against the intervening state.
            val firstResult = super.proofsForTransactions(txs)
            firstResult.isFailure shouldBe true
            genuineProofFailures += 1
            if (returnToOriginal) {
              state.applyModifier(b2, None)(_ => ()).isFailure shouldBe true
              genuineBlockFailures += 1
              val (updated, recovery) = history.reportModifierIsInvalid(b2, progress.get).get
              history = updated
              history.bestFullBlockOpt.map(_.id) shouldBe Some(a2.id)
              recovery.branchPoint shouldBe Some(root.id)
              recovery.toRemove.map(_.id) shouldBe Seq(b2.id, c3.id)
              recovery.toApply.map(_.id) shouldBe Seq(a2.id)
              state = state.rollbackTo(idToVersion(recovery.branchPoint.get)).get
              recovery.toApply.foreach(block => state = state.applyModifier(block, None)(_ => ()).get)
              state.stateContext.lastHeaderOpt.map(_.id) shouldBe Some(a2.id)
            }
            firstResult
          }
        }
      }

    def assemble(reader: UtxoState) = CandidateGenerator.generateCandidate(
      history, reader, pool, defaultMinerSecret.publicImage, Seq.empty, None, settings
    )

    def verifyCandidate(result: (CandidateGenerator.Candidate,
      org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.EliminateTransactions),
      emissionOnly: Boolean = false): Unit = {
      val (candidate, eliminate) = result
      val block = candidate.candidateBlock
      block.parentOpt.map(_.id) shouldBe Some(a2.id)
      if (emissionOnly) block.transactions shouldBe Seq(emissionTransaction)
      else block.transactions.map(_.id) should contain(transaction.id)
      eliminate.ids should not contain transaction.id
      // These fixtures use distinct transaction IDs and no competing spends.
      block.transactions.map(_.id).toSet.intersect(eliminate.ids.toSet) shouldBe empty
      if (emissionOnly) eliminate.ids shouldBe empty
      else eliminate.ids should contain(invalidTransaction.id)
      val retainedPool = eliminate.ids.foldLeft(pool)((current, id) => current.invalidate(id))
      retainedPool.getAllPrioritized.map(_.id) should contain(transaction.id)
      retainedPool.isInvalidated(transaction.id) shouldBe false
      if (emissionOnly) {
        retainedPool.getAllPrioritized.map(_.id) should contain(invalidTransaction.id)
        retainedPool.isInvalidated(invalidTransaction.id) shouldBe false
      } else {
        retainedPool.getAllPrioritized.map(_.id) should not contain invalidTransaction.id
        retainedPool.isInvalidated(invalidTransaction.id) shouldBe true
      }
      state.applyModifier(materialize(candidate), None)(_ => ()).isSuccess shouldBe true
    }

    private def materialize(candidate: CandidateGenerator.Candidate): ErgoFullBlock = {
      val block = candidate.candidateBlock
      powScheme.proveBlock(
        block.parentOpt, block.version, block.nBits, block.stateRoot,
        block.adProofBytes, block.transactions, block.timestamp, block.extension,
        block.votes, defaultMinerSecretNumber
      ).get
    }

    def close(): Unit = {
      history.closeStorage()
      state.closeStorage()
    }
  }

  it should "retain pool transactions after a real proof failure during a rejected fork" in {
    val fixture = new Fixture
    try {
      val result = fixture.assemble(fixture.coordinatedState(returnToOriginal = true)).get.get
      fixture.genuineProofFailures shouldBe 1
      fixture.genuineBlockFailures shouldBe 1
      fixture.verifyCandidate(result)
    } finally fixture.close()
  }

  it should "discard assembly after a persistent tip change" in {
    val fixture = new Fixture
    try {
      fixture.assemble(fixture.coordinatedState(returnToOriginal = false)) shouldBe None
      fixture.genuineProofFailures shouldBe 1
      fixture.genuineBlockFailures shouldBe 0
    } finally fixture.close()
  }

  it should "keep a transaction selected on retry out of the elimination list" in {
    val fixture = new Fixture
    try {
      val reader = fixture.coordinatedState(returnToOriginal = true, rollbackDuringCollection = true)
      val result = fixture.assemble(reader).get.get
      fixture.genuineProofFailures shouldBe 1
      fixture.genuineBlockFailures shouldBe 1
      fixture.verifyCandidate(result)
    } finally fixture.close()
  }

  it should "retain pool transactions when no state change interrupts assembly" in {
    val fixture = new Fixture
    try {
      fixture.verifyCandidate(fixture.assemble(fixture.state).get.get)
      fixture.genuineProofFailures shouldBe 0
    } finally fixture.close()
  }

  it should "retain the retry-selected transaction when a second proof failure requires emission-only fallback" in {
    val fixture = new Fixture
    try {
      val reader = fixture.coordinatedState(
        returnToOriginal = true, rollbackDuringCollection = true, failRetryProof = true
      )
      val result = fixture.assemble(reader).get.get
      fixture.genuineProofFailures shouldBe 2
      fixture.genuineBlockFailures shouldBe 1
      fixture.verifyCandidate(result, emissionOnly = true)
    } finally fixture.close()
  }

  it should "discard stale retry rejections when the retry collection also overlaps a rollback" in {
    val fixture = new Fixture
    try {
      val reader = fixture.coordinatedState(
        returnToOriginal = true, failRetryProof = true, rollbackDuringRetryCollection = true
      )
      val result = fixture.assemble(reader).get.get
      fixture.genuineProofFailures shouldBe 2
      fixture.genuineBlockFailures shouldBe 1
      fixture.verifyCandidate(result, emissionOnly = true)
    } finally fixture.close()
  }
}
