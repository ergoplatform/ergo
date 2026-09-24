package org.ergoplatform.mining

import org.ergoplatform.consensus.ProgressInfo
import org.ergoplatform.core.idToVersion
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock}
import org.ergoplatform.modifiers.history.extension.ExtensionCandidate
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.popow.NipopowAlgos
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, OutputsHolder, UnconfirmedTransaction}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{BoxHolder, ErgoStateContext, StateType, UtxoState}
import org.ergoplatform.settings.{Constants, NetworkType, Parameters}
import org.ergoplatform.subblocks.InputBlockAnnouncement
import org.ergoplatform.utils.{HistoryTestHelpers, RandomWrapper}
import org.ergoplatform.utils.generators.ChainGenerator.applyChain
import org.ergoplatform.utils.generators.ValidBlocksGenerators.{createUtxoState, validTransactionsFromBoxHolder}
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
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

  private class Fixture(protocolVersion: Header.Version = Header.InitialVersion) {
    private val fixtureSettings = settings.copy(networkType =
      if (protocolVersion >= Header.Interpreter60Version) NetworkType.DevNet60 else NetworkType.Tests)
    // DevNet60 starts at V4 with the initial validation rules, matching the native
    // Matrix lifecycle fixture. Do not infer this from the shared Tests profile.
    private val fixtureParameters = Parameters(parameters.height,
      parameters.parametersTable.updated(Parameters.BlockVersion, protocolVersion.toInt),
      parameters.proposedUpdate)
    fixtureParameters.blockVersion shouldBe protocolVersion
    private val initial = createUtxoState(fixtureSettings, Some(fixtureParameters))
    var state: UtxoState = initial._1
    var history: ErgoHistory = HistoryTestHelpers.generateHistory(
      verifyTransactions = true, stateType = StateType.Utxo,
      PoPoWBootstrap = false, blocksToKeep = 100
    )

    private def proveOrdering(parent: Option[Header], rootDigest: ADDigest,
                              proof: SerializedAdProof, txs: Seq[ErgoTransaction], time: Long,
                              extension: ExtensionCandidate, votes: Array[Byte],
                              nBits: Long = settings.chainSettings.initialNBits): ErgoFullBlock = {
      // Retain the suite's existing test PoW scheme; state proofs and validation are real.
      powScheme.proveBlock(parent, protocolVersion, nBits, rootDigest, proof, txs, time,
        extension, votes, defaultMinerSecretNumber, Long.MinValue, Long.MaxValue, fixtureParameters) match {
        case org.ergoplatform.OrderingBlockFound(block) => block
        case _ => throw new IllegalStateException("Fixture did not produce an ordering block")
      }
    }

    private def validBlock(parent: Option[ErgoFullBlock], txs: Seq[ErgoTransaction], time: Long): ErgoFullBlock = {
      val (proof, digest) = state.proofsForTransactions(txs).get
      val interlinks = parent.toSeq.flatMap { block =>
        nipopowAlgos.updateInterlinks(block.header, NipopowAlgos.unpackInterlinks(block.extension.fields).get)
      }
      val extension = fixtureParameters.toExtensionCandidate ++
        nipopowAlgos.interlinksToExtension(interlinks) ++ state.stateContext.validationSettings.toExtensionCandidate
      proveOrdering(parent.map(_.header), digest, proof, txs, time, extension, Array.fill[Byte](3)(0))
    }

    private def build(parent: Option[ErgoFullBlock], boxes: BoxHolder, seed: Int,
                      time: Long): (ErgoFullBlock, BoxHolder) = {
      val (txs, nextBoxes) = validTransactionsFromBoxHolder(boxes, new RandomWrapper(Some(seed)))
      val block = validBlock(parent, txs, time)
      block.header.version shouldBe protocolVersion
      state = state.applyModifier(block, None)(_ => ()).get
      state.stateContext.blockVersion shouldBe protocolVersion
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
    // Keep a separate pool transaction outside the accepted input prefix. It can
    // be rejected during the rollback and selected again by the retry collection.
    private val poolInput = a2Pair._2.boxes.values.find { box =>
      box.ergoTree == Constants.TrueTree && a2OutputIds.contains(box.id.toSeq) &&
        !box.id.sameElements(input.id)
    }.get
    val poolTransaction: ErgoTransaction = ErgoTransaction(
      IndexedSeq(Input(poolInput.id, emptyProverResult)), IndexedSeq.empty,
      IndexedSeq(new ErgoBoxCandidate(poolInput.value, Constants.TrueTree, a2.height, poolInput.additionalTokens))
    )
    poolTransaction.statelessValidity().get
    private val absentInputId = ADKey @@ Array.fill(32)(0: Byte)
    state.boxById(absentInputId) shouldBe None
    val invalidTransaction: ErgoTransaction = ErgoTransaction(
      IndexedSeq(Input(absentInputId, emptyProverResult)), IndexedSeq.empty,
      IndexedSeq(new ErgoBoxCandidate(input.value, Constants.TrueTree, a2.height, input.additionalTokens))
    )
    private val emissionTransaction = CandidateGenerator.collectEmission(
      state, defaultMinerSecret.publicImage, state.stateContext
    ).get
    val pool: ErgoMemPool = ErgoMemPool.empty(fixtureSettings).put(
      Seq(transaction, poolTransaction, invalidTransaction).map(UnconfirmedTransaction(_, None))
    )
    history = applyChain(history, Seq(root, a2))

    // Publish the pool transaction in an input block on top of a2, so that the candidate
    // generator picks it up as a previously collected (ordering block) transaction, and it
    // lands in the transaction set the candidate's proofs are generated for.
    private val inputBlockHeader = validBlock(Some(a2), Seq(transaction), start + 2).header
    inputBlockHeader.version shouldBe protocolVersion
    private val inputBlock = InputBlockAnnouncement(1, inputBlockHeader, InputBlockFields.empty, None)
    history.applyInputBlock(inputBlock) shouldBe None
    private val (newBestInputBlocks, _) = history.applyInputBlockTransactions(inputBlock.id, Seq(transaction), state)
    newBestInputBlocks should contain(inputBlock.id)

    private val beforeRace = assemble(state).get.get
    beforeRace._1.candidateBlock.version shouldBe protocolVersion
    beforeRace._1.candidateBlock.transactions.map(_.id) should contain(transaction.id)
    (beforeRace._1.candidateBlock.inputBlockTransactions ++ beforeRace._1.candidateBlock.orderingBlockTransactions)
      .map(_.id) should contain(poolTransaction.id)
    beforeRace._2.ids should not contain transaction.id
    beforeRace._2.ids should contain(invalidTransaction.id)
    // Use the real candidate so the persistent-tip control certainly spends this input.
    val a3: ErgoFullBlock = materialize(beforeRace._1)
    a3.blockTransactions.txs.flatMap(_.inputs).exists(_.boxId.sameElements(input.id)) shouldBe true

    rollbackToRoot()
    private val b2Pair = build(Some(root), rootPair._2, 12, start + 4)
    private val validB2 = b2Pair._1
    private val wrongRoot = validB2.header.stateRoot.clone()
    wrongRoot(0) = (wrongRoot(0) ^ 1).toByte
    // Re-prove the header, keeping section commitments coherent. Only the declared
    // resulting state root is wrong; history can select this branch before execution.
    val b2: ErgoFullBlock = proveOrdering(
      Some(root.header),
      ADDigest @@ wrongRoot, validB2.adProofs.get.proofBytes,
      validB2.blockTransactions.txs, validB2.header.timestamp,
      ExtensionCandidate(validB2.extension.fields), validB2.header.votes,
      validB2.header.nBits
    )
    private val c3Transactions = validTransactionsFromBoxHolder(b2Pair._2, new RandomWrapper(Some(13)))._1
    val c3: ErgoFullBlock = validBlock(Some(b2), c3Transactions, start + 5)

    restoreOriginalState()
    history.bestFullBlockOpt.map(_.id) shouldBe Some(a2.id)
    state.stateContext.lastHeaderOpt.map(_.id) shouldBe Some(a2.id)

    var genuineProofFailures = 0
    var genuineBlockFailures = 0
    var collectionRollbacks = 0
    var retryCollectionRollbacks = 0
    var prefixValidationFailures = 0
    var prefixValidationVersions = Vector.empty[Header.Version]
    var lastPrefixFailure: Option[Throwable] = None
    var proofAttempts = 0

    private def rollbackToRoot(): Unit = {
      state.stateContext.blockVersion shouldBe protocolVersion
      state = state.rollbackTo(idToVersion(root.id)).get
      state.stateContext.blockVersion shouldBe protocolVersion
    }

    private def restoreOriginalState(): Unit = {
      rollbackToRoot()
      state = state.applyModifier(a2, None)(_ => ()).get
      state.stateContext.blockVersion shouldBe protocolVersion
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
      new UtxoState(state.persistentProver, state.version, state.store, fixtureSettings) {
        private var first = true
        private var failNextRetry = failRetryProof
        private var retryCollectionRolledBack = false
        private var progress: Option[ProgressInfo[BlockSection]] = None

        private def rollBackFork(): Unit = {
          progress = Some(selectFork())
          rollbackToRoot()
          state.boxById(input.id) shouldBe None
        }

        override def withTransactions(transactions: Seq[OutputsHolder]): UtxoState = {
          // The accepted prefix is always present. Interleave after emission and
          // before the separate pool transaction, whose real input rollback removes.
          val txIds = transactions.collect { case tx: ErgoTransaction => tx.id }
          val afterEmission = txIds.contains(emissionTransaction.id) &&
            !txIds.contains(poolTransaction.id)
          if (first && rollbackDuringCollection && progress.isEmpty && afterEmission) {
            rollBackFork()
            collectionRollbacks += 1
          }
          if (!first && rollbackDuringRetryCollection && !retryCollectionRolledBack && afterEmission) {
            rollbackToRoot()
            state.boxById(input.id) shouldBe None
            retryCollectionRolledBack = true
            retryCollectionRollbacks += 1
          }
          super.withTransactions(transactions)
        }

        override def proofsForTransactions(txs: Seq[ErgoTransaction]): Try[(SerializedAdProof, ADDigest)] = {
          proofAttempts += 1
          if (!first && failNextRetry) {
            failNextRetry = false
            if (rollbackDuringRetryCollection) {
              retryCollectionRolledBack shouldBe true
              txs.map(_.id) should contain(emissionTransaction.id)
              txs.map(_.id) should not contain poolTransaction.id
              // Note: on weak-blocks `transaction` stays in the proof set via
              // previously collected input-block transactions, regardless of the rollback.
            } else {
              txs.map(_.id) should contain(transaction.id)
              if (protocolVersion < Header.Interpreter60Version) {
                txs.map(_.id) should contain(poolTransaction.id)
              }
              rollbackToRoot()
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
                txs.map(_.id) should not contain poolTransaction.id
                // `transaction` is still in the proof set (collected via an earlier input
                // block), but its input was removed by the rollback, so the proof fails.
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
              val (updated, recovery) = history.reportModifierIsInvalid(b2).get
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

    def collectionFailureState(failures: Int): UtxoState =
      new UtxoState(state.persistentProver, state.version, state.store, fixtureSettings) {
        private var remainingFailures = failures

        override def withTransactions(transactions: Seq[OutputsHolder]): UtxoState = {
          val overlay = super.withTransactions(transactions)
          val txIds = transactions.collect { case tx: ErgoTransaction => tx.id }
          if (remainingFailures > 0 && txIds == Seq(transaction.id)) {
            new UtxoState(state.persistentProver, state.version, state.store, fixtureSettings) {
              override def validateWithCost(tx: ErgoTransaction, context: ErgoStateContext,
                                            costLimit: Int, interpreterOpt: Option[ErgoInterpreter],
                                            softFieldsAllowed: Boolean): Try[Int] = {
                tx.id shouldBe transaction.id
                context.sigmaPreHeader.version shouldBe protocolVersion
                prefixValidationVersions :+= context.sigmaPreHeader.version
                remainingFailures -= 1
                rollbackToRoot()
                state.boxById(input.id) shouldBe None
                try {
                  val result = overlay.validateWithCost(tx, context, costLimit, interpreterOpt, softFieldsAllowed)
                  result.isFailure shouldBe true
                  prefixValidationFailures += 1
                  lastPrefixFailure = Some(result.failed.get)
                  result
                } finally restoreOriginalState()
              }
            }
          } else overlay
        }

        override def proofsForTransactions(txs: Seq[ErgoTransaction]): Try[(SerializedAdProof, ADDigest)] = {
          proofAttempts += 1
          super.proofsForTransactions(txs)
        }
      }

    def assemble(reader: UtxoState) = {
      reader.stateContext.blockVersion shouldBe protocolVersion
      CandidateGenerator.generateCandidate(
        history, reader, pool, defaultMinerSecret.publicImage, Seq.empty, None, fixtureSettings
      )
    }

    def verifyCandidate(result: (CandidateGenerator.Candidate,
      org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.EliminateTransactions),
      emissionOnly: Boolean = false): Unit = {
      val (candidate, eliminate) = result
      val block = candidate.candidateBlock
      block.version shouldBe protocolVersion
      block.parentOpt.map(_.id) shouldBe Some(a2.id)
      if (emissionOnly) block.transactions shouldBe Seq(emissionTransaction)
      else {
        block.transactions.map(_.id) should contain(transaction.id)
        (block.inputBlockTransactions ++ block.orderingBlockTransactions).map(_.id) should contain(poolTransaction.id)
      }
      eliminate.ids should not contain transaction.id
      eliminate.ids should not contain poolTransaction.id
      // These fixtures use distinct transaction IDs and no competing spends.
      block.transactions.map(_.id).toSet.intersect(eliminate.ids.toSet) shouldBe empty
      if (emissionOnly) eliminate.ids shouldBe empty
      else eliminate.ids should contain(invalidTransaction.id)
      val retainedPool = eliminate.ids.foldLeft(pool)((current, id) => current.invalidate(id))
      retainedPool.getAllPrioritized.map(_.id) should contain(transaction.id)
      retainedPool.isInvalidated(transaction.id) shouldBe false
      retainedPool.getAllPrioritized.map(_.id) should contain(poolTransaction.id)
      retainedPool.isInvalidated(poolTransaction.id) shouldBe false
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
      block.version shouldBe protocolVersion
      proveOrdering(
        block.parentOpt, block.stateRoot,
        block.adProofBytes, block.transactions, block.timestamp, block.extension,
        block.votes, block.nBits
      )
    }

    def close(): Unit = {
      try history.closeStorage()
      finally state.closeStorage()
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
      fixture.collectionRollbacks shouldBe 1
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
      fixture.collectionRollbacks shouldBe 1
      fixture.genuineProofFailures shouldBe 2
      fixture.genuineBlockFailures shouldBe 1
      fixture.verifyCandidate(result, emissionOnly = true)
      fixture.proofAttempts shouldBe 3
    } finally fixture.close()
  }

  it should "discard stale retry rejections when the retry collection also overlaps a rollback" in {
    val fixture = new Fixture
    try {
      val reader = fixture.coordinatedState(
        returnToOriginal = true, failRetryProof = true, rollbackDuringRetryCollection = true
      )
      val result = fixture.assemble(reader).get.get
      fixture.retryCollectionRollbacks shouldBe 1
      fixture.genuineProofFailures shouldBe 2
      fixture.genuineBlockFailures shouldBe 1
      fixture.verifyCandidate(result, emissionOnly = true)
      fixture.proofAttempts shouldBe 3
    } finally fixture.close()
  }

  it should "retry a real accepted-prefix validation failure without eliminating pool transactions" in {
    val fixture = new Fixture
    try {
      val result = fixture.assemble(fixture.collectionFailureState(failures = 1)).get.get
      fixture.prefixValidationFailures shouldBe 1
      fixture.prefixValidationVersions shouldBe Vector(Header.InitialVersion)
      fixture.proofAttempts shouldBe 1
      fixture.verifyCandidate(result)
    } finally fixture.close()
  }

  it should "preserve version 1 emission-only fallback without elimination after both prefix validations fail" in {
    val fixture = new Fixture
    try {
      val result = fixture.assemble(fixture.collectionFailureState(failures = 2)).get.get
      fixture.prefixValidationFailures shouldBe 2
      fixture.prefixValidationVersions shouldBe Vector(Header.InitialVersion, Header.InitialVersion)
      fixture.proofAttempts shouldBe 1
      fixture.verifyCandidate(result, emissionOnly = true)
    } finally fixture.close()
  }

  it should "reject a version 4 candidate after both prefix validations fail without proving emission-only work" in {
    val fixture = new Fixture(Header.Interpreter60Version)
    try {
      val reader = fixture.collectionFailureState(failures = 2)
      reader.stateContext.blockVersion shouldBe Header.Interpreter60Version
      val result = fixture.assemble(reader).get
      fixture.prefixValidationFailures shouldBe 2
      fixture.prefixValidationVersions shouldBe Vector(Header.Interpreter60Version, Header.Interpreter60Version)
      result.isFailure shouldBe true
      result.failed.get shouldBe fixture.lastPrefixFailure.get
      fixture.proofAttempts shouldBe 0
    } finally fixture.close()
  }

  it should "reject version 4 work after two proof failures without proving emission-only work" in {
    val fixture = new Fixture(Header.Interpreter60Version)
    try {
      val reader = fixture.coordinatedState(
        returnToOriginal = true, rollbackDuringCollection = true, failRetryProof = true
      )
      val result = fixture.assemble(reader).get
      fixture.collectionRollbacks shouldBe 1
      fixture.genuineProofFailures shouldBe 2
      fixture.genuineBlockFailures shouldBe 1
      result.isFailure shouldBe true
      fixture.proofAttempts shouldBe 2
    } finally fixture.close()
  }

  it should "recover version 4 work after a real proof failure on a rejected fork" in {
    val fixture = new Fixture(Header.Interpreter60Version)
    try {
      val result = fixture.assemble(fixture.coordinatedState(returnToOriginal = true)).get.get
      result._1.candidateBlock.version shouldBe Header.Interpreter60Version
      fixture.genuineProofFailures shouldBe 1
      fixture.genuineBlockFailures shouldBe 1
      fixture.proofAttempts shouldBe 2
      fixture.verifyCandidate(result)
    } finally fixture.close()
  }
}
