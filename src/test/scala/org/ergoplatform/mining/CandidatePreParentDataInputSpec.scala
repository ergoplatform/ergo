package org.ergoplatform.mining

import org.ergoplatform.{DataInput, ErgoBox, ErgoBoxCandidate, Input}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.ProcessingOutcome
import org.ergoplatform.nodeView.state.{BoxHolder, UtxoState}
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.settings.{NetworkType, Parameters}
import org.ergoplatform.utils.{ErgoCompilerHelpers, ErgoCorePropertyTest}
import org.ergoplatform.validation.SoftFieldsAccessError
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.ergoplatform.wallet.utils.WalletGenerators.ergoBoxGen
import org.scalacheck.Gen
import scorex.crypto.authds.ADKey
import sigma.ast.ErgoTree

class CandidatePreParentDataInputSpec extends ErgoCorePropertyTest with ErgoCompilerHelpers {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants.settings
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._
  import org.ergoplatform.utils.generators.ValidBlocksGenerators._

  property("defer a prioritized data-input reader until its pending producer without invalidating it") {
    def inputBox(value: Long): ErgoBox = ergoBoxGen(
      propGen = trueLeafGen,
      tokensGen = Gen.const(Seq.empty),
      valueGenOpt = Some(Gen.const(value)),
      heightGen = Gen.const(0)
    ).sample.get

    val parentInput = inputBox(1000000000L)
    val readerInput = inputBox(1000000000L)
    val missingReaderInput = inputBox(1000000000L)
    val conflictInput = inputBox(1000000000L)
    val boxHolder = BoxHolder(Seq(parentInput, readerInput, missingReaderInput, conflictInput))
    val us = createUtxoState(boxHolder, parameters)

    def outputs(input: ErgoBox, fee: Long): IndexedSeq[ErgoBoxCandidate] =
      IndexedSeq(
        new ErgoBoxCandidate(input.value - fee, TrueTree, input.creationHeight),
        new ErgoBoxCandidate(fee, feeProp, input.creationHeight)
      )

    val parent = ErgoTransaction(
      IndexedSeq(new Input(parentInput.id, emptyProverResult)),
      IndexedSeq.empty,
      outputs(parentInput, fee = 1000000L)
    )
    val reader = ErgoTransaction(
      IndexedSeq(new Input(readerInput.id, emptyProverResult)),
      IndexedSeq(DataInput(parent.outputs.head.id)),
      outputs(readerInput, fee = 100000000L)
    )
    val missingReader = ErgoTransaction(
      IndexedSeq(new Input(missingReaderInput.id, emptyProverResult)),
      IndexedSeq(DataInput(ADKey @@ Array.fill(32)(42.toByte))),
      outputs(missingReaderInput, fee = 100000000L)
    )
    val futureAndMissingReader = ErgoTransaction(
      IndexedSeq(new Input(missingReaderInput.id, emptyProverResult)),
      IndexedSeq(
        DataInput(parent.outputs.head.id),
        DataInput(ADKey @@ Array.fill(32)(43.toByte))
      ),
      outputs(missingReaderInput, fee = 100000000L)
    )
    val selectedSpend = ErgoTransaction(
      IndexedSeq(new Input(conflictInput.id, emptyProverResult)),
      IndexedSeq.empty,
      outputs(conflictInput, fee = 1000000L)
    )
    val futureAndSelectedConflict = ErgoTransaction(
      IndexedSeq(new Input(conflictInput.id, emptyProverResult)),
      IndexedSeq(DataInput(parent.outputs.head.id)),
      outputs(conflictInput, fee = 2000000L)
    )
    val selectedAlternative = ErgoTransaction(
      IndexedSeq(new Input(conflictInput.id, emptyProverResult)),
      IndexedSeq.empty,
      outputs(conflictInput, fee = 3000000L)
    )

    val emptyPool = ErgoMemPool.empty(settings)
    val (parentPool, parentOutcome) = emptyPool.process(UnconfirmedTransaction(parent, None), us)
    parentOutcome shouldBe a[ProcessingOutcome.Accepted]
    val (pool, readerOutcome) = parentPool.process(UnconfirmedTransaction(reader, None), us)
    readerOutcome shouldBe a[ProcessingOutcome.Accepted]
    pool.getAllPrioritized.map(_.id) should contain theSameElementsAs Seq(reader.id, parent.id)

    val h = validFullBlock(None, us, boxHolder).header
    val upcomingContext = us.stateContext.upcoming(
      h.minerPk, h.timestamp, h.nBits, h.votes, emptyVSUpdate, h.version
    )
    upcomingContext.sigmaPreHeader.version should be < Header.Interpreter60Version

    val collected = CandidateGenerator.collectTxs(
      defaultMinerPk,
      parameters.maxBlockCost,
      parameters.maxBlockSize,
      us,
      upcomingContext,
      Seq(reader, parent)
    )
    (collected._1 ++ collected._2) should contain(parent)
    (collected._1 ++ collected._2) should not contain reader
    collected._3 shouldBe empty

    val retainedPool = collected._3.foldLeft(pool)((current, id) => current.invalidate(id))
    retainedPool.getAllPrioritized.map(_.id) should contain(reader.id)
    retainedPool.isInvalidated(reader.id) shouldBe false

    val futureAndMissing = CandidateGenerator.collectTxs(
      defaultMinerPk,
      parameters.maxBlockCost,
      parameters.maxBlockSize,
      us,
      upcomingContext,
      Seq(futureAndMissingReader, parent)
    )
    futureAndMissing._3 shouldBe Seq(futureAndMissingReader.id)

    val futureAndConflict = CandidateGenerator.collectTxs(
      defaultMinerPk,
      parameters.maxBlockCost,
      parameters.maxBlockSize,
      us,
      upcomingContext,
      Seq(selectedSpend, futureAndSelectedConflict, parent)
    )
    (futureAndConflict._1 ++ futureAndConflict._2) should contain(selectedSpend)
    futureAndConflict._3 shouldBe empty

    val selectedOnlyConflict = CandidateGenerator.collectTxs(
      defaultMinerPk, parameters.maxBlockCost, parameters.maxBlockSize,
      us, upcomingContext, Seq(selectedSpend, selectedAlternative)
    )
    (selectedOnlyConflict._1 ++ selectedOnlyConflict._2) should contain(selectedSpend)
    (selectedOnlyConflict._1 ++ selectedOnlyConflict._2) should not contain selectedAlternative
    selectedOnlyConflict._3 shouldBe empty

    val mandatoryPrefixConflict = CandidateGenerator.collectTxs(
      defaultMinerPk, parameters.maxBlockCost, parameters.maxBlockSize,
      us, upcomingContext, Seq(futureAndSelectedConflict, parent), Seq(selectedSpend)
    )
    mandatoryPrefixConflict._3 shouldBe empty

    val prefixOnlyConflict = CandidateGenerator.collectTxs(
      defaultMinerPk, parameters.maxBlockCost, parameters.maxBlockSize,
      us, upcomingContext, Seq(selectedAlternative), Seq(selectedSpend)
    )
    prefixOnlyConflict._3 shouldBe Seq(selectedAlternative.id)

    val missing = CandidateGenerator.collectTxs(
      defaultMinerPk,
      parameters.maxBlockCost,
      parameters.maxBlockSize,
      us,
      upcomingContext,
      Seq(missingReader)
    )
    missing._3 shouldBe Seq(missingReader.id)
  }

  property("do not permanently invalidate a V6 ordering alternative with a future data input") {
    val matrixParameters = Parameters(parameters.height,
      parameters.parametersTable.updated(Parameters.BlockVersion, Header.Interpreter60Version.toInt),
      parameters.proposedUpdate)
    val matrixSettings = settings.copy(networkType = NetworkType.DevNet60)
    val softTree = compileSourceV5("CONTEXT.minerPubKey.size >= 0", 0)

    def inputBox(tree: ErgoTree, value: Long): ErgoBox = ergoBoxGen(
      propGen = Gen.const(tree),
      tokensGen = Gen.const(Seq.empty),
      valueGenOpt = Some(Gen.const(value)),
      heightGen = Gen.const(0)
    ).sample.get

    val sharedInput = inputBox(softTree, 10000000L)
    val parentInput = inputBox(TrueTree, 10000000L)
    val us = UtxoState.fromBoxHolder(
      BoxHolder(Seq(sharedInput, parentInput)), None, createTempDir, matrixSettings, matrixParameters
    )
    val upcomingContext = us.stateContext.upcoming(
      defaultMinerPk.value, System.currentTimeMillis(), matrixSettings.chainSettings.initialNBits,
      Array.fill[Byte](3)(0), emptyVSUpdate, Header.Interpreter60Version
    )
    val verifier = ErgoInterpreter(upcomingContext.currentParameters)

    val parent = ErgoTransaction(
      IndexedSeq(new Input(parentInput.id, emptyProverResult)), IndexedSeq.empty,
      IndexedSeq(new ErgoBoxCandidate(parentInput.value, TrueTree, 0))
    )
    val ordering = ErgoTransaction(
      IndexedSeq(new Input(sharedInput.id, emptyProverResult)), IndexedSeq.empty,
      IndexedSeq(new ErgoBoxCandidate(sharedInput.value, TrueTree, 0))
    )
    val reader = ErgoTransaction(
      IndexedSeq(new Input(sharedInput.id, emptyProverResult)),
      IndexedSeq(DataInput(parent.outputs.head.id)),
      IndexedSeq(new ErgoBoxCandidate(sharedInput.value, TrueTree, 0))
    )

    us.validateWithCost(ordering, upcomingContext, matrixParameters.maxBlockCost,
      Some(verifier), softFieldsAllowed = false).failed.get shouldBe a[SoftFieldsAccessError]
    us.validateWithCost(ordering, upcomingContext, matrixParameters.maxBlockCost,
      Some(verifier), softFieldsAllowed = true).isSuccess shouldBe true
    us.withTransactions(Seq(parent)).validateWithCost(reader, upcomingContext,
      matrixParameters.maxBlockCost, Some(verifier), softFieldsAllowed = true).isSuccess shouldBe true

    val emptyPool = ErgoMemPool.empty(matrixSettings)
    val (parentPool, parentOutcome) = emptyPool.process(UnconfirmedTransaction(parent, None), us)
    parentOutcome shouldBe a[ProcessingOutcome.Accepted]
    val (pool, readerOutcome) = parentPool.process(UnconfirmedTransaction(reader, None), us)
    readerOutcome shouldBe a[ProcessingOutcome.Accepted]
    pool.getAllPrioritized.map(_.id) should contain theSameElementsAs Seq(reader.id, parent.id)

    val conflicted = CandidateGenerator.collectTxs(
      defaultMinerPk, matrixParameters.maxBlockCost, matrixParameters.maxBlockSize,
      us, upcomingContext, Seq(ordering, reader, parent)
    )
    conflicted._2 should contain(ordering)
    (conflicted._1 ++ conflicted._2) should not contain reader
    conflicted._3 shouldBe empty

    val retainedPool = conflicted._3.foldLeft(pool)((current, id) => current.invalidate(id))
    retainedPool.getAllPrioritized.map(_.id) should contain(reader.id)
    retainedPool.isInvalidated(reader.id) shouldBe false

    val reselected = CandidateGenerator.collectTxs(
      defaultMinerPk, matrixParameters.maxBlockCost, matrixParameters.maxBlockSize,
      us, upcomingContext, Seq(reader), Seq(parent)
    )
    reselected._2 should contain(reader)
    reselected._3 shouldBe empty
  }
}
