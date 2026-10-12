package org.ergoplatform.nodeView.state

import org.ergoplatform.mining.CandidateGenerator
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.ProcessingOutcome
import org.ergoplatform.settings.Algos.HF
import org.ergoplatform.settings.{Constants, ErgoValidationSettingsUpdate, Parameters}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.ergoplatform.wallet.utils.TestFileUtils
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import scorex.crypto.authds.ADValue
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert, PersistentBatchAVLProver, VersionedLDBAVLStorage}
import scorex.crypto.hash.Digest32
import scorex.db.LDBVersionedStore
import sigmastate.helpers.TestingHelpers.testBox

import scala.util.Try

/**
  * Parameters of an upcoming state context (the one the mempool, the API and the block
  * candidate assembly validate transactions with) against parameters of the state context
  * block validation uses for the same height: voted parameter changes are to be applied in
  * both on the first block of a voting epoch only.
  */
class UpcomingParametersSpec extends ErgoCorePropertyTest with TestFileUtils {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._
  import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators._

  private val votingSettings = settings.chainSettings.voting
  private val votingLength: Int = votingSettings.votingLength

  /** Number of votes which is enough to change a parameter. */
  private val majority: Int = votingLength / 2 + 1

  /** First block of the voting epoch the tests are in. */
  private val epochStart: Int = 3 * votingLength

  /** A block in the middle of the epoch, the first one built after `majority` votes. */
  private val midEpochHeight: Int = epochStart + majority

  /** First block of the next voting epoch. */
  private val nextEpochStart: Int = epochStart + votingLength

  /** Parameters set by the first block of the epoch. */
  private val epochParameters: Parameters =
    Parameters(epochStart, parameters.parametersTable, parameters.proposedUpdate)

  /** Both parameter changes are voted for by a majority of blocks of the epoch. */
  private val majorityVotes: VotingData = VotingData(Array(
    Parameters.MinValuePerByteDecrease -> majority,
    Parameters.StorageFeeFactorIncrease -> majority))

  private val loweredMinValuePerByte: Int = epochParameters.minValuePerByte - Parameters.MinValueStep
  private val raisedStorageFeeFactor: Int = epochParameters.storageFeeFactor + Parameters.StorageFeeFactorStep

  private val noVotes: Array[Byte] = Array.fill(3)(Parameters.NoParameter)

  private val dustError = "should contain at least <minValuePerByte * outputSize> nanoErgs"

  /** State context a block at `height` is built on and validated against. */
  private def contextBefore(height: Int): ErgoStateContext = {
    val parent = defaultHeaderGen.sample.get
      .copy(height = height - 1, version = epochParameters.blockVersion, votes = noVotes)
    new ErgoStateContext(Seq(parent), None, genesisStateDigest, epochParameters,
      validationSettingsNoIl, majorityVotes)(settings.chainSettings)
  }

  /** Both flavours of an upcoming context for a block on top of `ctx`. */
  private def upcomingContexts(ctx: ErgoStateContext): Seq[UpcomingStateContext] = {
    val parent = ctx.lastHeaders.head
    Seq(
      ctx.simplifiedUpcoming(),
      ctx.upcoming(defaultMinerPkPoint, parent.timestamp + 1, parent.nBits, noVotes,
        ErgoValidationSettingsUpdate.empty, parent.version)
    )
  }

  /** Block on top of `ctx`, with no parameters in its extension. */
  private def nextBlock(ctx: ErgoStateContext): ErgoFullBlock = {
    val parent = ctx.lastHeaders.head
    val fb = invalidErgoFullBlockGen.sample.get
    fb.copy(header = fb.header.copy(height = parent.height + 1, parentId = parent.id,
      version = parent.version, votes = noVotes))
  }

  /** State context block validation checks transactions of the next block with. */
  private def processedContext(ctx: ErgoStateContext): ErgoStateContext =
    ctx.appendFullBlock(nextBlock(ctx)).get

  private val inputBox: ErgoBox =
    testBox(10000000000L, Constants.TrueTree, epochStart, Seq.empty, Map.empty)

  /** Transaction spending `inputBox` into an output of `value` nanoErgs and a change output. */
  private def txWithOutputOf(value: Long): ErgoTransaction = {
    val out = new ErgoBoxCandidate(value, Constants.TrueTree, midEpochHeight)
    val change = new ErgoBoxCandidate(inputBox.value - value, Constants.TrueTree, midEpochHeight)
    ErgoTransaction(IndexedSeq(new Input(inputBox.id, emptyProverResult)), IndexedSeq(out, change))
  }

  /** Minimal value of the first output of the test transaction (VLQ fixpoint). */
  private def minOutputValue(minValuePerByte: Int): Long = {
    var value = 1L
    var required = minValuePerByte.toLong * txWithOutputOf(value).outputs.head.bytes.length
    while (value != required) {
      value = required
      required = minValuePerByte.toLong * txWithOutputOf(value).outputs.head.bytes.length
    }
    value
  }

  /** Transaction which is valid with the current dust limit. */
  private lazy val validTx: ErgoTransaction = txWithOutputOf(minOutputValue(epochParameters.minValuePerByte))

  /** Transaction with an output in between the voted (lowered) dust limit and the current one. */
  private lazy val betweenLimitsTx: ErgoTransaction = {
    val tx = txWithOutputOf(minOutputValue(epochParameters.minValuePerByte) - 1)
    val out = tx.outputs.head
    out.value should be >= loweredMinValuePerByte.toLong * out.bytes.length
    out.value should be < epochParameters.minValuePerByte.toLong * out.bytes.length
    tx
  }

  private def validate(tx: ErgoTransaction, ctx: ErgoStateContext): Try[Int] =
    tx.statefulValidity(IndexedSeq(inputBox), emptyDataBoxes, ctx)(ErgoInterpreter(ctx.currentParameters))

  /** UTXO set holding `inputBox`, with `ctx` stored as its state context. */
  private def utxoStateWith(ctx: ErgoStateContext): UtxoState = {
    val prover = new BatchAVLProver[Digest32, HF](keyLength = 32, valueLengthOpt = None)
    prover.performOneOperation(Insert(inputBox.id, ADValue @@ inputBox.bytes)).get
    val store = new LDBVersionedStore(createTempDir, initialKeepVersions = settings.nodeSettings.keepVersions)
    val version = ErgoState.genesisStateVersion
    val persistentProver = PersistentBatchAVLProver.create(
      prover,
      new VersionedLDBAVLStorage(store),
      UtxoState.metadata(version, prover.digest, None, ctx),
      paranoidChecks = true
    ).get
    new UtxoState(persistentProver, version, store, settings)
  }

  property("upcoming context carries current parameters in the middle of a voting epoch") {
    (midEpochHeight % votingLength) should not be 0
    val ctx = contextBefore(midEpochHeight)
    val processed = processedContext(ctx)
    processed.currentParameters.minValuePerByte shouldBe epochParameters.minValuePerByte
    processed.currentParameters.storageFeeFactor shouldBe epochParameters.storageFeeFactor

    upcomingContexts(ctx).foreach { upcoming =>
      upcoming.currentHeight shouldBe midEpochHeight
      upcoming.currentParameters.minValuePerByte shouldBe processed.currentParameters.minValuePerByte
      upcoming.currentParameters.storageFeeFactor shouldBe processed.currentParameters.storageFeeFactor
      upcoming.currentParameters shouldBe processed.currentParameters
      upcoming.validationSettings shouldBe processed.validationSettings
    }
  }

  property("output below the current dust limit is rejected with upcoming context in the middle of a voting epoch") {
    val ctx = contextBefore(midEpochHeight)
    val processed = processedContext(ctx)

    // block validation rejects the transaction
    validate(betweenLimitsTx, processed).failed.get.getMessage should include(dustError)
    validate(validTx, processed) shouldBe 'success

    upcomingContexts(ctx).foreach { upcoming =>
      validate(betweenLimitsTx, upcoming).failed.get.getMessage should include(dustError)
      validate(validTx, upcoming) shouldBe 'success
    }
  }

  property("mempool does not accept an output below the current dust limit in the middle of a voting epoch") {
    val us = utxoStateWith(contextBefore(midEpochHeight))
    us.stateContext.currentHeight shouldBe midEpochHeight - 1
    us.stateContext.votingData shouldBe majorityVotes

    val outcome = ErgoMemPool.empty(settings).process(UnconfirmedTransaction(betweenLimitsTx, None), us)._2
    outcome shouldBe a[ProcessingOutcome.Invalidated]
    outcome.asInstanceOf[ProcessingOutcome.Invalidated].e.getMessage should include(dustError)

    val (pool, validOutcome) = ErgoMemPool.empty(settings).process(UnconfirmedTransaction(validTx, None), us)
    validOutcome shouldBe a[ProcessingOutcome.Accepted]
    pool.contains(validTx.id) shouldBe true
  }

  property("candidate assembly leaves out an output below the current dust limit in the middle of a voting epoch") {
    val ctx = contextBefore(midEpochHeight)
    val us = utxoStateWith(ctx)
    val upcoming = upcomingContexts(ctx).last // the `upcoming()` flavour, the one createCandidate uses
    val limits = ctx.currentParameters

    val (included, invalid) = CandidateGenerator.collectTxs(defaultMinerPk, limits.maxBlockCost,
      limits.maxBlockSize, us, upcoming, Seq(betweenLimitsTx))
    included shouldBe empty
    invalid shouldBe Seq(betweenLimitsTx.id)

    val (includedValid, _) = CandidateGenerator.collectTxs(defaultMinerPk, limits.maxBlockCost,
      limits.maxBlockSize, us, upcoming, Seq(validTx))
    includedValid.map(_.id) shouldBe Seq(validTx.id)
  }

  property("upcoming context applies voted parameter changes on the first block of a voting epoch") {
    (nextEpochStart % votingLength) shouldBe 0
    val ctx = contextBefore(nextEpochStart)
    val (expectedParameters, _) = ctx.currentParameters.update(nextEpochStart, forkVote = false,
      majorityVotes.epochVotes, ErgoValidationSettingsUpdate.empty, votingSettings)
    expectedParameters.minValuePerByte shouldBe loweredMinValuePerByte
    expectedParameters.storageFeeFactor shouldBe raisedStorageFeeFactor

    upcomingContexts(ctx).foreach { upcoming =>
      upcoming.currentHeight shouldBe nextEpochStart
      upcoming.currentParameters.height shouldBe nextEpochStart
      upcoming.currentParameters.parametersTable shouldBe expectedParameters.parametersTable
      upcoming.currentParameters.proposedUpdate shouldBe expectedParameters.proposedUpdate

      // a block announcing parameters of the upcoming context is accepted
      val block = nextBlock(ctx)
      val extension = (upcoming.currentParameters.toExtensionCandidate ++
        upcoming.validationSettings.toExtensionCandidate).toExtension(block.header.id)
      val processed = ctx.appendFullBlock(block.copy(extension = extension)).get
      processed.currentParameters shouldBe upcoming.currentParameters
      processed.currentParameters.minValuePerByte shouldBe loweredMinValuePerByte
      processed.currentParameters.storageFeeFactor shouldBe raisedStorageFeeFactor

      // while a block announcing parameters of the previous epoch is not
      val staleExtension = (Parameters(nextEpochStart, epochParameters.parametersTable, epochParameters.proposedUpdate)
        .toExtensionCandidate ++ ctx.validationSettings.toExtensionCandidate).toExtension(block.header.id)
      ctx.appendFullBlock(block.copy(extension = staleExtension)) shouldBe 'failure
    }
  }

}
