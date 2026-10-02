package org.ergoplatform.mining

import com.google.common.io.Files.createTempDir
import org.ergoplatform.{ErgoBoxCandidate, Input}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.state.{BoxHolder, UtxoState}
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.settings.{NetworkType, Parameters}
import org.ergoplatform.utils.{ErgoCompilerHelpers, ErgoCorePropertyTest}
import org.ergoplatform.validation.SoftFieldsAccessError
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.ergoplatform.wallet.utils.WalletGenerators.ergoBoxGen
import org.scalacheck.Gen
import scorex.crypto.hash.Blake2b256
import sigma.Colls
import sigma.data.SigmaConstants.MaxBoxSize
import sigmastate.eval.Extensions._

class CandidateFeeCollectorPrefixSpec extends ErgoCorePropertyTest with ErgoCompilerHelpers {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants.settings
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._

  property("invalid optional V4 prefix collector does not block independent ordering transaction") {
    val matrixParameters = Parameters(parameters.height,
      parameters.parametersTable.updated(Parameters.BlockVersion, Header.Interpreter60Version.toInt),
      parameters.proposedUpdate)
    val matrixSettings = settings.copy(networkType = NetworkType.DevNet60)
    val tokens = (0 until 100).map(i => Blake2b256(s"prefix-fee-token-$i").toTokenId -> Long.MaxValue)
    val tokenInputs = tokens.grouped(50).map(group =>
      ergoBoxGen(
        propGen = trueLeafGen,
        tokensGen = Gen.const(group),
        valueGenOpt = Some(Gen.const(1000000000000L)),
        heightGen = Gen.const(0)
      ).sample.get
    ).toIndexedSeq
    val softTree = compileSourceV5("CONTEXT.minerPubKey.size >= 0", 0)
    val orderingInput = ergoBoxGen(
      propGen = Gen.const(softTree),
      tokensGen = Gen.const(Seq.empty),
      valueGenOpt = Some(Gen.const(10000000L)),
      heightGen = Gen.const(0)
    ).sample.get
    val us = UtxoState.fromBoxHolder(
      BoxHolder(tokenInputs :+ orderingInput), None, createTempDir(), matrixSettings, matrixParameters
    )
    val upcomingContext = us.stateContext.upcoming(
      defaultMinerPk.value, System.currentTimeMillis(), matrixSettings.chainSettings.initialNBits,
      Array.fill[Byte](3)(0), emptyVSUpdate, Header.Interpreter60Version
    )
    val verifier = ErgoInterpreter(upcomingContext.currentParameters)
    upcomingContext.sigmaPreHeader.version shouldBe Header.Interpreter60Version
    upcomingContext.currentParameters.blockVersion shouldBe Header.Interpreter60Version

    val prefixOutputs = tokens.map(token =>
      new ErgoBoxCandidate(20000000000L, feeProp, 0, Colls.fromItems(token))
    ).toIndexedSeq
    val prefix = ErgoTransaction(
      tokenInputs.map(box => new Input(box.id, emptyProverResult)), IndexedSeq.empty, prefixOutputs
    )
    val ordering = ErgoTransaction(
      IndexedSeq(new Input(orderingInput.id, emptyProverResult)), IndexedSeq.empty,
      IndexedSeq(new ErgoBoxCandidate(orderingInput.value, TrueTree, 0))
    )
    us.validateWithCost(prefix, upcomingContext, matrixParameters.maxBlockCost,
      Some(verifier), softFieldsAllowed = true).isSuccess shouldBe true
    us.validateWithCost(ordering, upcomingContext, matrixParameters.maxBlockCost,
      Some(verifier), softFieldsAllowed = false).failed.get shouldBe a[SoftFieldsAccessError]
    us.validateWithCost(ordering, upcomingContext, matrixParameters.maxBlockCost,
      Some(verifier), softFieldsAllowed = true).isSuccess shouldBe true

    val collectors = CandidateGenerator.collectFees(
      us.stateContext.currentHeight, Seq(prefix, ordering), defaultMinerPk, upcomingContext
    )
    collectors.length shouldBe 1
    collectors.head.outputs.head.bytes.length should be > MaxBoxSize.value
    val prefixBoxes = prefix.outputs.map(box => box.id.toSeq -> box).toMap
    val collectorInputs = collectors.head.inputs.map(i => prefixBoxes(i.boxId.toSeq))
    collectors.head.statefulValidity(collectorInputs, IndexedSeq.empty, upcomingContext)(verifier)
      .failed.get.getMessage should include ("Box size should not exceed")

    val control = CandidateGenerator.collectTxs(
      defaultMinerPk, matrixParameters.maxBlockCost, matrixParameters.maxBlockSize,
      us, upcomingContext, Seq(ordering)
    )
    control._1 shouldBe empty
    control._2 should contain (ordering)

    val result = CandidateGenerator.collectTxs(
      defaultMinerPk, matrixParameters.maxBlockCost, matrixParameters.maxBlockSize,
      us, upcomingContext, Seq(ordering), Seq(prefix)
    )
    result._1 shouldBe empty
    result._2 should contain (ordering)
    result._2 should not contain collectors.head
    result._3 shouldBe empty
  }
}
