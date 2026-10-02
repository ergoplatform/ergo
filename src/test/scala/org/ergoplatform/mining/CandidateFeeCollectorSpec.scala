package org.ergoplatform.mining

import org.ergoplatform._
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.state.BoxHolder
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.ergoplatform.wallet.utils.WalletGenerators.ergoBoxGen
import org.scalacheck.Gen
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Blake2b256
import sigma.data.SigmaConstants.MaxBoxSize
import sigmastate.eval.Extensions._

class CandidateFeeCollectorSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._
  import org.ergoplatform.utils.generators.ValidBlocksGenerators._

  implicit private val verifier: ErgoInterpreter = ErgoInterpreter(parameters)

  property("oversized fee collector defers its source and descendants without starving valid transactions") {
    val tokens = (0 until 100).map(i => Blake2b256(s"mining-fee-token-$i").toTokenId -> Long.MaxValue)
    val tokenInputs = tokens.grouped(50).map(group =>
      ergoBoxGen(
        propGen = trueLeafGen,
        tokensGen = Gen.const(group),
        valueGenOpt = Some(Gen.const(1000000000000L)),
        heightGen = Gen.const(0)
      ).sample.get
    ).toIndexedSeq
    tokenInputs.map(_.additionalTokens.size) shouldBe IndexedSeq(50, 50)

    def plainInput(value: Long) = ergoBoxGen(
      propGen = trueLeafGen,
      tokensGen = Gen.const(Seq.empty),
      valueGenOpt = Some(Gen.const(value)),
      heightGen = Gen.const(0)
    ).sample.get
    val ordinaryInput = plainInput(10000000L)
    val dataOnlyInput = plainInput(100000000L)
    val zeroFeeInput = plainInput(10000000L)
    val allInputs = tokenInputs ++ IndexedSeq(ordinaryInput, dataOnlyInput, zeroFeeInput)
    val us = createUtxoState(BoxHolder(allInputs), parameters)

    def payingFees(inputs: IndexedSeq[ErgoBox]): ErgoTransaction = {
      val outputs = inputs.map(box =>
        new ErgoBoxCandidate(box.value, feeProp, box.creationHeight, box.additionalTokens)
      )
      ErgoTransaction(inputs.map(box => new Input(box.id, emptyProverResult)), IndexedSeq.empty, outputs)
    }

    val poisonFeeOutputs = tokenInputs.map(box =>
      new ErgoBoxCandidate(box.value - 5000000L, feeProp, box.creationHeight, box.additionalTokens)
    )
    val poison = ErgoTransaction(
      tokenInputs.map(box => new Input(box.id, emptyProverResult)), IndexedSeq.empty,
      poisonFeeOutputs :+ new ErgoBoxCandidate(10000000L, TrueTree, 0)
    )
    val child = ErgoTransaction(
      IndexedSeq(new Input(poison.outputs.last.id, emptyProverResult)), IndexedSeq.empty,
      IndexedSeq(new ErgoBoxCandidate(5000000L, feeProp, 0),
        new ErgoBoxCandidate(5000000L, TrueTree, 0))
    )
    val grandchild = payingFees(IndexedSeq(child.outputs.last))
    val dataOnly = ErgoTransaction(
      IndexedSeq(new Input(dataOnlyInput.id, emptyProverResult)),
      IndexedSeq(DataInput(poison.outputs.last.id)),
      IndexedSeq(new ErgoBoxCandidate(90000000L, feeProp, 0),
        new ErgoBoxCandidate(10000000L, TrueTree, 0))
    )
    val dataOnlyChild = payingFees(IndexedSeq(dataOnly.outputs.last))
    val ordinary = payingFees(IndexedSeq(ordinaryInput))
    val zeroFee = ErgoTransaction(
      IndexedSeq(new Input(zeroFeeInput.id, emptyProverResult)), IndexedSeq.empty,
      IndexedSeq(new ErgoBoxCandidate(zeroFeeInput.value, TrueTree, zeroFeeInput.creationHeight))
    )
    val missing = ErgoTransaction(
      IndexedSeq(new Input(ADKey @@ Array.fill(32)(42.toByte), emptyProverResult)), IndexedSeq.empty,
      IndexedSeq(new ErgoBoxCandidate(1000000L, feeProp, 0))
    )

    val h = validFullBlock(None, us, BoxHolder(allInputs)).header
    val upcomingContext = us.stateContext.upcoming(
      h.minerPk, h.timestamp, h.nBits, h.votes, emptyVSUpdate, h.version
    )
    upcomingContext.sigmaPreHeader.version should be < Header.Interpreter60Version
    us.validateWithCost(poison, upcomingContext, parameters.maxBlockCost,
      Some(verifier), softFieldsAllowed = true).isSuccess shouldBe true
    us.validateWithCost(ordinary, upcomingContext, parameters.maxBlockCost,
      Some(verifier), softFieldsAllowed = true).isSuccess shouldBe true

    val collector = CandidateGenerator.collectFees(
      us.stateContext.currentHeight, Seq(poison), defaultMinerPk, upcomingContext
    ).head
    collector.outputs.head.bytes.length should be > MaxBoxSize.value
    val poisonBoxes = poison.outputs.map(box => box.id.toSeq -> box).toMap
    val collectorInputs = collector.inputs.map(i => poisonBoxes(i.boxId.toSeq))
    collector.statefulValidity(collectorInputs, IndexedSeq.empty, upcomingContext)
      .failed.get.getMessage should include ("Box size should not exceed")

    val control = CandidateGenerator.collectTxs(
      defaultMinerPk, parameters.maxBlockCost, parameters.maxBlockSize,
      us, upcomingContext, Seq(ordinary, zeroFee, missing)
    )
    control._2 should contain (ordinary)
    control._2 should contain (zeroFee)
    control._3 shouldBe Seq(missing.id)

    val result = CandidateGenerator.collectTxs(
      defaultMinerPk, parameters.maxBlockCost, parameters.maxBlockSize,
      us, upcomingContext,
      Seq(poison, child, grandchild, dataOnly, dataOnlyChild, ordinary, zeroFee, missing)
    )
    result._1 shouldBe empty
    result._2 should contain (ordinary)
    result._2 should contain (zeroFee)
    result._2 should not contain poison
    result._2 should not contain child
    result._2 should not contain grandchild
    result._2 should not contain dataOnly
    result._2 should not contain dataOnlyChild
    result._3 shouldBe Seq(missing.id)
    val selectedCollector = result._2.filterNot(tx => tx == ordinary || tx == zeroFee).head
    selectedCollector.inputs.map(_.boxId.toSeq) shouldBe ordinary.outputs.map(_.id.toSeq)
    selectedCollector.outputs.head.value shouldBe ordinary.outputs.head.value
  }
}
