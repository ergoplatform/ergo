package org.ergoplatform.nodeView.mempool

import org.ergoplatform.ErgoAddressEncoder.TestnetNetworkPrefix
import org.ergoplatform.ErgoScriptPredef.boxCreationHeight
import org.ergoplatform.{ErgoBox, ErgoScriptPredef, Height, Self}
import org.ergoplatform.nodeView.state.{BoxHolder, ErgoState, UtxoState}
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.crypto.authds.ADKey
import sigmastate._
import sigmastate.Values._
import sigmastate.lang.Terms._
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.eval.{IRContext, RuntimeIRContext}
import sigmastate.interpreter.CryptoConstants.dlogGroup
import sigmastate.lang.{SigmaCompiler, TransformingSigmaBuilder}

import scala.util.{Random, Try}

class ScriptsSpec extends ErgoPropertyTest {

  val compiler = SigmaCompiler(TestnetNetworkPrefix, TransformingSigmaBuilder)
  val delta = emission.settings.minerRewardDelay
  val fixedBox: ErgoBox = ergoBoxGen(fromString("1 == 1"), heightGen = 0).sample.get
  implicit lazy val context: IRContext = new RuntimeIRContext

  property("scripts complexity") {
    defaultMinerPk.toSigmaProp.treeWithSegregation.complexity should be <= settings.nodeSettings.maxTransactionComplexity
    ErgoScriptPredef.rewardOutputScript(delta, defaultMinerPk).complexity should be <= settings.nodeSettings.maxTransactionComplexity
  }

  property("simple operations without cryptography") {
    // true/false
    applyBlockSpendingScript(Values.TrueLeaf.toSigmaProp) shouldBe 'success
    applyBlockSpendingScript(Values.FalseLeaf.toSigmaProp) shouldBe 'failure
    // eq
    applyBlockSpendingScript(EQ(IntConstant(1), IntConstant(1)).toSigmaProp) shouldBe 'success
    applyBlockSpendingScript(EQ(IntConstant(1), IntConstant(2)).toSigmaProp) shouldBe 'failure
    // math
    applyBlockSpendingScript(EQ(Plus(1, 2), Minus(6, 3)).toSigmaProp) shouldBe 'success
    applyBlockSpendingScript(EQ(Multiply(1, 2), Divide(7, 3)).toSigmaProp) shouldBe 'success
    // context
    applyBlockSpendingScript(EQ(IntConstant(1), Height).toSigmaProp) shouldBe 'success
    applyBlockSpendingScript(fromString("CONTEXT.preHeader.height == 1")) shouldBe 'success
    applyBlockSpendingScript(fromString("CONTEXT.headers.size == 0")) shouldBe 'success
    applyBlockSpendingScript(fromString(s"CONTEXT.dataInputs.exists{ (box: Box) => box.value == ${fixedBox.value}L}")) shouldBe 'success
    // todo other common operations: tokens, data from registers, context extension, etc.
  }

  property("simple crypto") {
    applyBlockSpendingScript(defaultMinerPk) shouldBe 'success
    applyBlockSpendingScript(SigmaAnd(defaultProver.secrets.map(s => SigmaPropConstant(s.publicImage)))) shouldBe 'success
    applyBlockSpendingScript(SigmaAnd(defaultMinerPk, ProveDlog(dlogGroup.generator))) shouldBe 'failure
    applyBlockSpendingScript(SigmaOr(defaultMinerPk, ProveDlog(dlogGroup.generator))) shouldBe 'success
  }

  property("predef scripts") {
    delta shouldBe -1000

    applyBlockSpendingScript(GE(Height, Plus(boxCreationHeight(Self), IntConstant(delta))).toSigmaProp) shouldBe 'success
    applyBlockSpendingScript(ErgoScriptPredef.rewardOutputScript(delta, defaultMinerPk)) shouldBe 'success
//        applyBlockSpendingScript(ErgoScriptPredef.feeProposition(delta)) shouldBe 'success
  }


  private def fromString(str: String): ErgoTree = {
    compiler.compile(Map(), str).asBoolValue.toSigmaProp
  }

  private def applyBlockSpendingScript(script: ErgoTree): Try[UtxoState] = {
    val scriptBox = ergoBoxGen(script, heightGen = 0).sample.get
    val bh = BoxHolder(Seq(fixedBox, scriptBox))
    val us = UtxoState.fromBoxHolder(bh, None, createTempDir, stateConstants)
    bh.boxes.map(b => us.boxById(b._2.id) shouldBe Some(b._2))
    val tx = validTransactionsFromBoxHolder(bh, new Random(1), 201)._1
    tx.size shouldBe 1
    tx.head.inputs.size shouldBe 2
    ErgoState.boxChanges(tx)._1.foreach { boxId: ADKey =>
      assert(us.boxById(boxId).isDefined, s"Box ${Algos.encode(boxId)} missed")
    }
    val block = validFullBlock(None, us, tx, Some(1234L))
    us.applyModifier(block)
  }
}
