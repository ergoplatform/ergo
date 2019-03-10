package org.ergoplatform.nodeView.mempool

import org.ergoplatform.ErgoAddressEncoder.TestnetNetworkPrefix
import org.ergoplatform.ErgoScriptPredef.boxCreationHeight
import org.ergoplatform.{ErgoScriptPredef, Height, Self}
import org.ergoplatform.nodeView.state.{BoxHolder, UtxoState}
import org.ergoplatform.utils.ErgoPropertyTest
import sigmastate._
import sigmastate.Values.{ErgoTree, IntConstant, SigmaPropConstant}
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.interpreter.CryptoConstants.dlogGroup
import sigmastate.lang.{SigmaCompiler, TransformingSigmaBuilder}
import sigmastate.lang.Terms._

import scala.util.{Random, Try}

class ScriptsSpec extends ErgoPropertyTest {

  val compiler = SigmaCompiler(TestnetNetworkPrefix, TransformingSigmaBuilder)
  val delta = emission.settings.minerRewardDelay

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
    applyBlockSpendingScript(fromString("CONTEXT.dataInputs.size == 1")) shouldBe 'success
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
  }


  private def fromString(str: String): ErgoTree = {
    compiler.compile(Map(), str).asBoolValue.toSigmaProp
  }

  private def applyBlockSpendingScript(script: ErgoTree): Try[UtxoState] = {
    val box = ergoBoxGen(script, heightGen = 0).sample.get
    val bh = BoxHolder(Seq(box))
    val us = UtxoState.fromBoxHolder(bh, None, createTempDir, stateConstants)
    val tx = validTransactionsFromBoxHolder(bh, new Random(1), 101)._1
    tx.size shouldBe 1
    val block = validFullBlock(None, us, tx, Some(1234L))
    us.applyModifier(block)
  }
}
