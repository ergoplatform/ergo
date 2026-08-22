package org.ergoplatform.wallet.interpreter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sigmastate.helpers.TestingHelpers._

class ErgoInterpreterSpec
  extends AnyFlatSpec
    with ScalaCheckPropertyChecks
    with Matchers {

  import org.ergoplatform.wallet.utils.WalletGenerators._

  "ErgoInterpreter.sameNonMandatoryRegisters" should
    "compare only non-mandatory registers (R4..R9), ignoring R0..R3" in {
    forAll(ergoBoxGen, ergoBoxGen) { (a, b) =>
      // rebuild `b` keeping its own script, tokens and height but adopting `a`'s
      // non-mandatory registers: the mandatory registers differ, yet R4..R9 match.
      val bWithARegs = testBox(
        b.value,
        b.ergoTree,
        b.creationHeight,
        b.additionalTokens.toArray.toSeq,
        a.additionalRegisters,
        transactionId = b.transactionId,
        boxIndex = b.index)
      ErgoInterpreter.sameNonMandatoryRegisters(a, bWithARegs) shouldBe true
    }
  }

  it should "detect a change in non-mandatory registers" in {
    forAll(ergoBoxGen) { box =>
      whenever(box.additionalRegisters.nonEmpty) {
        val noRegs = testBox(
          box.value,
          box.ergoTree,
          box.creationHeight,
          box.additionalTokens.toArray.toSeq,
          Map(),
          transactionId = box.transactionId,
          boxIndex = box.index)
        ErgoInterpreter.sameNonMandatoryRegisters(box, noRegs) shouldBe false
      }
    }
  }

}
