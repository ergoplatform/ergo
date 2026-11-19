package org.ergoplatform.modifiers.mempool

import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.VersionDependentValues._
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import scorex.crypto.authds.ADKey
import sigma.ast.{BoxConstant, EvaluatedValue, IntConstant, LongConstant, SType, Tuple, UnsignedBigIntConstant}
import sigma.data.CBox
import sigma.interpreter.{ContextExtension, ProverResult}

import java.math.BigInteger

class ErgoTransactionVersionsSpec extends ErgoCorePropertyTest {

  private def tx(register: Option[EvaluatedValue[SType]], extension: Option[EvaluatedValue[SType]]): ErgoTransaction =
    atV6 {
      val input = Input(ADKey @@ Array.fill(32)(2.toByte),
        ProverResult(Array.emptyByteArray, ContextExtension(extension.map(0.toByte -> _).toMap)))
      val output = new ErgoBoxCandidate(1000000000L, TrueTree, 0,
        additionalRegisters = register.map(ErgoBox.R4 -> _).toMap)
      ErgoTransaction(IndexedSeq(input), IndexedSeq(output))
    }

  property("a value serialized differently below ErgoTree version 3 and from it is version-dependent") {
    ErgoTransaction.hasVersionDependentValues(tx(register = Some(upcastValue), extension = None)) shouldBe true
    ErgoTransaction.hasVersionDependentValues(tx(register = None, extension = Some(upcastValue))) shouldBe true
  }

  // a 6.0 type: serialized from ErgoTree version 3 only
  private val v6Only = Tuple(UnsignedBigIntConstant(BigInteger.ONE), IntConstant(1))

  property("constants, and a value with no form below ErgoTree version 3, are not version-dependent") {
    ErgoTransaction.hasVersionDependentValues(tx(Some(LongConstant(1L)), Some(LongConstant(1L)))) shouldBe false
    ErgoTransaction.hasVersionDependentValues(tx(register = None, extension = Some(v6Only))) shouldBe false
  }

  property("a box constant whose register holds a version-dependent value is version-dependent") {
    val inner = tx(register = Some(upcastValue), extension = None).outputs.head
    ErgoTransaction.hasVersionDependentValues(
      tx(register = Some(BoxConstant(CBox(inner))), extension = None)) shouldBe true
  }

}
