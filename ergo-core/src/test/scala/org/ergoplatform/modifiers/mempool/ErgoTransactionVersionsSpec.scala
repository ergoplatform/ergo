package org.ergoplatform.modifiers.mempool

import io.circe.syntax._
import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.VersionDependentValues._
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, ErgoLikeTransaction, Input}
import scorex.crypto.authds.ADKey
import sigma.ast.{BoxConstant, EvaluatedValue, IntConstant, LongConstant, SType, Tuple, UnsignedBigIntConstant}
import sigma.data.CBox
import sigma.interpreter.{ContextExtension, ProverResult}

import java.math.BigInteger
import scala.util.Try

class ErgoTransactionVersionsSpec extends ErgoCorePropertyTest with ApiCodecs {

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

  property("size of a v6 block transaction is its length in the block, whatever context reads it first") {
    val blockTx = readFromBlock(tx(register = Some(upcastValue), extension = Some(upcastValue)))
    val inBlock = atV6(ErgoTransactionSerializer.toBytes(blockTx)).length
    blockTx.size shouldBe inBlock // first read here, at the default context
  }

  property("json of a v6 block transaction holds the form its id commits to and decodes to its id and size") {
    val blockTx = readFromBlock(tx(register = Some(upcastValue), extension = Some(upcastValue)))
    val json = blockTx.asJson
    json.hcursor.downField("outputs").downN(0).downField("additionalRegisters").downField("R4")
      .as[String] shouldBe Right(upcastHexV3)
    val decoded = json.as[ErgoTransaction]
    decoded.map(_.id) shouldBe Right(blockTx.id)
    decoded.map(_.size) shouldBe Right(atV6(ErgoTransactionSerializer.toBytes(blockTx)).length)
  }

  property("json of a transaction from a block before version 4 decodes to its id") {
    val blockTx = readFromBlock(tx(register = Some(upcastValue), extension = Some(upcastValue)),
      (Header.Interpreter60Version - 1).toByte)
    blockTx.asJson.as[ErgoTransaction].map(_.id) shouldBe Right(blockTx.id)
  }

  property("json of a transaction whose output has no id below ErgoTree version 3 is an error, as before") {
    val t = tx(register = Some(v6Only), extension = None)
    val rendered = Try(t.asJson)
    withClue(s"rendered: $rendered") { rendered.isFailure shouldBe true }
  }

  property("transaction json decodes exactly when its bytes parse at a v6 block's versions") {
    val cases = Seq(
      "version-dependent values" -> tx(register = Some(upcastValue), extension = Some(upcastValue)),
      "6.0 type in an extension" -> tx(register = None, extension = Some(v6Only)),
      "6.0 type in a register" -> tx(register = Some(v6Only), extension = None)
    )
    cases.foreach { case (name, t) =>
      val parsed = atV6(ErgoTransactionSerializer.parseBytesTry(t.bytes)).map(_.id).toEither.left.map(_ => ())
      // json input built with sigma's encoder (the node's own renders such a register as an error)
      val decoded = atV6((t: ErgoLikeTransaction).asJson).as[ErgoTransaction].map(_.id).left.map(_ => ())
      withClue(s"$name: ") { decoded shouldBe parsed }
    }
  }

}
