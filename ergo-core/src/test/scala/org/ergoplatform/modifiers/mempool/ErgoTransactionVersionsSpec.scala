package org.ergoplatform.modifiers.mempool

import io.circe.syntax._
import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.VersionDependentValues._
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, ErgoLikeTransaction, Input}
import scorex.crypto.authds.ADKey
import sigma.ast.{BoxConstant, EvaluatedValue, IntConstant, LongConstant, SType, Tuple, UnsignedBigIntConstant}
import sigma.data.CBox
import sigma.interpreter.{ContextExtension, ProverResult}

import java.math.BigInteger

class ErgoTransactionVersionsSpec extends ErgoCorePropertyTest with ApiCodecs {

  private def tx(register: Option[EvaluatedValue[SType]], extension: Option[EvaluatedValue[SType]]): ErgoTransaction =
    atV6 {
      val input = Input(ADKey @@ Array.fill(32)(2.toByte),
        ProverResult(Array.emptyByteArray, ContextExtension(extension.map(0.toByte -> _).toMap)))
      val output = new ErgoBoxCandidate(1000000000L, TrueTree, 0,
        additionalRegisters = register.map(ErgoBox.R4 -> _).toMap)
      ErgoTransaction(IndexedSeq(input), IndexedSeq(output))
    }

  // a 6.0 type: serialized from ErgoTree version 3 only
  private val v6Only = Tuple(UnsignedBigIntConstant(BigInteger.ONE), IntConstant(1))

  property("size of a v6 block transaction is its length in the block, whatever context reads it first") {
    val blockTx = readFromBlock(tx(register = Some(upcastValue), extension = Some(upcastValue)))
    val inBlock = atV6(ErgoTransactionSerializer.toBytes(blockTx)).length
    blockTx.size shouldBe inBlock // first read here, at the default context
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
