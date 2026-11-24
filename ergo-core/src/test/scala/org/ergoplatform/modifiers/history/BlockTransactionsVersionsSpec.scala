package org.ergoplatform.modifiers.history

import io.circe.parser.parse
import io.circe.syntax._
import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.VersionDependentValues._
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import scorex.crypto.authds.ADKey
import scorex.util.bytesToId
import scorex.util.encode.Base16
import sigma.Colls
import sigma.ast.{BoxConstant, EvaluatedValue, IntConstant, LongConstant, SType, Tuple}
import sigma.data.CBox
import sigma.interpreter.{ContextExtension, ProverResult}

import scala.util.Try

/**
  * Transactions root and parsing of a v6 block's transactions section with version-dependent values.
  */
class BlockTransactionsVersionsSpec extends ErgoCorePropertyTest with ApiCodecs {

  private def tx(inputId: Byte,
                 register: Option[EvaluatedValue[SType]],
                 extension: Option[EvaluatedValue[SType]]): ErgoTransaction =
    atV6 {
      val input = Input(ADKey @@ Array.fill(32)(inputId),
        ProverResult(Array.emptyByteArray, ContextExtension(extension.map(0.toByte -> _).toMap)))
      val output = new ErgoBoxCandidate(1000000000L, TrueTree, 0,
        additionalRegisters = register.map(ErgoBox.R4 -> _).toMap)
      ErgoTransaction(IndexedSeq(input), IndexedSeq(output))
    }

  private val plain = tx(4, register = Some(LongConstant(7L)), extension = None)

  private def section(caseTx: ErgoTransaction): BlockTransactions =
    atV6 { BlockTransactions(bytesToId(Array.fill(32)(1.toByte)), Header.Interpreter60Version, Seq(plain, caseTx)) }

  // a box whose register holds `upcastValue`
  private val boxWithUpcast = new ErgoBox(1000000000L, TrueTree, Colls.emptyColl, Map(ErgoBox.R4 -> upcastValue),
    bytesToId(Array.fill(32)(3.toByte)), 0.toShort, 0)

  property("the transactions root recomputed from a v6 block's json is the block's") {
    val cases = Seq(
      "(a) register" -> tx(5, register = Some(upcastValue), extension = None),
      "(b) extension" -> tx(6, register = None, extension = Some(upcastValue)),
      "(c) box constant in a register" -> tx(7, register = Some(BoxConstant(CBox(boxWithUpcast))), extension = None),
      "(d) tuple of constants" -> tx(8, register = Some(Tuple(IntConstant(1), IntConstant(1))), extension = None),
      "(e) long constant" -> tx(9, register = Some(LongConstant(1L)), extension = None)
    )
    cases.foreach { case (name, caseTx) =>
      val carried = BlockTransactionsSerializer.parseBytes(BlockTransactionsSerializer.toBytes(section(caseTx)))
      // rendered and decoded at the default context, through text
      val decoded = parse(carried.asJson.noSpaces).flatMap(_.as[BlockTransactions])
        .fold(e => fail(s"$name: $e"), identity)
      withClue(s"$name: ") {
        BlockTransactions.transactionsRoot(decoded.txs, decoded.blockVersion).toSeq shouldBe carried.digest.toSeq
        decoded.txs.map(_.id) shouldBe carried.txs.map(_.id)
      }
    }
  }

  // Upcast(IntConstant(1), SLong) with no tuple around it
  private val bareUpcastHex = "7e040205"

  property("a bare Upcast in a v6 block section's register or extension is rejected") {
    val cases = Seq(
      "register" -> tx(5, register = Some(upcastValue), extension = None),
      "extension" -> tx(6, register = None, extension = Some(upcastValue))
    )
    cases.foreach { case (name, caseTx) =>
      val hex = Base16.encode(atV6(BlockTransactionsSerializer.toBytes(section(caseTx))))
      def parsed(h: String) = Try(BlockTransactionsSerializer.parseBytes(Base16.decode(h).get))
      withClue(s"$name: ") {
        hex.sliding(upcastHexV3.length).count(_ == upcastHexV3) shouldBe 1
        parsed(hex).isSuccess shouldBe true
        // a shorter value alone does not break the section
        parsed(hex.replace(upcastHexV3, upcastHexBelowV3)).isSuccess shouldBe true
        parsed(hex.replace(upcastHexV3, bareUpcastHex)).isFailure shouldBe true
      }
    }
  }

}
