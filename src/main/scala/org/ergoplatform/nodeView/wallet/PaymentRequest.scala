package org.ergoplatform.nodeView.wallet

import io.circe.{Decoder, Encoder, HCursor, Json}
import io.circe.syntax._
import org.ergoplatform.ErgoBox.NonMandatoryRegisterId
import org.ergoplatform.modifiers.mempool.ErgoTransaction._
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate}
import sigmastate.SType
import sigmastate.Values.EvaluatedValue

/**
  * A payment request contains a script, value, assets, additional registers.
  */
case class PaymentRequest(address: ErgoAddress,
                          value: Long,
                          assets: Seq[(ErgoBox.TokenId, Long)],
                          registers: Map[NonMandatoryRegisterId, EvaluatedValue[_ <: SType]]) {
  def toBoxCandidate: ErgoBoxCandidate = {
    new ErgoBoxCandidate(value, address.script, assets, registers)
  }
}

class PaymentRequestEncoder(settings: ErgoSettings) extends Encoder[PaymentRequest] {
  implicit val addressEncoder = new ErgoAddressEncoder(settings).encoder
  def apply(request: PaymentRequest): Json = {
    Json.obj(
      "address" -> request.address.asJson,
      "value" -> request.value.asJson,
      "assets" -> request.assets.asJson,
      "registers" -> request.registers.asJson
    )
  }
}

class PaymentRequestDecoder(settings: ErgoSettings) extends Decoder[PaymentRequest] {

  implicit val addressDecoder = new ErgoAddressEncoder(settings).decoder

  def apply(cursor: HCursor): Decoder.Result[PaymentRequest] = {
    for {
      address <- cursor.downField("address").as[ErgoAddress]
      value <- cursor.downField("value").as[Long]
      assets <- cursor.downField("assets").as[Seq[(ErgoBox.TokenId, Long)]]
      registers <- cursor.downField("registers").as[Map[NonMandatoryRegisterId, EvaluatedValue[SType]]]
    } yield PaymentRequest(address, value, assets, registers)
  }
}
