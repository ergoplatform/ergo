package org.ergoplatform.nodeView.wallet.requests

import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.ergoplatform.ErgoBox.NonMandatoryRegisterId
import org.ergoplatform.modifiers.mempool.ErgoTransaction._
import org.ergoplatform.nodeView.wallet.ErgoAddressJsonEncoder
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.{ErgoAddress, ErgoBox}
import sigmastate.SType
import sigmastate.Values.EvaluatedValue

/**
  * A payment request contains an address (probably containing script), value, assets, additional registers.
  */
case class PaymentRequest(address: ErgoAddress,
                          value: Long,
                          assets: Seq[(ErgoBox.TokenId, Long)],
                          registers: Map[NonMandatoryRegisterId, EvaluatedValue[_ <: SType]])
  extends TransactionGenerationRequest

class PaymentRequestEncoder(settings: ErgoSettings) extends Encoder[PaymentRequest] {

  implicit val addressEncoder: Encoder[ErgoAddress] = ErgoAddressJsonEncoder(settings).encoder

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

  val addressEncoders: ErgoAddressJsonEncoder = ErgoAddressJsonEncoder(settings)

  implicit def addressDecoder: Decoder[ErgoAddress] = addressEncoders.decoder

  def apply(cursor: HCursor): Decoder.Result[PaymentRequest] = {
    for {
      address <- cursor.downField("address").as[ErgoAddress]
      value <- cursor.downField("value").as[Long]
      assets <- cursor.downField("assets").as[Option[Seq[(ErgoBox.TokenId, Long)]]]
      registers <- cursor.downField("registers").as[Option[Map[NonMandatoryRegisterId, EvaluatedValue[SType]]]]
    } yield PaymentRequest(address, value, assets.toSeq.flatten, registers.getOrElse(Map.empty))
  }

}
