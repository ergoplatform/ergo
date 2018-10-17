package org.ergoplatform.nodeView.wallet.requests

import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.NonMandatoryRegisterId
import org.ergoplatform.modifiers.mempool.ErgoTransaction._
import org.ergoplatform.nodeView.wallet.{ErgoAddress, ErgoAddressEncoder}
import org.ergoplatform.settings.ErgoSettings
import sigmastate.SType
import sigmastate.Values.EvaluatedValue

/**
  * A payment request contains a script, value, assets, additional registers.
  */
case class PaymentRequest(address: ErgoAddress,
                          value: Long,
                          assets: Option[Seq[(ErgoBox.TokenId, Long)]],
                          registers: Option[Map[NonMandatoryRegisterId, EvaluatedValue[_ <: SType]]],
                          fee: Long) extends TransactionRequest

class PaymentRequestEncoder(settings: ErgoSettings) extends Encoder[PaymentRequest] {

  implicit val addressEncoder: Encoder[ErgoAddress] = new ErgoAddressEncoder(settings).encoder

  def apply(request: PaymentRequest): Json = Json.obj(
    "address" -> request.address.asJson,
    "value" -> request.value.asJson,
    "assets" -> request.assets.asJson,
    "registers" -> request.registers.asJson,
    "fee" -> request.fee.asJson
  )
}

class PaymentRequestDecoder(settings: ErgoSettings) extends Decoder[PaymentRequest] {

  val addressEncoders: ErgoAddressEncoder = new ErgoAddressEncoder(settings)

  implicit def addressDecoder: Decoder[ErgoAddress] = addressEncoders.decoder

  def apply(cursor: HCursor): Decoder.Result[PaymentRequest] = {
    for {
      address <- cursor.downField("address").as[ErgoAddress]
      value <- cursor.downField("value").as[Long]
      assets <- cursor.downField("assets").as[Option[Seq[(ErgoBox.TokenId, Long)]]]
      registers <- cursor.downField("registers").as[Option[Map[NonMandatoryRegisterId, EvaluatedValue[SType]]]]
      feeOpt <- cursor.downField("fee").as[Option[Long]]
    } yield PaymentRequest(address, value, assets, registers,
      feeOpt.getOrElse(settings.walletSettings.defaultTransactionFee))
  }
}
