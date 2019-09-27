package org.ergoplatform.nodeView.wallet.requests

import io.circe._
import org.ergoplatform.ErgoAddress
import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.nodeView.wallet.ErgoAddressJsonEncoder
import org.ergoplatform.settings.ErgoSettings

trait TransactionRequest

class TransactionRequestEncoder(settings: ErgoSettings) extends Encoder[TransactionRequest] with ApiCodecs {

  implicit val addressEncoder: Encoder[ErgoAddress] = ErgoAddressJsonEncoder(settings).encoder

  def apply(request: TransactionRequest): Json = request match {
    case pr: PaymentRequest => new PaymentRequestEncoder(settings)(pr)
    case ar: AssetIssueRequest => new AssetIssueRequestEncoder(settings)(ar)
    case other => throw new Exception(s"Unknown TransactionRequest type: $other")
  }

}

class TransactionRequestDecoder(settings: ErgoSettings) extends Decoder[TransactionRequest] {

  val paymentRequestDecoder: PaymentRequestDecoder = new PaymentRequestDecoder(settings)
  val assetIssueRequestDecoder: AssetIssueRequestDecoder = new AssetIssueRequestDecoder(settings)

  def apply(cursor: HCursor): Decoder.Result[TransactionRequest] = {
    Seq(paymentRequestDecoder, assetIssueRequestDecoder)
      .map(_.apply(cursor))
      .find(_.isRight)
      .getOrElse(Left(DecodingFailure("Can not find suitable decoder", cursor.history)))
  }

}
