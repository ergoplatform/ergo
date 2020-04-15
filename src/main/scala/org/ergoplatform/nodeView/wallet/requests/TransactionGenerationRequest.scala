package org.ergoplatform.nodeView.wallet.requests

import io.circe._
import org.ergoplatform.ErgoAddress
import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.nodeView.wallet.ErgoAddressJsonEncoder
import org.ergoplatform.settings.ErgoSettings

trait TransactionGenerationRequest

class TransactionRequestEncoder(settings: ErgoSettings) extends Encoder[TransactionGenerationRequest] with ApiCodecs {

  implicit val addressEncoder: Encoder[ErgoAddress] = ErgoAddressJsonEncoder(settings).encoder

  def apply(request: TransactionGenerationRequest): Json = request match {
    case pr: PaymentRequest => new PaymentRequestEncoder(settings)(pr)
    case ar: AssetIssueRequest => new AssetIssueRequestEncoder(settings)(ar)
    case other => throw new Exception(s"Unknown TransactionRequest type: $other")
  }

}

class TransactionRequestDecoder(settings: ErgoSettings) extends Decoder[TransactionGenerationRequest] {

  val paymentRequestDecoder: PaymentRequestDecoder = new PaymentRequestDecoder(settings)
  val assetIssueRequestDecoder: AssetIssueRequestDecoder = new AssetIssueRequestDecoder(settings)

  def apply(cursor: HCursor): Decoder.Result[TransactionGenerationRequest] = {
    Seq(paymentRequestDecoder, assetIssueRequestDecoder)
      .map(_.apply(cursor))
      .find(_.isRight)
      .getOrElse(Left(DecodingFailure("Can not find suitable decoder", cursor.history)))
  }

}
