package org.ergoplatform.nodeView.wallet.requests

import io.circe._
import org.ergoplatform.ErgoAddress
import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.nodeView.wallet.ErgoAddressJsonEncoder
import org.ergoplatform.settings.ErgoSettings

trait TransactionGenerationRequest

class TransactionRequestEncoder(ergoSettings: ErgoSettings) extends Encoder[TransactionGenerationRequest] with ApiCodecs {

  implicit val addressEncoder: Encoder[ErgoAddress] = ErgoAddressJsonEncoder(ergoSettings.chainSettings).encoder

  def apply(request: TransactionGenerationRequest): Json = request match {
    case pr: PaymentRequest => new PaymentRequestEncoder(ergoSettings)(pr)
    case ar: AssetIssueRequest => new AssetIssueRequestEncoder(ergoSettings)(ar)
    case br: BurnTokensRequest => new BurnTokensRequestEncoder()(br)
    case other => throw new Exception(s"Unknown TransactionRequest type: $other")
  }

}

class TransactionRequestDecoder(settings: ErgoSettings) extends Decoder[TransactionGenerationRequest] {

  val paymentRequestDecoder: PaymentRequestDecoder = new PaymentRequestDecoder(settings)
  val assetIssueRequestDecoder: AssetIssueRequestDecoder = new AssetIssueRequestDecoder(settings)
  val burnTokensRequestDecoder: BurnTokensRequestDecoder = new BurnTokensRequestDecoder()

  def apply(cursor: HCursor): Decoder.Result[TransactionGenerationRequest] = {
    val keys = cursor.keys.map(_.toSet).getOrElse(Set.empty[String])
    if (keys.contains("value") || keys.contains("assets")) {
      paymentRequestDecoder(cursor)
    } else if (Seq("amount", "name", "description", "decimals", "ergValue").exists(keys.contains)) {
      assetIssueRequestDecoder(cursor)
    } else if (keys.contains("assetsToBurn")) {
      burnTokensRequestDecoder(cursor)
    } else {
      Left(DecodingFailure("Missing transaction request discriminator", cursor.history))
    }
  }

}
