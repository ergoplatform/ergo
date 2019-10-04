package org.ergoplatform.nodeView.wallet

import io.circe.syntax._
import io.circe.{Encoder, Json}
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.http.api.ApiEncoderOption.{Detalization, ShowDetails}
import org.ergoplatform.wallet.boxes.TrackedBox

case class WalletBox(trackedBox: TrackedBox, confirmationsNumOpt: Option[Int])

object WalletBox extends ApiCodecs {

  implicit val detalization: Detalization = ShowDetails

  implicit def encoder(implicit ae: ErgoAddressEncoder): Encoder[WalletBox] = { obj =>
    obj.trackedBox.asJson.deepMerge(
      Json.obj(
        "confirmationsNum" -> obj.confirmationsNumOpt.asJson,
        "address" -> ae.fromProposition(obj.trackedBox.box.ergoTree)
          .toOption
          .map(_.toString)
          .asJson
      )
    )
  }

}
