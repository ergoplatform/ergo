package org.ergoplatform.nodeView.wallet.models

import io.circe.syntax._
import io.circe.{Encoder, Json}
import org.ergoplatform.{ErgoBox, JsonCodecs}

/**
  * Response for requested boxes that contains ErgoBoxes and ChangeBoxes
  *
  * @param boxes       - ErgoBoxes that satisfy user's request
  * @param changeBox - Box with excessive tokens and ergs
  */
final case class CollectedBoxes(boxes: Seq[ErgoBox], changeBox: Option[ChangeBox])

object CollectedBoxes extends JsonCodecs {

  implicit val encoder: Encoder[CollectedBoxes] = request =>
    Json.obj(
      "boxes" -> request.boxes.asJson,
      "changeBox" -> request.changeBox.asJson
    )
}
