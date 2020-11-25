package org.ergoplatform.nodeView.wallet.models

import io.circe.generic.encoding.DerivedObjectEncoder.deriveEncoder
import io.circe.syntax._
import io.circe.{Encoder, Json}
import org.ergoplatform.{ErgoBox, JsonCodecs}

/**
  * Response for requested boxes that contains ErgoBoxes and ChangeBoxes
  *
  * @param boxes       - ErgoBoxes that satisfy user's request
  * @param changeBoxes - ChangeBoxes that satisfy user's request
  */
final case class CollectedBoxes(boxes: Seq[ErgoBox], changeBoxes: Seq[ChangeBox])

object CollectedBoxes extends JsonCodecs {

  implicit val encoder: Encoder[CollectedBoxes] = request =>
    Json.obj(
      "boxes" -> request.boxes.asJson,
      "changeBoxes" -> request.changeBoxes.asJson
    )
}
