package org.ergoplatform.nodeView.wallet.models

import io.circe.generic.encoding.DerivedObjectEncoder.deriveEncoder
import io.circe.syntax._
import io.circe.{Encoder, Json, KeyEncoder}
import scorex.util.ModifierId

/**
  * Box model for Wallet API
  *
  * @param value  - Amount of Ergs
  * @param tokens - ID's and amounts of other tokens
  */
final case class ChangeBox(value: Long, tokens: Map[ModifierId, Long])

object ChangeBox {

  implicit val modifierIdEncoder: KeyEncoder[ModifierId] = KeyEncoder.instance(_.asInstanceOf[String])

  implicit val encoder: Encoder[ChangeBox] = box =>
    Json.obj(
      "value" -> box.value.asJson,
      "tokens" -> box.tokens.asJson
    )
}
