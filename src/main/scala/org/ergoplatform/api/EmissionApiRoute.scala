package org.ergoplatform.api

import akka.actor.ActorRefFactory
import akka.http.scaladsl.server.Route
import io.circe.{Encoder, Json}
import org.ergoplatform.mining.emission.CoinsEmission
import org.ergoplatform.settings.ErgoSettings
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings

final case class EmissionApiRoute(emission: CoinsEmission, ergoSettings: ErgoSettings)
                                 (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute {

  import EmissionApiRoute._

  override val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  override def route: Route = (pathPrefix("emission") & withCors) {
    emissionAt
  }

  val emissionAt = (pathPrefix("at" / LongNumber) & get) { height =>
    ApiResponse(emissionInfoAtHeight(height, emission))
  }
}

object EmissionApiRoute {

  def emissionInfoAtHeight(height: Long, ce: CoinsEmission): EmissionInfo = {
    val minerReward = ce.emissionAtHeight(height)
    val totalCoinsIssued = ce.issuedCoinsAfterHeight(height)
    val totalRemainCoins = ce.coinsTotal - totalCoinsIssued
    EmissionInfo(minerReward, totalCoinsIssued, totalRemainCoins)
  }

  final case class EmissionInfo(minerReward: Long, totalCoinsIssued: Long, totalRemainCoins: Long)

  implicit val encoder: Encoder[EmissionInfo] = (ei: EmissionInfo) => Json.obj(
    "minerReward" -> Json.fromLong(ei.minerReward),
    "totalCoinsIssued" -> Json.fromLong(ei.totalCoinsIssued),
    "totalRemainCoins" -> Json.fromLong(ei.totalRemainCoins)
  )
}
