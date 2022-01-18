package org.ergoplatform.http.api

import akka.actor.ActorRefFactory
import akka.http.scaladsl.server.Route
import io.circe.{Encoder, Json}
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.reemission.ReemissionRules
import org.ergoplatform.settings.{ErgoSettings, ReemissionSettings}
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings

case class EmissionApiRoute(ergoSettings: ErgoSettings)
                           (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute {

  import EmissionApiRoute._

  override val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  private val emissionRules = ergoSettings.chainSettings.emissionRules

  private val reemissionSettings = ergoSettings.chainSettings.reemission

  override def route: Route = pathPrefix("emission") {
    emissionAt
  }

  val emissionAt: Route = (pathPrefix("at" / LongNumber) & get) { height =>
    ApiResponse(emissionInfoAtHeight(height, emissionRules, reemissionSettings))
  }

}

object EmissionApiRoute {

  final case class EmissionInfo(minerReward: Long, totalCoinsIssued: Long, totalRemainCoins: Long, reemissionAmt: Long)

  def emissionInfoAtHeight(height: Long,
                           emissionRules: EmissionRules,
                           reemissionSettings: ReemissionSettings): EmissionInfo = {
    val reemissionAmt = ReemissionRules.reemissionForHeight(height.toInt, emissionRules, reemissionSettings)
    val minerReward = emissionRules.emissionAtHeight(height) - reemissionAmt
    val totalCoinsIssued = emissionRules.issuedCoinsAfterHeight(height)
    val totalRemainCoins = emissionRules.coinsTotal - totalCoinsIssued
    EmissionInfo(minerReward, totalCoinsIssued, totalRemainCoins, reemissionAmt)
  }

  implicit val encoder: Encoder[EmissionInfo] = (ei: EmissionInfo) => Json.obj(
    "minerReward" -> Json.fromLong(ei.minerReward),
    "totalCoinsIssued" -> Json.fromLong(ei.totalCoinsIssued),
    "totalRemainCoins" -> Json.fromLong(ei.totalRemainCoins),
    "reemitted" -> Json.fromLong(ei.reemissionAmt)
  )

}
