package org.ergoplatform.http.api

import akka.actor.ActorRefFactory
import akka.http.scaladsl.server.Route
import io.circe.{Encoder, Json}
import org.ergoplatform.{ErgoAddressEncoder, ErgoScriptPredef, Pay2SAddress}
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.settings.{ErgoSettings, ReemissionSettings}
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings
import io.circe.syntax._

case class EmissionApiRoute(ergoSettings: ErgoSettings)
                           (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute {

  import EmissionApiRoute._

  override val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  private val chainSettings = ergoSettings.chainSettings
  private val emissionRules = chainSettings.emissionRules

  private val reemissionSettings = chainSettings.reemission

  private implicit val ergoAddressEncoder: ErgoAddressEncoder = new ErgoAddressEncoder(chainSettings.addressPrefix)

  override def route: Route = pathPrefix("emission") {
    emissionAt ~ scripts
  }

  /**
    * API method to display emission data for given height
    */
  val emissionAt: Route = (pathPrefix("at" / LongNumber) & get) { height =>
    ApiResponse(emissionInfoAtHeight(height, emissionRules, reemissionSettings))
  }

  /**
    * API method which displays emission and re-emission related scripts as P2S addresses
    */
  val scripts: Route = pathPrefix("scripts") {
    val ms = ergoSettings.chainSettings.monetary

    ApiResponse(
      Json.obj(
        "emission" -> Pay2SAddress(ErgoScriptPredef.emissionBoxProp(ms)).toString().asJson,
        "reemission" -> Pay2SAddress(reemissionSettings.reemissionRules.reemissionBoxProp(ms)).toString().asJson,
        "pay2Reemission" -> Pay2SAddress(reemissionSettings.reemissionRules.payToReemission).toString().asJson
      )
    )
  }

}

object EmissionApiRoute {

  final case class EmissionInfo(minerReward: Long, totalCoinsIssued: Long, totalRemainCoins: Long, reemissionAmt: Long)

  def emissionInfoAtHeight(height: Long,
                           emissionRules: EmissionRules,
                           reemissionSettings: ReemissionSettings): EmissionInfo = {
    val reemissionAmt = reemissionSettings.reemissionRules.reemissionForHeight(height.toInt, emissionRules)
    val minerReward = emissionRules.minersRewardAtHeight(height) - reemissionAmt
    val totalCoinsIssued = emissionRules.issuedCoinsAfterHeight(height)
    val totalRemainCoins = emissionRules.coinsTotal - totalCoinsIssued
    EmissionInfo(minerReward, totalCoinsIssued, totalRemainCoins, reemissionAmt)
  }

  // todo: add totalReemitted
  implicit val encoder: Encoder[EmissionInfo] = (ei: EmissionInfo) => Json.obj(
    "minerReward" -> Json.fromLong(ei.minerReward),
    "totalCoinsIssued" -> Json.fromLong(ei.totalCoinsIssued),
    "totalRemainCoins" -> Json.fromLong(ei.totalRemainCoins),
    "reemitted" -> Json.fromLong(ei.reemissionAmt)
  )

}
