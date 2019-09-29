package org.ergoplatform.http.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.syntax._
import io.circe.{Encoder, Json}
import org.ergoplatform.local.ErgoMiner
import org.ergoplatform.mining.{AutolykosSolution, ExternalCandidateBlock}
import org.ergoplatform.nodeView.wallet.ErgoAddressJsonEncoder
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.{ErgoAddress, ErgoScriptPredef, Pay2SAddress}
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings
import sigmastate.basics.DLogProtocol.ProveDlog

import scala.concurrent.Future

case class MiningApiRoute(miner: ActorRef, ergoSettings: ErgoSettings)
                         (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ApiCodecs {

  val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  implicit val addressEncoder: Encoder[ErgoAddress] = ErgoAddressJsonEncoder(ergoSettings).encoder

  override val route: Route = pathPrefix("mining") {
    candidateR ~
      solutionR ~
      rewardAddressR
  }

  def candidateR: Route = (path("candidate") & pathEndOrSingleSlash & get) {
    val candidateF = (miner ? ErgoMiner.PrepareCandidate).mapTo[Future[ExternalCandidateBlock]].flatten
    ApiResponse(candidateF)
  }

  def solutionR: Route = (path("solution") & post & entity(as[AutolykosSolution])) { solution =>
    val result = if (ergoSettings.nodeSettings.useExternalMiner) {
      (miner ? solution).mapTo[Future[Unit]].flatten
    } else {
      Future.failed(new Exception("External miner support is inactive"))
    }
    ApiResponse(result)
  }

  def rewardAddressR: Route = (path("rewardAddress") & get) {
    val addressF = (miner ? ErgoMiner.ReadMinerPk).mapTo[Option[ProveDlog]]
      .flatMap {
        _.fold[Future[ErgoAddress]](Future.failed(new Exception("Miner PK not initialized")))(pk => {
          val script = ErgoScriptPredef.rewardOutputScript(ergoSettings.chainSettings.monetary.minerRewardDelay, pk)
          Future.successful(Pay2SAddress(script)(ergoSettings.addressEncoder))
        })
      }
    ApiResponse(addressF.map(address => Json.obj("rewardAddress" -> address.asJson)))
  }

}
