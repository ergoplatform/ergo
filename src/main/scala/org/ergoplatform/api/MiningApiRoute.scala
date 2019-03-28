package org.ergoplatform.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import org.ergoplatform.local.ErgoMiner
import org.ergoplatform.mining.{AutolykosSolution, ExternalCandidateBlock}
import org.ergoplatform.settings.ErgoSettings
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings

import scala.concurrent.Future

case class MiningApiRoute(miner: ActorRef, ergoSettings: ErgoSettings)
                         (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ApiCodecs {

  val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  override val route: Route = (pathPrefix("mining") & withCors & withAuth) {
    candidateR ~ solutionR
  }

  def candidateR: Route = (path("candidate") & pathEndOrSingleSlash & get) {
    val result = (miner ? ErgoMiner.PrepareCandidate).mapTo[Future[ExternalCandidateBlock]].flatten
    ApiResponse(result)
  }

  def solutionR: Route = (path("solution") & post & entity(as[AutolykosSolution])) { solution =>
    val result = if (ergoSettings.nodeSettings.useExternalMiner) {
      (miner ? solution).mapTo[Future[Unit]].flatten
    } else {
      Future.failed(new Exception("External miner support is inactive"))
    }
    ApiResponse(result)
  }

}
