package org.ergoplatform.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.pattern.ask
import akka.http.scaladsl.server.Route
import org.ergoplatform.local.ErgoMiner
import org.ergoplatform.mining.external.{ExternalAutolykosSolution, ExternalCandidateBlock}
import org.ergoplatform.settings.ErgoSettings
import scorex.core.settings.RESTApiSettings
import scorex.core.api.http.ApiResponse

case class MiningApiRoute(miner: ActorRef, ergoSettings: ErgoSettings)
                         (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ApiCodecs {

  val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  override val route: Route = (pathPrefix("mining") & withCors & withAuth) {
    candidateR ~ solutionR
  }

  def candidateR: Route = (path("candidate") & pathEndOrSingleSlash & get) {
    ApiResponse((miner ? ErgoMiner.PrepareExternalCandidate).mapTo[ExternalCandidateBlock])
  }

  def solutionR: Route = (path("solution") & post & entity(as[ExternalAutolykosSolution])) { solution =>
    miner ! solution
    ApiResponse.OK
  }

}
