package org.ergoplatform.api.routes

import javax.ws.rs.Path

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.swagger.annotations.{Api, ApiOperation, ApiResponse, ApiResponses}
import org.ergoplatform.local.ErgoMiner.{MiningStatusRequest, MiningStatusResponse}
import scorex.core.api.http.SuccessApiResponse
import scorex.core.settings.RESTApiSettings

@Path("/mining")
@Api(value = "/mining", produces = "application/json")
case class MiningApiRoute(miner: ActorRef, override val settings: RESTApiSettings)
                         (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute {
  override val route = pathPrefix("mining") {
    concat(status)
  }

  @Path("/status")
  @ApiOperation(value = "Current state of node miner", httpMethod = "GET")
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with current state")))
  def status: Route = path("status") {
    get(toJsonResponse((miner ? MiningStatusRequest).mapTo[MiningStatusResponse].map(r =>
      SuccessApiResponse(r.json))))
  }
}
