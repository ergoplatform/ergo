package org.ergoplatform.api.routes

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.server.Route
import io.circe.Json
import io.swagger.annotations.{Api, ApiOperation, ApiResponse, ApiResponses}
import scorex.core.api.http.SuccessApiResponse
import scorex.core.settings.RESTApiSettings

@Path("/debug")
@Api(value = "/debug", produces = "application/json")
case class DebugApiRoute(override val settings: RESTApiSettings)
                   (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute {
  override val route = pathPrefix("debug") {
    concat(status)
  }

  @Path("/status")
  @ApiOperation(value = "Current state of node", httpMethod = "GET")
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with current state")))
  def status: Route = path("status") {
    get {
      // todo different statuses?
      toJsonResponse(SuccessApiResponse(Json.obj("status" -> Json.fromString("ok"))))
    }
  }
}
