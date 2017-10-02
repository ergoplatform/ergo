package org.ergoplatform.api.routes

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.server.Route
import io.circe.syntax._
import io.swagger.annotations.{Api, ApiOperation, ApiResponse, ApiResponses}
import org.ergoplatform.api.services.StateService
import scorex.core.api.http.{ScorexApiResponse, SuccessApiResponse}
import scorex.core.settings.Settings
import scorex.crypto.encode.Base58

import scala.concurrent.Future

@Path("/state")
@Api(value = "/state", produces = "application/json")
case class StateApiRoute(service: StateService,
                         override val settings: Settings)
                        (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute {

  private def getVersion: Future[ScorexApiResponse] = service.getVersion.map { v =>
    val version = Base58.encode(v)
    SuccessApiResponse(Map("version" -> version).asJson)
  }

  private def getType: Future[ScorexApiResponse] = service.getType.map { t =>
    SuccessApiResponse(Map("type" -> t).asJson)
  }

  override val route = pathPrefix("state") {
    concat(version, stateType)
  }

  @Path("/version")
  @ApiOperation(value = "Current state version", httpMethod = "GET, POST")
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with with version")))
  def version: Route = path("version") {
    (get | post) {
      toJsonResponse(getVersion)
    }
  }

  @Path("/type")
  @ApiOperation(value = "Current state type", httpMethod = "GET, POST")
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with with type (utxo or digest)")))
  def stateType: Route = path("type") {
    (get | post) {
      toJsonResponse(getType)
    }
  }
}
