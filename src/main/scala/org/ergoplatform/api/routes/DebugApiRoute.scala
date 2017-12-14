package org.ergoplatform.api.routes

import javax.ws.rs.Path

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.syntax._
import io.swagger.annotations.{Api, ApiOperation, ApiResponse, ApiResponses}
import org.ergoplatform.Version
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.settings.Algos
import scorex.core.api.http.SuccessApiResponse
import scorex.core.settings.RESTApiSettings

@Path("/debug")
@Api(value = "/debug", produces = "application/json")
case class DebugApiRoute(readersHolder: ActorRef, override val settings: RESTApiSettings, nodeId: Array[Byte])
                        (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute {
  override val route = pathPrefix("debug") {
    concat(status)
  }

  type S = ErgoState[_]


  @Path("/status")
  @ApiOperation(value = "Current state of node", httpMethod = "GET")
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with current state")))
  def status: Route = path("status") {
    get {
      toJsonResponse {
        (readersHolder ? GetReaders).mapTo[Readers].map { readers =>
          val bestHeader = readers.h.flatMap(_.bestHeaderOpt)
          val bestFullBlock = readers.h.flatMap(_.bestFullBlockOpt)
          val poolSize = readers.m.map(_.size).getOrElse(-1)
          val stateRoot = readers.s.map(s => Algos.encode(s.rootHash)).getOrElse("Undefined")
          val json = Map(
            "nodeId" -> Algos.encode(nodeId).asJson,
            "nodeVersion" -> Version.VersionString.asJson,
            "headers-height" -> bestHeader.map(_.height).getOrElse(-1).asJson,
            "full-height" -> bestFullBlock.map(_.header.height).getOrElse(-1).asJson,
            "best-header-id" -> bestHeader.map(_.encodedId).getOrElse("None").asJson,
            "best-full-header-id" -> bestFullBlock.map(_.header.encodedId).getOrElse("None").asJson,
            "difficulty" -> bestFullBlock.map(_.header.requiredDifficulty.toString).getOrElse("None").asJson,
            "utx-size" -> poolSize.asJson,
            "state-root" -> stateRoot.asJson
          ).asJson
          SuccessApiResponse(json)
        }
      }
    }
  }

}
