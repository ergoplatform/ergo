package org.ergoplatform.api.routes

import javax.ws.rs.Path

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import io.circe.Json
import io.swagger.annotations.{Api, ApiOperation, ApiResponse, ApiResponses}
import org.ergoplatform.nodeView.ErgoNodeViewHolder
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{DigestState, ErgoState}
import org.ergoplatform.nodeView.wallet.ErgoWallet
import scorex.core.NodeViewHolder.GetDataFromCurrentView
import scorex.core.api.http.SuccessApiResponse
import scorex.core.settings.RESTApiSettings
import akka.pattern.ask
import org.ergoplatform.modifiers.history.Header
import io.circe.syntax._
import org.ergoplatform.settings.Algos

@Path("/debug")
@Api(value = "/debug", produces = "application/json")
case class DebugApiRoute(nodeViewActorRef: ActorRef, override val settings: RESTApiSettings)
                   (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute {
  override val route = pathPrefix("debug") {
    concat(status)
  }

  type S = ErgoState[_]


    @Path("/status")
  @ApiOperation(value = "Current state of node", httpMethod = "GET")
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with current state")))
  def status: Route = path("status") {
      val height = 1
      get {
        toJsonResponse {
          val request = GetDataFromCurrentView[ErgoHistory, S, ErgoWallet, ErgoMemPool, Json]{ nvs =>
            val bestHeader = nvs.history.bestHeaderOpt
            val bestFullBlock = nvs.history.bestFullBlockOpt
            Map(
              "headers-height" -> bestHeader.map(_.height).getOrElse(-1).asJson,
              "full-height" -> bestFullBlock.map(_.header.height).getOrElse(-1).asJson,
              "best-header-id" -> bestHeader.map(_.encodedId).getOrElse("None").asJson,
              "best-full-header-id" -> bestFullBlock.map(_.header.encodedId).getOrElse("None").asJson,
              "utx-size" -> nvs.pool.size.asJson,
              "state-root" -> Algos.encode(nvs.state.rootHash()).asJson,
            ).asJson
          }
          (nodeViewActorRef ? request).mapTo[Json].map { json =>
            SuccessApiResponse(json)
          }
        }
      }
    }

}
