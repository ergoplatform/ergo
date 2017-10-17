package org.ergoplatform.api.routes

import javax.ws.rs.Path

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.syntax._
import io.swagger.annotations.{Api, ApiOperation, ApiResponse, ApiResponses}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{DigestState, UtxoState}
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.ErgoSettings
import scorex.core.NodeViewHolder.GetDataFromCurrentView
import scorex.core.api.http.{ScorexApiResponse, SuccessApiResponse}
import scorex.core.settings.{RESTApiSettings, ScorexSettings}
import scorex.crypto.encode.Base58

import scala.concurrent.Future

@Path("/state")
@Api(value = "/state", produces = "application/json")
case class StateApiRoute(nodeViewActorRef: ActorRef, ergoSettings: ErgoSettings)
                        (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute {

  override val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  private val digest: Boolean = ergoSettings.nodeSettings.ADState

  private val request = if (digest) {
    GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, DigestState](_.state)
  } else {
    GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, UtxoState](_.state)
  }

  private def getState = if (digest) {
    (nodeViewActorRef ? request).mapTo[DigestState]
  } else {
    (nodeViewActorRef ? request).mapTo[UtxoState]
  }

  private def getVersion: Future[ScorexApiResponse] = getState.map{ _.version }.map { v =>
    val version = Base58.encode(v)
    SuccessApiResponse(Map("version" -> version).asJson)
  }

  private def getType: Future[ScorexApiResponse] = Future.successful {
    if (digest) "digest" else "utxo"
  }.map { t =>
    SuccessApiResponse(Map("type" -> t).asJson)
  }

  override val route = pathPrefix("state") {
    concat(version, stateType)
  }

  @Path("/version")
  @ApiOperation(value = "Current state version", httpMethod = "GET")
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with version")))
  def version: Route = path("version") {
    get {
      toJsonResponse(getVersion)
    }
  }

  @Path("/type")
  @ApiOperation(value = "Current state type", httpMethod = "GET")
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with type (utxo or digest)")))
  def stateType: Route = path("type") {
    get {
      toJsonResponse(getType)
    }
  }
}
