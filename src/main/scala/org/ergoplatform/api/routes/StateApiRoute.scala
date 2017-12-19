package org.ergoplatform.api.routes

import javax.ws.rs.Path

import akka.actor.{ActorRef, ActorRefFactory}
import akka.pattern.ask
import akka.util.Timeout
import io.circe.syntax._
import io.swagger.annotations.Api
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{DigestState, UtxoState}
import org.ergoplatform.nodeView.wallet.ErgoWallet
import scorex.core.NodeViewHolder.GetDataFromCurrentView
import scorex.core.api.http.{ScorexApiResponse, SuccessApiResponse}
import scorex.core.settings.RESTApiSettings
import scorex.crypto.encode.Base58

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

@Path("/state")
@Api(value = "/state", produces = "application/json")
case class StateApiRoute(nodeViewActorRef: ActorRef, settings: RESTApiSettings, digest: Boolean)
                        (implicit val context: ActorRefFactory) {

  implicit val timeout = Timeout(settings.timeout)

  (nodeViewActorRef ? request).mapTo[DigestState]

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
}
