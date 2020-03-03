package org.ergoplatform.http.api


import akka.pattern.ask
import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import io.circe.Encoder
import io.circe.syntax._
import org.ergoplatform.modifiers.history.popow.{PoPowHeader, PoPowProof}
import org.ergoplatform.nodeView.ErgoReadersHolder.GetDataFromHistory
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.settings.ErgoSettings
import scorex.core.api.http.ApiError.BadRequest
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings
import scorex.util.ModifierId

import scala.concurrent.Future
import scala.util.Try

case class ClientApiRoute(viewHolderRef: ActorRef, readersHolder: ActorRef, ergoSettings: ErgoSettings)
                         (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ApiCodecs {

  val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  override val route: Route = pathPrefix("client") {
    getPopowHeaderByHeaderIdR ~
      getPopowHeaderByHeightR ~
      getPopowProofR
  }

  private def getHistory: Future[ErgoHistoryReader] =
    (readersHolder ? GetDataFromHistory[ErgoHistoryReader](r => r)).mapTo[ErgoHistoryReader]

  private def getPopowHeaderById(headerId: ModifierId): Future[Option[PoPowHeader]] =
    getHistory.map { history =>
      history.popowHeader(headerId)
    }

  private def getPopowHeaderByHeight(height: Int): Future[Option[PoPowHeader]] =
    getHistory.map { history =>
      history.popowHeader(height)
    }

  private def getPopowProof(): Future[Try[PoPowProof]] =
    getHistory.map { history =>
      history.popowProof()
    }

  def getPopowHeaderByHeaderIdR: Route = (pathPrefix("popowHeaderById") & modifierId & get) { headerId =>
    ApiResponse(getPopowHeaderById(headerId))
  }

  def getPopowHeaderByHeightR: Route = (pathPrefix("popowHeaderByHeight" / IntNumber) & get) { headerId =>
    ApiResponse(getPopowHeaderByHeight(headerId))
  }

  def getPopowProofR: Route = (pathPrefix("popowProof") & get) {
    implicit val popowProofEncoder: Encoder[PoPowProof] = PoPowProof.popowProofEncoder

    onSuccess(getPopowProof()){_.fold(
      e => BadRequest(e.getMessage),
      proof => ApiResponse(proof.asJson)
    )}
  }

}
