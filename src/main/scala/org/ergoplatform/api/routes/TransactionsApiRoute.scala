package org.ergoplatform.api.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.Json
import io.circe.syntax._
import io.circe.generic.auto._
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{DigestState, UtxoState}
import org.ergoplatform.nodeView.wallet.ErgoWallet
import scorex.core.LocalInterface.LocallyGeneratedTransaction
import scorex.core.NodeViewHolder.GetDataFromCurrentView
import scorex.core.settings.RESTApiSettings
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport

import scala.concurrent.Future

case class TransactionsApiRoute(nodeViewActorRef: ActorRef, restApiSettings: RESTApiSettings, digest: Boolean)
                               (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with FailFastCirceSupport {

  override val route: Route = pathPrefix("transactions") {
    concat(sendTransactionR, getTransactionByIdR, getUnconfirmedTransactionsR)
  }

  override val settings: RESTApiSettings = restApiSettings

  private val historyRequest = if (digest) {
    GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, ErgoHistory](_.history)
  } else {
    GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, ErgoHistory](_.history)
  }

  private val poolRequest = if (digest) {
    GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, ErgoMemPool](_.pool)
  } else {
    GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, ErgoMemPool](_.pool)
  }

  private def getHistory = (nodeViewActorRef ? historyRequest).mapTo[ErgoHistory]

  private def getMemPool = (nodeViewActorRef ? poolRequest).mapTo[ErgoMemPool]

  private def getTransactionById(id: String): Future[Option[Json]] = getHistory.map {
    _.modifierById(id)
  }.map {
    // todo how to get tx by id?
    _.map { modifier => ??? }
  }

  private def getUnconfirmedTransactions(limit: Int): Future[Json] = getMemPool.map {
    _.take(limit).toSeq
  }.map(_.map(_.json).asJson)

  def sendTransactionR: Route =
    post {
      entity(as[AnyoneCanSpendTransaction]) { tx =>
        complete {
          // todo validation?
          nodeViewActorRef ! LocallyGeneratedTransaction[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction](tx)
          StatusCodes.OK
        }
      }
    }

  // todo tx id validation
  def getTransactionByIdR: Route = path(Segment) { id =>
    get {
      toJsonOptionalResponse {
        getTransactionById(id)
      }
    }
  }

  def getUnconfirmedTransactionsR: Route = get {
    parameters('limit.as[Int] ? 50, 'offset.as[Int] ? 0) {
      case (limit, offset) =>
        // todo offset
        toJsonResponse(getUnconfirmedTransactions(limit))
    }
  }
}
