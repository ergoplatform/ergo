package org.ergoplatform.http.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.state.ErgoStateReader
import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import scorex.core.api.http.ApiError.BadRequest
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings
import scorex.core.transaction.state.TransactionValidation

import scala.concurrent.Future

case class TransactionsApiRoute(readersHolder: ActorRef, nodeViewActorRef: ActorRef, settings: RESTApiSettings)
                               (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ApiCodecs {

  override val route: Route = pathPrefix("transactions") {
    getUnconfirmedTransactionsR ~ sendTransactionR
  }

  private def getMemPool: Future[ErgoMemPoolReader] = (readersHolder ? GetReaders).mapTo[Readers].map(_.m)

  private def getState: Future[ErgoStateReader] = (readersHolder ? GetReaders).mapTo[Readers].map(_.s)

  private def getUnconfirmedTransactions(offset: Int, limit: Int): Future[Json] = getMemPool.map { p =>
    p.getAll.slice(offset, offset + limit).map(_.asJson).asJson
  }

  def sendTransactionR: Route = (post & entity(as[ErgoTransaction])) { tx =>
    onSuccess {
      getState
        .map {
          case validator: TransactionValidation[ErgoTransaction@unchecked] =>
            validator.validate(tx)
          case _ =>
            tx.statelessValidity
        }
    } {
      _.fold(
        e => BadRequest(s"Malformed transaction: ${e.getMessage}"),
        _ => {
          nodeViewActorRef ! LocallyGeneratedTransaction[ErgoTransaction](tx)
          ApiResponse(tx.id)
        }
      )
    }
  }

  def getUnconfirmedTransactionsR: Route = (path("unconfirmed") & get & paging) { (offset, limit) =>
    ApiResponse(getUnconfirmedTransactions(offset, limit))
  }

}
