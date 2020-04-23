package org.ergoplatform.http.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.state.{ErgoStateReader, UtxoStateReader}
import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import scorex.core.api.http.ApiError.BadRequest
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings

import scala.concurrent.Future
import scala.util.Try

case class TransactionsApiRoute(readersHolder: ActorRef, nodeViewActorRef: ActorRef, settings: RESTApiSettings)
                               (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ApiCodecs {

  override val route: Route = pathPrefix("transactions") {
    getUnconfirmedTransactionsR ~ sendTransactionR ~ checkTransactionR
  }

  private def getMemPool: Future[ErgoMemPoolReader] = (readersHolder ? GetReaders).mapTo[Readers].map(_.m)

  private def getStateAndPool: Future[(ErgoStateReader, ErgoMemPoolReader)] =
    (readersHolder ? GetReaders).mapTo[Readers].map { rs =>
      (rs.s, rs.m)
    }

  private def getUnconfirmedTransactions(offset: Int, limit: Int): Future[Json] = getMemPool.map { p =>
    p.getAll.slice(offset, offset + limit).map(_.asJson).asJson
  }

  def sendTransactionR: Route = (post & entity(as[ErgoTransaction])) { tx =>
    onSuccess {
      getStateAndPool
        .map {
          case (utxo: UtxoStateReader, mp: ErgoMemPoolReader) =>
            utxo.withTransactions(mp.getAll).validate(tx)
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

  def checkTransactionR: Route = (path("check") & post & entity(as[ErgoTransaction])) { tx =>
    onSuccess {
      getStateAndPool
        .map {
          case (utxo: UtxoStateReader, mp: ErgoMemPoolReader) =>
            utxo.withTransactions(mp.getAll).validate(tx)
          case _ =>
            Try(new Exception("Transaction validation is not supported in absence of UTXO set"))
        }
    } {
      _.fold(
        e => BadRequest(s"Validation failed: ${e.getMessage}"),
        _ => ApiResponse(tx.id)
      )
    }
  }

}
