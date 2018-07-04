package org.ergoplatform.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.utils.JsonEncoders
import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings

import scala.concurrent.Future

case class TransactionsApiRoute(readersHolder: ActorRef, nodeViewActorRef: ActorRef, settings: RESTApiSettings)
                               (implicit val context: ActorRefFactory, enc: JsonEncoders) extends ErgoBaseApiRoute {

  import enc._

  override val route: Route = (pathPrefix("transactions") & withCors) {
    getUnconfirmedTransactionsR ~ sendTransactionR
  }

  private def getMemPool: Future[ErgoMemPoolReader] = (readersHolder ? GetReaders).mapTo[Readers].map(_.m)

  private def getUnconfirmedTransactions(limit: Int): Future[Json] = getMemPool.map { p =>
    p.take(limit).toSeq.map(_.asJson).asJson
  }

  def sendTransactionR: Route = (post & entity(as[ErgoTransaction])) { tx =>
    // todo validation?
    nodeViewActorRef ! LocallyGeneratedTransaction[ErgoTransaction](tx)
    ApiResponse.OK
  }

  def getUnconfirmedTransactionsR: Route = (path("unconfirmed") & get & paging) { (_ , limit) =>
    // todo offset
    ApiResponse(getUnconfirmedTransactions(limit))
  }
}
