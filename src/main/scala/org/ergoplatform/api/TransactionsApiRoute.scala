package org.ergoplatform.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import io.circe.generic.auto._
import io.circe.syntax._
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.history.storage.modifierprocessors.blocktransactions.TransactionValidator
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings

import scala.concurrent.Future

case class TransactionsApiRoute(readersHolder: ActorRef, nodeViewActorRef: ActorRef, restApiSettings: RESTApiSettings)
                          (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with FailFastCirceSupport {

  override val route: Route = pathPrefix("transactions") {
    getUnconfirmedTransactionsR ~ sendTransactionR
  }

  override val settings: RESTApiSettings = restApiSettings

  private def getMemPool: Future[ErgoMemPoolReader] = (readersHolder ? GetReaders).mapTo[Readers].map(_.m)

  private def getUnconfirmedTransactions(limit: Int): Future[Json] = getMemPool.map { p =>
    p.take(limit).toSeq.map(_.asJson).asJson
  }

  def sendTransactionR: Route = (post & entity(as[TransactionView])) { proto =>
    // todo not only id validation?
    val tx = proto.toTransaction
    TransactionValidator.validateProto(proto, tx) toApi {
      nodeViewActorRef ! LocallyGeneratedTransaction[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction](tx)
      ApiResponse.OK
    }
  }

  def getUnconfirmedTransactionsR: Route = (path("unconfirmed") & get & paging) { (_ , limit) =>
    // todo offset
    getUnconfirmedTransactions(limit).okJson()
  }
}
