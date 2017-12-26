package org.ergoplatform.api.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import io.circe.generic.auto._
import io.circe.syntax._
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import scorex.core.LocalInterface.LocallyGeneratedTransaction
import scorex.core.ModifierId
import scorex.core.settings.RESTApiSettings
import scorex.crypto.encode.Base16

import scala.concurrent.Future

case class TransactionsApiRoute(readersHolder: ActorRef, nodeViewActorRef: ActorRef, restApiSettings: RESTApiSettings, digest: Boolean)
                               (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with FailFastCirceSupport {

  override val route: Route = pathPrefix("transactions") {
    getUnconfirmedTransactionsR ~ sendTransactionR ~getTransactionByIdR
  }

  override val settings: RESTApiSettings = restApiSettings

  private def getHistory: Future[ErgoHistoryReader] = (readersHolder ? GetReaders).mapTo[Readers].map(_.h.get)

  private def getMemPool: Future[ErgoMemPoolReader] = (readersHolder ? GetReaders).mapTo[Readers].map(_.m.get)

  private def getTransactionById(id: ModifierId): Future[Option[Json]] = getHistory.map {
    _.modifierById(id)
  }.map {
    // todo how to get tx by id?
    _.map { modifier => ??? }
  }

  private def getUnconfirmedTransactions(limit: Int): Future[Json] = getMemPool.map {
    _.take(limit).toSeq
  }.map(_.map(_.json).asJson)

  //todo There in no codec for "AnyoneCanSpendTransaction" need to make one.
  def sendTransactionR: Route = (post & entity(as[AnyoneCanSpendTransaction])) { tx =>
    // todo validation?
    nodeViewActorRef ! LocallyGeneratedTransaction[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction](tx)
    complete(StatusCodes.OK)
  }

  // todo tx id validation
  def getTransactionByIdR: Route = (path(Segment) & get) { id =>
    getTransactionById(ModifierId @@ Base16.decode(id)).okJson()
  }

  def getUnconfirmedTransactionsR: Route = (path("unconfirmed") & get & paging) { (offset, limit) =>
    // todo offset
    getUnconfirmedTransactions(limit).okJson()
  }
}
