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
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import scorex.core.LocalInterface.LocallyGeneratedTransaction
import scorex.core.settings.RESTApiSettings

import scala.concurrent.Future

case class TransactionsApiRoute(readersHolder: ActorRef, nodeViewActorRef: ActorRef, restApiSettings: RESTApiSettings, digest: Boolean)
                               (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with FailFastCirceSupport {

  override val route: Route = pathPrefix("transactions") {
    getUnconfirmedTransactionsR ~ sendTransactionR ~ getTransactionByIdR
  }

  override val settings: RESTApiSettings = restApiSettings

  private def getMemPool: Future[Option[ErgoMemPoolReader]] = (readersHolder ? GetReaders).mapTo[Readers].map(_.m)

  private def getUnconfirmedTransactions(limit: Int): Future[Json] = getMemPool.map {
    _.map {_.take(limit).toSeq }.map(_.map(_.json).asJson).getOrElse(Json.Null)
  }

  //todo There in no codec for "AnyoneCanSpendTransaction" need to make one.
  def sendTransactionR: Route = (post & entity(as[AnyoneCanSpendTransaction])) { tx =>
    // todo validation?
    nodeViewActorRef ! LocallyGeneratedTransaction[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction](tx)
    complete(StatusCodes.OK)
  }

  // todo tx id validation
  // todo how to get tx by id?
  def getTransactionByIdR: Route = (path(Segment) & get) { id => ??? }

  def getUnconfirmedTransactionsR: Route = (path("unconfirmed") & get & paging) { (_ , limit) =>
    // todo offset
    getUnconfirmedTransactions(limit).okJson()
  }
}
