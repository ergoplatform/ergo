package org.ergoplatform.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.wallet.ErgoWalletReader
import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings
import scala.concurrent.Future
import akka.pattern.ask

case class WalletApiRoute(readersHolder: ActorRef, nodeViewActorRef: ActorRef, settings: RESTApiSettings)
                          (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ApiCodecs {

  private def getWallet: Future[ErgoWalletReader] = (readersHolder ? GetReaders).mapTo[Readers].map(_.w)

  override val route: Route = (pathPrefix("wallet") & withCors) {
    balancesR ~ sendTransactionR ~ unconfirmedBalanceR
  }

  def sendTransactionR: Route = (post & entity(as[ErgoTransaction])) { tx =>
    // todo validation?
    nodeViewActorRef ! LocallyGeneratedTransaction[ErgoTransaction](tx)
    ApiResponse.OK
  }

  def balancesR: Route = (path("balances") & get) {
    ApiResponse(getWallet.flatMap(_.confirmedBalances()))
  }

  def unconfirmedBalanceR: Route = (path("balances" / "unconfirmed") & get) {
    ApiResponse(getWallet.flatMap(_.unconfirmedBalances()))
  }

}
