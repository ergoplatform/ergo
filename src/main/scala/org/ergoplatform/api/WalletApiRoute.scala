package org.ergoplatform.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings

case class WalletApiRoute(nodeViewActorRef: ActorRef, settings: RESTApiSettings)
                          (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute {

  override val route: Route = (pathPrefix("wallet") & withCors) {
    balanceR ~ sendTransactionR
  }

  def sendTransactionR: Route = (post & entity(as[ErgoTransaction])) { tx =>
    // todo validation?
    nodeViewActorRef ! LocallyGeneratedTransaction[ErgoTransaction](tx)
    ApiResponse.OK
  }

  def balanceR: Route = (path("balance") & get) {
    ???
  }
}
