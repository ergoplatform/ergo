package org.ergoplatform.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.Encoder
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.wallet._
import org.ergoplatform.settings.ErgoSettings
import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import scorex.core.api.http.ApiError.BadRequest
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings

import scala.concurrent.Future
import scala.util.{Failure, Success}

case class WalletApiRoute(readersHolder: ActorRef, nodeViewActorRef: ActorRef, ergoSettings: ErgoSettings)
                         (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ApiCodecs {

  implicit val paymentRequestDecoder = new PaymentRequestDecoder(ergoSettings)
  implicit val addressEncoder = paymentRequestDecoder.addressEncoders.encoder

  val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  private def withWalletOp[T](op: ErgoWalletReader => Future[T])(toRoute: T => Route): Route = {
    onSuccess((readersHolder ? GetReaders).mapTo[Readers].flatMap(r => op(r.w)))(toRoute)
  }

  private def withWallet[T : Encoder](op: ErgoWalletReader => Future[T]): Route = {
    withWalletOp(op)(ApiResponse.apply[T])
  }

  override val route: Route = (pathPrefix("wallet") & withCors) {
    balancesRoute ~
      unconfirmedBalanceRoute ~
      addressesRoute ~
      generateTransactionRoute ~
      generateAndSendTransactionRoute
  }

  def generateTransactionRoute: Route = (path("transaction" / "generate") & post
    & entity(as[Seq[PaymentRequest]])) { payments =>
    withWalletOp(_.generateTransaction(payments)) {
      case Failure(e) => BadRequest(s"Bad payment request $payments. ${Option(e.getMessage).getOrElse(e.toString)}")
      case Success(tx) => ApiResponse(tx)
    }
  }

  def generateAndSendTransactionRoute: Route = (path("transaction" / "payment") & post
    & entity(as[Seq[PaymentRequest]])) { payments =>
    withWalletOp(_.generateTransaction(payments)) {
      case Failure(e) => BadRequest(s"Bad payment request $payments. ${Option(e.getMessage).getOrElse(e.toString)}")
      case Success(tx) =>
        nodeViewActorRef ! LocallyGeneratedTransaction[ErgoTransaction](tx)
        ApiResponse(tx.id)
    }
  }

  def balancesRoute: Route = (path("balances") & get) {
    withWallet(_.confirmedBalances())
  }

  def unconfirmedBalanceRoute: Route = (path("balances" / "unconfirmed") & get) {
    withWallet(_.unconfirmedBalances())
  }

  def addressesRoute: Route = (path("addresses") & get) {
    withWallet(_.walletAddresses())
  }

}
