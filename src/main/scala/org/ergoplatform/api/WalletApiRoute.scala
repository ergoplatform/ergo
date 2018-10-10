package org.ergoplatform.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.Encoder
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.wallet._
import org.ergoplatform.nodeView.wallet.requests._
import org.ergoplatform.settings.ErgoSettings
import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import scorex.core.api.http.ApiError.BadRequest
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings

import scala.concurrent.Future
import scala.util.{Failure, Success}

case class WalletApiRoute(readersHolder: ActorRef, nodeViewActorRef: ActorRef, ergoSettings: ErgoSettings)
                         (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ApiCodecs {

  implicit val paymentRequestDecoder: PaymentRequestDecoder = new PaymentRequestDecoder(ergoSettings)
  implicit val assetIssueRequestDecoder: AssetIssueRequestDecoder = new AssetIssueRequestDecoder(ergoSettings)
  implicit val addressEncoder: Encoder[ErgoAddress] = paymentRequestDecoder.addressEncoders.encoder

  val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  private def withWalletOp[T](op: ErgoWalletReader => Future[T])(toRoute: T => Route): Route = {
    onSuccess((readersHolder ? GetReaders).mapTo[Readers].flatMap(r => op(r.w)))(toRoute)
  }

  private def withWallet[T: Encoder](op: ErgoWalletReader => Future[T]): Route = {
    withWalletOp(op)(ApiResponse.apply[T])
  }

  private def generateTransaction(requests: Seq[TransactionRequest]): Route =
    withWalletOp(_.generateTransaction(requests)) {
      case Failure(e) => BadRequest(s"Bad request $requests. ${Option(e.getMessage).getOrElse(e.toString)}")
      case Success(tx) => ApiResponse(tx)
    }

  private def sendTransaction(requests: Seq[TransactionRequest]): Route =
    withWalletOp(_.generateTransaction(requests)) {
      case Failure(e) => BadRequest(s"Bad request $requests. ${Option(e.getMessage).getOrElse(e.toString)}")
      case Success(tx) =>
        nodeViewActorRef ! LocallyGeneratedTransaction[ErgoTransaction](tx)
        ApiResponse(tx.id)
    }

  override val route: Route = (pathPrefix("wallet") & withCors & withAuth) {
    balancesR ~
      unconfirmedBalanceR ~
      addressesR ~
      generatePaymentTransactionR ~
      generateAssetIssueTransactionR ~
      sendPaymentTransactionR ~
      sendAssetIssueTransactionR
  }

  def generatePaymentTransactionR: Route = (path( "payment" / "generate") & post
    & entity(as[Seq[PaymentRequest]]))(generateTransaction)

  def sendPaymentTransactionR: Route = (path("payment") & post
    & entity(as[Seq[PaymentRequest]]))(sendTransaction)

  def generateAssetIssueTransactionR: Route = (path("assets" / "issue" / "generate") & post
    & entity(as[Seq[AssetIssueRequest]]))(generateTransaction)

  def sendAssetIssueTransactionR: Route = (path("assets" / "issue") & post
    & entity(as[Seq[AssetIssueRequest]]))(sendTransaction)

  def balancesR: Route = (path("balances") & get) {
    withWallet(_.confirmedBalances())
  }

  def unconfirmedBalanceR: Route = (path("balances" / "unconfirmed") & get) {
    withWallet(_.unconfirmedBalances())
  }

  def addressesR: Route = (path("addresses") & get) {
    withWallet(_.trackedAddresses())
  }

}
