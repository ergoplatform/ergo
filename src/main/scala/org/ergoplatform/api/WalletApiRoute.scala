package org.ergoplatform.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.{Directive1, Route}
import akka.pattern.ask
import io.circe.{Encoder, Json}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.wallet._
import org.ergoplatform.nodeView.wallet.requests._
import org.ergoplatform.settings.ErgoSettings
import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import scorex.core.api.http.ApiError.BadRequest
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings
import sigmastate.Values

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

case class WalletApiRoute(readersHolder: ActorRef, nodeViewActorRef: ActorRef, ergoSettings: ErgoSettings)
                         (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ApiCodecs {

  implicit val paymentRequestDecoder: PaymentRequestDecoder = new PaymentRequestDecoder(ergoSettings)
  implicit val assetIssueRequestDecoder: AssetIssueRequestDecoder = new AssetIssueRequestDecoder(ergoSettings)
  implicit val requestsHolderDecoder: RequestsHolderDecoder = new RequestsHolderDecoder(ergoSettings)
  implicit val ergoAddressEncoder: ErgoAddressEncoder = paymentRequestDecoder.addressEncoders
  implicit val addressEncoder: Encoder[ErgoAddress] = paymentRequestDecoder.addressEncoders.encoder

  val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  override val route: Route = (pathPrefix("wallet") & withCors & withAuth) {
    balancesR ~
      unconfirmedBalanceR ~
      addressesR ~
      generateTransactionR ~
      generatePaymentTransactionR ~
      generateAssetIssueTransactionR ~
      sendTransactionR ~
      sendPaymentTransactionR ~
      sendAssetIssueTransactionR
  }

  private val fee: Directive1[Option[Long]] = entity(as[Json]).flatMap { p =>
    Try(p.hcursor.downField("fee").as[Long]) match {
      case Success(Right(value)) => provide(Some(value))
      case _ => provide(None)
    }
  }

  private def withFee(requests: Seq[TransactionRequest], feeOpt: Option[Long]): Seq[TransactionRequest] = {
    requests :+ PaymentRequest(Pay2SAddress(Values.TrueLeaf),
      feeOpt.getOrElse(ergoSettings.walletSettings.defaultTransactionFee), None, None)
  }

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

  def sendTransactionR: Route = (path("transaction" / "send") & post
    & entity(as[RequestsHolder]))(holder => sendTransaction(holder.requestsWithFee))

  def generateTransactionR: Route = (path("transaction" / "generate") & post
    & entity(as[RequestsHolder]))(holder => generateTransaction(holder.requestsWithFee))

  def generatePaymentTransactionR: Route = (path( "payment" / "generate") & post
    & entity(as[Seq[PaymentRequest]]) & fee) { (requests, feeOpt) =>
      generateTransaction(withFee(requests, feeOpt))
    }

  def sendPaymentTransactionR: Route = (path("payment" / "send") & post
    & entity(as[Seq[PaymentRequest]]) & fee) { (requests, feeOpt) =>
      sendTransaction(withFee(requests, feeOpt))
    }

  def generateAssetIssueTransactionR: Route = (path("assets" / "generate") & post
    & entity(as[Seq[AssetIssueRequest]]) & fee) { (requests, feeOpt) =>
      generateTransaction(withFee(requests, feeOpt))
    }

  def sendAssetIssueTransactionR: Route = (path("assets" / "issue") & post
    & entity(as[Seq[AssetIssueRequest]]) & fee) { (requests, feeOpt) =>
      sendTransaction(withFee(requests, feeOpt))
    }

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
