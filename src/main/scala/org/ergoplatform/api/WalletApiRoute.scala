package org.ergoplatform.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.{Directive1, Route}
import akka.pattern.ask
import io.circe.{Encoder, Json}
import org.ergoplatform._
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.wallet._
import org.ergoplatform.nodeView.wallet.requests._
import org.ergoplatform.settings.ErgoSettings
import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import scorex.core.api.http.ApiError.BadRequest
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings
import sigmastate.Values.ErgoTree
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.eval.RuntimeIRContext
import sigmastate.lang.SigmaCompiler
import sigmastate.{SBoolean, SSigmaProp}

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

case class WalletApiRoute(readersHolder: ActorRef, nodeViewActorRef: ActorRef, ergoSettings: ErgoSettings)
                         (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ApiCodecs {

  implicit val paymentRequestDecoder: PaymentRequestDecoder = new PaymentRequestDecoder(ergoSettings)
  implicit val assetIssueRequestDecoder: AssetIssueRequestDecoder = new AssetIssueRequestDecoder(ergoSettings)
  implicit val requestsHolderDecoder: RequestsHolderDecoder = new RequestsHolderDecoder(ergoSettings)
  implicit val addressEncoder: ErgoAddressEncoder = ErgoAddressEncoder(ergoSettings.chainSettings.addressPrefix)
  implicit val addressJsonEncoder: Encoder[ErgoAddress] = paymentRequestDecoder.addressEncoders.encoder

  val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  override val route: Route = (pathPrefix("wallet") & withAuth) {
    corsHandler {
      balancesR ~
        unconfirmedBalanceR ~
        addressesR ~
        generateTransactionR ~
        generatePaymentTransactionR ~
        generateAssetIssueTransactionR ~
        sendTransactionR ~
        sendPaymentTransactionR ~
        sendAssetIssueTransactionR ~
        p2shAddressR ~
        p2sAddressR
    }
  }

  private val loadMaxKeys: Int = 100

  private val fee: Directive1[Option[Long]] = entity(as[Json]).flatMap { p =>
    Try(p.hcursor.downField("fee").as[Long]) match {
      case Success(Right(value)) => provide(Some(value))
      case _ => provide(None)
    }
  }

  private val source: Directive1[String] = entity(as[Json]).flatMap { p =>
    p.hcursor.downField("source").as[String]
      .fold(_ => reject, s => provide(s))
  }

  private def addressResponse(address: ErgoAddress): Json = Json.obj("address" -> addressJsonEncoder(address))

  private def keysToEnv(keys: Seq[ProveDlog]): Map[String, Any] = {
    keys.zipWithIndex.map { case (pk, i) => s"myPubKey_$i" -> pk }.toMap
  }

  private def compileSource(source: String, env: Map[String, Any]): Try[ErgoTree] = {
    import sigmastate.Values._
    val compiler = SigmaCompiler(ergoSettings.chainSettings.addressPrefix)
    Try(compiler.compile(env, source)(new RuntimeIRContext)).flatMap {
      case script: Value[SSigmaProp.type@unchecked] if script.tpe == SSigmaProp =>
        Success(script)
      case script: Value[SBoolean.type@unchecked] if script.tpe == SBoolean =>
        Success(script.toSigmaProp)
      case other =>
        Failure(new Exception(s"Source compilation result is of type ${other.tpe}, but `SBoolean` expected"))
    }
  }

  private def withFee(requests: Seq[TransactionRequest], feeOpt: Option[Long]): Seq[TransactionRequest] = {
    requests :+ PaymentRequest(Pay2SAddress(ergoSettings.chainSettings.monetary.feeProposition),
      feeOpt.getOrElse(ergoSettings.walletSettings.defaultTransactionFee), None, None)
  }

  private def withWalletOp[T](op: ErgoWalletReader => Future[T])(toRoute: T => Route): Route = {
    onSuccess((readersHolder ? GetReaders).mapTo[Readers].flatMap(r => op(r.w)))(toRoute)
  }

  private def withWallet[T: Encoder](op: ErgoWalletReader => Future[T]): Route = {
    withWalletOp(op)(ApiResponse.apply[T])
  }

  private def generateTransaction(requests: Seq[TransactionRequest]): Route = {
    withWalletOp(_.generateTransaction(requests)) {
      case Failure(e) => BadRequest(s"Bad request $requests. ${Option(e.getMessage).getOrElse(e.toString)}")
      case Success(tx) => ApiResponse(tx)
    }
  }

  private def sendTransaction(requests: Seq[TransactionRequest]): Route = {
    withWalletOp(_.generateTransaction(requests)) {
      case Failure(e) => BadRequest(s"Bad request $requests. ${Option(e.getMessage).getOrElse(e.toString)}")
      case Success(tx) =>
        nodeViewActorRef ! LocallyGeneratedTransaction[ErgoTransaction](tx)
        ApiResponse(tx.id)
    }
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

  def unconfirmedBalanceR: Route = (path("balances" / "with_unconfirmed") & get) {
    withWallet(_.balancesWithUnconfirmed())
  }

  def p2sAddressR: Route = (path("p2s_address") & post & source) { source =>
    withWalletOp(_.publicKeys(0, loadMaxKeys)) { addrs =>
      compileSource(source, keysToEnv(addrs.map(_.pubkey))).map(Pay2SAddress.apply).fold(
        e => BadRequest(e.getMessage),
        address => ApiResponse(addressResponse(address))
      )
    }
  }

  def p2shAddressR: Route = (path("p2sh_address") & post & source) { source =>
    withWalletOp(_.publicKeys(0, loadMaxKeys)) { addrs =>
      compileSource(source, keysToEnv(addrs.map(_.pubkey))).map(Pay2SHAddress.apply).fold(
        e => BadRequest(e.getMessage),
        address => ApiResponse(addressResponse(address))
      )
    }
  }

  def addressesR: Route = (path("addresses") & get) {
    withWallet(_.trackedAddresses())
  }

}
