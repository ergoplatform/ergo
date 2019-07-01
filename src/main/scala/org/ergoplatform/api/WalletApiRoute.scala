package org.ergoplatform.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.{Directive, Directive1, Route}
import akka.pattern.ask
import io.circe.syntax._
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
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

case class WalletApiRoute(readersHolder: ActorRef, nodeViewActorRef: ActorRef, ergoSettings: ErgoSettings)
                         (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ApiCodecs {

  implicit val paymentRequestDecoder: PaymentRequestDecoder = new PaymentRequestDecoder(ergoSettings)
  implicit val assetIssueRequestDecoder: AssetIssueRequestDecoder = new AssetIssueRequestDecoder(ergoSettings)
  implicit val requestsHolderDecoder: RequestsHolderDecoder = new RequestsHolderDecoder(ergoSettings)
  implicit val addressEncoder: ErgoAddressEncoder = ErgoAddressEncoder(ergoSettings.chainSettings.addressPrefix)
  implicit val addressJsonEncoder: Encoder[ErgoAddress] = paymentRequestDecoder.addressEncoders.encoder
  implicit val walletTxEncoder: Encoder[WalletTransaction] = WalletTransaction.jsonEncoder

  val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  override val route: Route = (pathPrefix("wallet") & withAuth) {
    toStrictEntity(10.seconds) {
      corsHandler {
        balancesR ~
          unconfirmedBalanceR ~
          addressesR ~
          transactionR ~
          unspentBoxesR ~
          boxesR ~
          generateTransactionR ~
          generatePaymentTransactionR ~
          generateAssetIssueTransactionR ~
          sendTransactionR ~
          sendPaymentTransactionR ~
          sendAssetIssueTransactionR ~
          p2shAddressR ~
          p2sAddressR ~
          initWalletR ~
          restoreWalletR ~
          unlockWalletR ~
          lockWalletR ~
          deriveKeyR ~
          deriveNextKeyR
      }
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

  private val password: Directive1[String] = entity(as[Json]).flatMap { p =>
    p.hcursor.downField("pass").as[String]
      .fold(_ => reject, s => provide(s))
  }

  private val derivationPath: Directive1[String] = entity(as[Json]).flatMap { p =>
    p.hcursor.downField("derivationPath").as[String]
      .fold(_ => reject, s => provide(s))
  }

  private val restoreRequest: Directive1[(String, String, Option[String])] = entity(as[Json]).flatMap { p =>
    p.hcursor.downField("pass").as[String]
      .flatMap(pass => p.hcursor.downField("mnemonic").as[String]
        .flatMap(mnemo => p.hcursor.downField("mnemonicPass").as[Option[String]]
          .map(mnemoPassOpt => (pass, mnemo, mnemoPassOpt))
        )
      )
      .fold(_ => reject, s => provide(s))
  }

  private val initRequest: Directive1[(String, Option[String])] = entity(as[Json]).flatMap { p =>
    p.hcursor.downField("pass").as[String]
      .flatMap(pass => p.hcursor.downField("mnemonicPass").as[Option[String]]
        .map(mnemoPassOpt => (pass, mnemoPassOpt))
      )
      .fold(_ => reject, s => provide(s))
  }

  private val txParams: Directive[(Int, Int)] =
    parameters("minInclusionHeight".as[Int] ? 0, "maxInclusionHeight".as[Int] ? Int.MaxValue)

  private val boxParams: Directive[(Int, Int)] =
    parameters("minConfirmations".as[Int] ? 0, "minInclusionHeight".as[Int] ? 0)

  private val boxPredicate = { (bx: WalletBox, minConfNum: Int, minHeight: Int) =>
    bx.confirmationsNumOpt.getOrElse(0) >= minConfNum &&
      bx.trackedBox.inclusionHeightOpt.getOrElse(-1) >= minHeight
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
    withWallet(_.confirmedBalances)
  }

  def unconfirmedBalanceR: Route = (path("balances" / "with_unconfirmed") & get) {
    withWallet(_.balancesWithUnconfirmed)
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
    withWallet(_.trackedAddresses)
  }

  def unspentBoxesR: Route = (path("boxes" / "unspent") & get & boxParams) { (minConfNum, minHeight) =>
    withWallet {
      _.boxes(unspentOnly = true)
        .map {
          _.filter(boxPredicate(_, minConfNum, minHeight))
        }
    }
  }

  def boxesR: Route = (path("boxes") & get & boxParams) { (minConfNum, minHeight) =>
    withWallet {
      _.boxes()
        .map {
          _.filter(boxPredicate(_, minConfNum, minHeight))
        }
    }
  }

  def transactionR: Route = (path("transactions") & get & txParams) { case (minHeight, maxHeight) =>
    withWallet {
      _.transactions
        .map {
          _.filter(tx => tx.inclusionHeight >= minHeight && tx.inclusionHeight <= maxHeight)
        }
    }
  }

  def initWalletR: Route = (path("init") & post & initRequest) {
    case (pass, mnemonicPassOpt) =>
      withWalletOp(_.initWallet(pass, mnemonicPassOpt)) {
        _.fold(
          e => BadRequest(e.getMessage),
          mnemonic => ApiResponse(Json.obj("mnemonic" -> mnemonic.asJson))
        )
      }
  }

  def restoreWalletR: Route = (path("restore") & post & restoreRequest) {
    case (pass, mnemo, mnemoPassOpt) =>
      withWalletOp(_.restoreWallet(pass, mnemo, mnemoPassOpt)) {
        _.fold(
          e => BadRequest(e.getMessage),
          _ => ApiResponse.toRoute(ApiResponse.OK)
        )
      }
  }

  def unlockWalletR: Route = (path("unlock") & post & password) { pass =>
    withWalletOp(_.unlockWallet(pass)) {
      _.fold(
        e => BadRequest(e.getMessage),
        _ => ApiResponse.toRoute(ApiResponse.OK)
      )
    }
  }

  def lockWalletR: Route = (path("lock") & get) {
    withWallet { w =>
      w.lockWallet()
      Future.successful(())
    }
  }

  def deriveKeyR: Route = (path("deriveKey") & post & derivationPath) { path =>
    withWalletOp(_.deriveKey(path)) {
      _.fold(
        e => BadRequest(e.getMessage),
        address => ApiResponse(Json.obj("address" -> address.toString.asJson))
      )
    }
  }

  def deriveNextKeyR: Route = (path("deriveNextKey") & get) {
    withWalletOp(_.deriveNextKey) {
      _.fold(
        e => BadRequest(e.getMessage),
        x => ApiResponse(
          Json.obj(
            "derivationPath" -> x._1.encoded.asJson,
            "address" -> x._2.toString.asJson
          )
        )
      )
    }
  }

}
