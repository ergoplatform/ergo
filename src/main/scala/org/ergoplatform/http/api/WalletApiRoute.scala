package org.ergoplatform.http.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.{Directive, Directive1, Route, ValidationRejection}
import akka.pattern.ask
import io.circe.syntax._
import io.circe.{Encoder, Json}
import org.ergoplatform._
import org.ergoplatform.http.api.requests.HintExtractionRequest
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.wallet._
import org.ergoplatform.nodeView.wallet.requests._
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.wallet.interface4j.SecretString
import org.ergoplatform.wallet.Constants
import org.ergoplatform.wallet.Constants.ScanId
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer
import scorex.core.api.http.ApiError.{BadRequest, NotExists}
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings
import scorex.util.encode.Base16

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}
import akka.http.scaladsl.server.MissingQueryParamRejection

case class WalletApiRoute(readersHolder: ActorRef,
                          nodeViewActorRef: ActorRef,
                          ergoSettings: ErgoSettings)
                         (implicit val context: ActorRefFactory) extends WalletApiOperations with ApiCodecs {

  implicit val paymentRequestDecoder: PaymentRequestDecoder = new PaymentRequestDecoder(ergoSettings)
  implicit val assetIssueRequestDecoder: AssetIssueRequestDecoder = new AssetIssueRequestDecoder(ergoSettings)
  implicit val requestsHolderDecoder: RequestsHolderDecoder = new RequestsHolderDecoder(ergoSettings)
  implicit val addressEncoder: ErgoAddressEncoder = ErgoAddressEncoder(ergoSettings.chainSettings.addressPrefix)
  implicit val addressJsonEncoder: Encoder[ErgoAddress] = paymentRequestDecoder.addressEncoders.encoder
  implicit val walletTxEncoder: Encoder[AugWalletTransaction] = AugWalletTransaction.jsonEncoder

  val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  override val route: Route = (pathPrefix("wallet") & withAuth) {
    toStrictEntity(30.seconds) {
      getWalletStatusR ~
        balancesR ~
        unconfirmedBalanceR ~
        addressesR ~
        getTransactionR ~
        transactionsR ~
        unspentBoxesR ~
        boxesR ~
        collectBoxesR ~
        getTransactionsByScanIdR ~
        generateTransactionR ~
        generateUnsignedTransactionR ~
        generateCommitmentsR ~
        sendPaymentTransactionR ~
        sendTransactionR ~
        initWalletR ~
        restoreWalletR ~
        unlockWalletR ~
        lockWalletR ~
        deriveKeyR ~
        deriveNextKeyR ~
        updateChangeAddressR ~
        signTransactionR ~
        checkSeedR ~
        rescanWalletR ~
        extractHintsR ~
        getPrivateKeyR
    }
  }

  private val password: Directive1[String] = entity(as[Json]).flatMap { p =>
    p.hcursor.downField("pass").as[String]
      .fold(_ => reject, s => provide(s))
  }

  private val derivationPath: Directive1[String] = entity(as[Json]).flatMap { p =>
    p.hcursor.downField("derivationPath").as[String]
      .fold(_ => reject, s => provide(s))
  }

  private val restoreRequest: Directive1[(Boolean, String, String, Option[String])] = entity(as[Json]).flatMap { p =>
    p.hcursor.downField("pass").as[String]
      .flatMap(usePre1627KeyDerivation => p.hcursor.downField("usePre1627KeyDerivation").as[Boolean]
        .flatMap(pass => p.hcursor.downField("mnemonic").as[String]
          .flatMap(mnemo => p.hcursor.downField("mnemonicPass").as[Option[String]]
            .map(mnemoPassOpt => (pass, usePre1627KeyDerivation, mnemo, mnemoPassOpt))
          )
        )
      )
      .fold(e => reject(MissingQueryParamRejection(e.toString())), s => provide(s))
  }

  private val checkRequest: Directive1[(String, Option[String])] = entity(as[Json]).flatMap { p =>
    p.hcursor.downField("mnemonic").as[String]
      .flatMap(mnemo => p.hcursor.downField("mnemonicPass").as[Option[String]]
        .map(mnemoPassOpt => (mnemo, mnemoPassOpt)))
      .fold(_ => reject, s => provide(s))
  }

  private val initRequest: Directive1[(String, Option[String])] = entity(as[Json]).flatMap { p =>
    p.hcursor.downField("pass").as[String]
      .flatMap(pass => p.hcursor.downField("mnemonicPass").as[Option[String]]
        .map(mnemoPassOpt => (pass, mnemoPassOpt))
      )
      .fold(_ => reject, s => provide(s))
  }

  private val txParams: Directive[(Int, Int, Int, Int)] = parameters(
    "minInclusionHeight".as[Int] ? 0,
    "maxInclusionHeight".as[Int] ? Int.MaxValue,
    "minConfirmations".as[Int] ? 0,
    "maxConfirmations".as[Int] ? Int.MaxValue
  )

  private val txsByScanIdParams: Directive[(Int, Int, Int, Int, Boolean)] = parameters(
    "minInclusionHeight".as[Int] ? 0,
    "maxInclusionHeight".as[Int] ? Int.MaxValue,
    "minConfirmations".as[Int] ? 0,
    "maxConfirmations".as[Int] ? Int.MaxValue,
    "includeUnconfirmed".as[Boolean] ? false
  )


  private val p2pkAddress: Directive1[P2PKAddress] = entity(as[Json])
    .flatMap {
      _.hcursor.downField("address").as[String]
        .flatMap { address =>
          addressEncoder.fromString(address).toEither
        } match {
        case Right(value: P2PKAddress) => provide(value)
        case _ => reject
      }
    }

  /** POST body field - from what height to rescan wallet */
  private val heightEntityField: Directive1[Int] = entity(as[Option[Json]])
    .flatMap {
      _.map[Directive1[Int]] { entity =>
        entity.hcursor.downField("fromHeight").as[Int] match {
          case Right(fromHeight) if fromHeight >= 0 =>
            provide(fromHeight)
          case Right(_) =>
            reject(ValidationRejection("fromHeight field must be >= 0"))
          case Left(_) =>
            reject
        }
      }.getOrElse(provide(0))
    }

  private def withFee(requests: Seq[TransactionGenerationRequest]): Seq[TransactionGenerationRequest] = {
    requests :+ PaymentRequest(Pay2SAddress(ergoSettings.chainSettings.monetary.feeProposition),
      ergoSettings.walletSettings.defaultTransactionFee, Seq.empty, Map.empty)
  }

  private def generateTransactionAndProcess(requests: Seq[TransactionGenerationRequest],
                                            inputsRaw: Seq[String],
                                            dataInputsRaw: Seq[String],
                                            verifyFn: ErgoTransaction => Future[Try[UnconfirmedTransaction]],
                                            processFn: UnconfirmedTransaction => Route): Route = {
    withWalletOp(_.generateTransaction(requests, inputsRaw, dataInputsRaw).flatMap(txTry => txTry match {
      case Success(tx) => verifyFn(tx)
      case Failure(e) => Future(Failure[UnconfirmedTransaction](e))
    })) {
      case Failure(e) => BadRequest(s"Bad request $requests. ${Option(e.getMessage).getOrElse(e.toString)}")
      case Success(tx) => processFn(tx)
    }
  }

  private def generateTransaction(requests: Seq[TransactionGenerationRequest],
                                  inputsRaw: Seq[String],
                                  dataInputsRaw: Seq[String]): Route = {
    generateTransactionAndProcess(
      requests,
      inputsRaw,
      dataInputsRaw,
      tx => Future(Success(UnconfirmedTransaction(tx, source = None))),
      utx => ApiResponse(utx.transaction)
    )
  }

  private def generateUnsignedTransaction(requests: Seq[TransactionGenerationRequest],
                                          inputsRaw: Seq[String],
                                          dataInputsRaw: Seq[String]): Route = {
    withWalletOp(_.generateUnsignedTransaction(requests, inputsRaw, dataInputsRaw)) {
      case Failure(e) => BadRequest(s"Bad request $requests. ${Option(e.getMessage).getOrElse(e.toString)}")
      case Success(utx) => ApiResponse(utx)
    }
  }

  private def sendTransaction(requests: Seq[TransactionGenerationRequest],
                              inputsRaw: Seq[String],
                              dataInputsRaw: Seq[String]): Route = {
    generateTransactionAndProcess(requests, inputsRaw, dataInputsRaw,
      tx => verifyTransaction(tx, readersHolder, ergoSettings),
      validTx => sendLocalTransactionRoute(nodeViewActorRef, validTx)
    )
  }

  def sendTransactionR: Route =
    (path("transaction" / "send") & post & entity(as[RequestsHolder])) { holder =>
      sendTransaction(holder.withFee(), holder.inputsRaw, holder.dataInputsRaw)
    }

  def generateTransactionR: Route =
    (path("transaction" / "generate") & post & entity(as[RequestsHolder])) { holder =>
      generateTransaction(holder.withFee(), holder.inputsRaw, holder.dataInputsRaw)
    }

  def generateUnsignedTransactionR: Route =
    (path("transaction" / "generateUnsigned") & post & entity(as[RequestsHolder])) { holder =>
      generateUnsignedTransaction(holder.withFee(), holder.inputsRaw, holder.dataInputsRaw)
    }

  def generateCommitmentsR: Route = (path("generateCommitments")
    & post & entity(as[GenerateCommitmentsRequest])) { gcr =>

    val utx = gcr.unsignedTx
    val externalSecretsOpt = gcr.externalSecretsOpt
    val extInputsOpt = gcr.inputs.map(ErgoWalletService.stringsToBoxes)
    val extDataInputsOpt = gcr.dataInputs.map(ErgoWalletService.stringsToBoxes)

    withWalletOp(_.generateCommitmentsFor(utx, externalSecretsOpt, extInputsOpt, extDataInputsOpt).map(_.response)) {
      case Failure(e) => BadRequest(s"Bad request $gcr. ${Option(e.getMessage).getOrElse(e.toString)}")
      case Success(thb) => ApiResponse(thb)
    }
  }

  def signTransactionR: Route = (path("transaction" / "sign")
    & post & entity(as[TransactionSigningRequest])) { tsr =>

    val tx = tsr.unsignedTx
    val secrets = tsr.externalSecrets

    val hints = tsr.hints

    def signWithReaders(r: Readers): Future[Try[ErgoTransaction]] = {
      if (tsr.inputs.isDefined) {
        val boxesToSpend = tsr.inputs.get
          .flatMap(in => Base16.decode(in).flatMap(ErgoBoxSerializer.parseBytesTry).toOption)
        val dataBoxes = tsr.dataInputs.getOrElse(Seq.empty)
          .flatMap(in => Base16.decode(in).flatMap(ErgoBoxSerializer.parseBytesTry).toOption)

        if (boxesToSpend.size == tx.inputs.size && dataBoxes.size == tx.dataInputs.size) {
          r.w.signTransaction(tx, secrets, hints, Some(boxesToSpend), Some(dataBoxes))
        } else {
          Future(Failure(new Exception("Can't parse input boxes provided")))
        }
      } else {
        r.w.signTransaction(tx, secrets, hints, None, None)
      }
    }

    onSuccess {
      (readersHolder ? GetReaders)
        .mapTo[Readers]
        .flatMap(r => signWithReaders(r))
    } {
      _.fold(
        e => BadRequest(s"Malformed request: ${e.getMessage}"),
        tx => ApiResponse(tx.asJson)
      )
    }
  }

  def sendPaymentTransactionR: Route = (path("payment" / "send") & post
    & entity(as[Seq[PaymentRequest]])) { requests =>
    sendTransaction(withFee(requests), Seq.empty, Seq.empty)
  }

  def collectBoxesR: Route = (path("boxes" / "collect") & post
    & entity(as[BoxesRequest])) { request =>
    withWalletOp(_.collectBoxes(request)) {
      _.result.fold(
        e => NotExists(s"No wallet boxes found due to ${e.getMessage}"),
        boxes => ApiResponse(boxes.asJson)
      )
    }
  }

  def balancesR: Route = (path("balances") & get) {
    withWallet(_.confirmedBalances)
  }

  def getWalletStatusR: Route = (path("status") & get) {
    withWallet(_.getWalletStatus) { walletStatus =>
        Json.obj(
          "isInitialized" -> walletStatus.initialized.asJson,
          "isUnlocked" -> walletStatus.unlocked.asJson,
          "changeAddress" -> walletStatus.changeAddress.map(_.toString()).getOrElse("").asJson,
          "walletHeight" -> walletStatus.height.asJson,
          "error" -> walletStatus.error.getOrElse("").asJson
        )
    }
  }

  def unconfirmedBalanceR: Route = (path("balances" / "withUnconfirmed") & get) {
    withWallet(_.balancesWithUnconfirmed)
  }

  def addressesR: Route = (path("addresses") & get) {
    withWallet(_.publicKeys(0, Int.MaxValue): Future[Seq[ErgoAddress]])
  }

  def unspentBoxesR: Route = (path("boxes" / "unspent") & get & boxParams) {
    (minConfNum, maxConfNum, minHeight, maxHeight, limit, offset) =>
      val considerUnconfirmed = minConfNum == -1
      withWallet { wallet =>
        wallet.walletBoxes(unspentOnly = true, considerUnconfirmed)
          .map { boxes =>
            boxes
              .filter(boxConfirmationHeightFilter(_, minConfNum, maxConfNum, minHeight, maxHeight))
              .slice(offset, offset + limit)
          }
      }
  }

  def boxesR: Route = (path("boxes") & get & boxParams) {
    (minConfNum, maxConfNum, minHeight, maxHeight, limit, offset)  =>
      val considerUnconfirmed = minConfNum == -1
      withWallet {
        _.walletBoxes(unspentOnly = false, considerUnconfirmed = considerUnconfirmed)
          .map {
            _.filter(boxConfirmationHeightFilter(_, minConfNum, maxConfNum, minHeight, maxHeight))
            .slice(offset, offset + limit)
          }
      }
  }

  def transactionsR: Route = (path("transactions") & get & txParams) {
    case (minHeight, maxHeight, minConfNum, maxConfNum) =>
      if ((minHeight > 0 || maxHeight < Int.MaxValue) && // height is set
        (minConfNum > 0 || maxConfNum < Int.MaxValue) // confirmations are set
      ) {
        BadRequest("Bad request: both heights and confirmations set")
      }
      else if (minHeight == 0 && maxHeight == Int.MaxValue && minConfNum == 0 && maxConfNum == Int.MaxValue) {
        withWallet {
          _.transactions
            .map {
              _.filter(tx =>
                tx.wtx.scanIds.exists(scanId => scanId <= Constants.PaymentsScanId)
              )
            }
        }
      } else {
        withWallet {
          _.filteredScanTransactions(
            List(Constants.PaymentsScanId, Constants.MiningScanId),
            minHeight,
            maxHeight,
            minConfNum,
            maxConfNum,
            includeUnconfirmed = false
          )
        }
      }
  }

  def getTransactionR: Route = (path("transactionById") & modifierIdGet & get) { id =>
    withWalletOp(_.transactionById(id)) {
      _.fold[Route](NotExists)(tx => ApiResponse(tx.asJson))
    }
  }

  def getTransactionsByScanIdR: Route = (path("transactionsByScanId" / Segment) & get & txsByScanIdParams) {
    case (id, minHeight, maxHeight, minConfNum, maxConfNum, includeUnconfirmed) =>
      if ((minHeight > 0 || maxHeight < Int.MaxValue) && (minConfNum > 0 || maxConfNum < Int.MaxValue))
        BadRequest("Bad request: both heights and confirmations set")
      else if (minHeight == 0 && maxHeight == Int.MaxValue && minConfNum == 0 && maxConfNum == Int.MaxValue) {
        withWalletOp(_.transactionsByScanId(ScanId @@ id.toShort, includeUnconfirmed)) {
          resp => ApiResponse(resp.result.asJson)
        }
      }
      else {
        withWalletOp(_.filteredScanTransactions(
          List(ScanId @@ id.toShort),
          minHeight,
          maxHeight,
          minConfNum,
          maxConfNum,
          includeUnconfirmed)
        ) {
          resp => ApiResponse(resp.asJson)
        }
      }
  }

  def initWalletR: Route = (path("init") & post & initRequest) {
    case (pass, mnemonicPassOpt) =>
      withWalletOp(_.initWallet(SecretString.create(pass), mnemonicPassOpt.map(SecretString.create(_)))) {
        _.fold(
          e => BadRequest(e.getMessage),
          mnemonic => {
            val responseJson = Json.obj("mnemonic" -> mnemonic.asJson)
            mnemonic.erase()
            ApiResponse(responseJson)
          }
        )
      }
  }

  def restoreWalletR: Route = (path("restore") & post & restoreRequest) {
    case (usePre1627KeyDerivation, pass, mnemo, mnemoPassOpt) =>
      withWalletOp(_.restoreWallet(SecretString.create(pass), SecretString.create(mnemo), mnemoPassOpt.map(SecretString.create(_)), usePre1627KeyDerivation)) {
        _.fold(
          e => BadRequest(e.getMessage),
          _ => ApiResponse.toRoute(ApiResponse.OK)
        )
      }
  }

  def unlockWalletR: Route = (path("unlock") & post & password) { pass =>
    withWalletOp(_.unlockWallet(SecretString.create(pass))) {
      _.fold(
        e => BadRequest(e.getMessage),
        _ => ApiResponse.toRoute(ApiResponse.OK)
      )
    }
  }

  def checkSeedR: Route = (path("check") & post & checkRequest) {
    case (mnemo, mnemoPassOpt) =>
      withWalletOp(_.checkSeed(SecretString.create(mnemo), mnemoPassOpt.map(SecretString.create(_)))) { matched =>
        ApiResponse(
          Json.obj(
            "matched" -> matched.asJson
          )
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
      _.result.fold(
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

  def updateChangeAddressR: Route = (path("updateChangeAddress") & post & p2pkAddress) { p2pk =>
    withWallet { w =>
      w.updateChangeAddress(p2pk)
    }
  }

  def rescanWalletR: Route = (path("rescan") & post & heightEntityField) { fromHeight =>
    withWalletOp(_.rescanWallet(fromHeight)) {
      _.fold(
        e => BadRequest(e.getMessage),
        _ => ApiResponse.toRoute(ApiResponse.OK)
      )
    }
  }

  def extractHintsR: Route = (path("extractHints") & post & entity(as[HintExtractionRequest])) { her =>
    withWallet { w =>
      val extInputsOpt = her.inputs.map(ErgoWalletService.stringsToBoxes)
      val extDataInputsOpt = her.dataInputs.map(ErgoWalletService.stringsToBoxes)

      w.extractHints(her.tx, her.real, her.simulated, extInputsOpt, extDataInputsOpt).map(_.transactionHintsBag)
    }
  }

  def getPrivateKeyR: Route = (path("getPrivateKey") & post & p2pkAddress) { p2pk =>
    withWalletOp(_.allExtendedPublicKeys()) { extKeys =>
      extKeys.find(_.key.value.equals(p2pk.pubkey.value)).map(_.path) match {
        case Some(path) =>
          withWalletOp(_.getPrivateKeyFromPath(path)) {
            case Success(secret) => ApiResponse(secret.w)
            case Failure(f) => BadRequest(f.getMessage)
          }
        case None => NotExists("Address not found in wallet database.")
      }
    }
  }

}
