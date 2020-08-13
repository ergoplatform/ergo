package org.ergoplatform.http.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.{Directive, Directive1, Route}
import akka.pattern.ask
import io.circe.syntax._
import io.circe.{Encoder, Json}
import org.ergoplatform._
import org.ergoplatform.modifiers.mempool.{ErgoBoxSerializer, ErgoTransaction}
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.state.UtxoStateReader
import org.ergoplatform.nodeView.wallet._
import org.ergoplatform.nodeView.wallet.requests._
import org.ergoplatform.settings.ErgoSettings
import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import scorex.core.api.http.ApiError.{BadRequest, NotExists}
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings
import scorex.util.encode.Base16

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

case class WalletApiRoute(readersHolder: ActorRef, nodeViewActorRef: ActorRef, ergoSettings: ErgoSettings)
                         (implicit val context: ActorRefFactory) extends WalletApiOperations with ApiCodecs {

  implicit val paymentRequestDecoder: PaymentRequestDecoder = new PaymentRequestDecoder(ergoSettings)
  implicit val assetIssueRequestDecoder: AssetIssueRequestDecoder = new AssetIssueRequestDecoder(ergoSettings)
  implicit val requestsHolderDecoder: RequestsHolderDecoder = new RequestsHolderDecoder(ergoSettings)
  implicit val addressEncoder: ErgoAddressEncoder = ErgoAddressEncoder(ergoSettings.chainSettings.addressPrefix)
  implicit val addressJsonEncoder: Encoder[ErgoAddress] = paymentRequestDecoder.addressEncoders.encoder
  implicit val walletTxEncoder: Encoder[AugWalletTransaction] = AugWalletTransaction.jsonEncoder

  val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  override val route: Route = (pathPrefix("wallet") & withAuth) {
    toStrictEntity(10.seconds) {
      getWalletStatusR ~
        balancesR ~
        unconfirmedBalanceR ~
        addressesR ~
        getTransactionR ~
        transactionsR ~
        unspentBoxesR ~
        boxesR ~
        generateTransactionR ~
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
        rescanWalletR
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

  private val restoreRequest: Directive1[(String, String, Option[String])] = entity(as[Json]).flatMap { p =>
    p.hcursor.downField("pass").as[String]
      .flatMap(pass => p.hcursor.downField("mnemonic").as[String]
        .flatMap(mnemo => p.hcursor.downField("mnemonicPass").as[Option[String]]
          .map(mnemoPassOpt => (pass, mnemo, mnemoPassOpt))
        )
      )
      .fold(_ => reject, s => provide(s))
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

  private def withFee(requests: Seq[TransactionGenerationRequest]): Seq[TransactionGenerationRequest] = {
    requests :+ PaymentRequest(Pay2SAddress(ergoSettings.chainSettings.monetary.feeProposition),
      ergoSettings.walletSettings.defaultTransactionFee, Seq.empty, Map.empty)
  }

  private def generateTransactionAndProcess(requests: Seq[TransactionGenerationRequest],
                                            inputsRaw: Seq[String],
                                            dataInputsRaw: Seq[String],
                                            processFn: ErgoTransaction => Route): Route = {
    withWalletOp(_.generateTransaction(requests, inputsRaw, dataInputsRaw)) {
      case Failure(e) => BadRequest(s"Bad request $requests. ${Option(e.getMessage).getOrElse(e.toString)}")
      case Success(tx) => processFn(tx)
    }
  }

  private def generateTransaction(requests: Seq[TransactionGenerationRequest],
                                  inputsRaw: Seq[String],
                                  dataInputsRaw: Seq[String]): Route = {
    generateTransactionAndProcess(requests, inputsRaw, dataInputsRaw, tx => ApiResponse(tx))
  }

  private def sendTransaction(requests: Seq[TransactionGenerationRequest],
                              inputsRaw: Seq[String],
                              dataInputsRaw: Seq[String]): Route = {
    generateTransactionAndProcess(requests, inputsRaw, dataInputsRaw, { tx =>
      nodeViewActorRef ! LocallyGeneratedTransaction[ErgoTransaction](tx)
      ApiResponse(tx.id)
    })
  }

  def sendTransactionR: Route =
    (path("transaction" / "send") & post & entity(as[RequestsHolder])) { holder =>
      sendTransaction(holder.withFee, holder.inputsRaw, holder.dataInputsRaw)
    }

  def generateTransactionR: Route =
    (path("transaction" / "generate") & post & entity(as[RequestsHolder])) { holder =>
      generateTransaction(holder.withFee, holder.inputsRaw, holder.dataInputsRaw)
    }

  def signTransactionR: Route = (path("transaction" / "sign")
    & post & entity(as[TransactionSigningRequest])) { tsr =>

    val tx = tsr.unsignedTx
    val secrets = (tsr.dlogs ++ tsr.dhts).map(ExternalSecret.apply)

    def signWithReaders(r: Readers): Future[Try[ErgoTransaction]] = {
      if (tsr.inputs.isDefined) {
        val boxesToSpend = tsr.inputs.get
          .flatMap(in => Base16.decode(in).flatMap(ErgoBoxSerializer.parseBytesTry).toOption)
        val dataBoxes = tsr.dataInputs.getOrElse(Seq.empty)
          .flatMap(in => Base16.decode(in).flatMap(ErgoBoxSerializer.parseBytesTry).toOption)

        if (boxesToSpend.size == tx.inputs.size && dataBoxes.size == tx.dataInputs.size) {
          r.w.signTransaction(secrets, tx, boxesToSpend, dataBoxes)
        } else {
          Future(Failure(new Exception("Can't parse input boxes provided")))
        }
      } else {
        r.s match {
          case utxoSet: UtxoStateReader =>
            val mempool = r.m
            val utxosWithUnconfirmed = utxoSet.withTransactions(mempool.getAll)
            val boxesToSpend = tx.inputs.map(d => utxosWithUnconfirmed.boxById(d.boxId).get)
            val dataBoxes = tx.dataInputs.map(d => utxosWithUnconfirmed.boxById(d.boxId).get)
            r.w.signTransaction(secrets, tx, boxesToSpend, dataBoxes)
          case _ => Future(Failure(new Exception("No input boxes provided, and no UTXO set to read them from")))
        }
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

  def balancesR: Route = (path("balances") & get) {
    withWallet(_.confirmedBalances)
  }

  def getWalletStatusR: Route = (path("status") & get) {
    withWalletOp(_.getWalletStatus) { walletStatus =>
      ApiResponse(
        Json.obj(
          "isInitialized" -> walletStatus.initialized.asJson,
          "isUnlocked" -> walletStatus.unlocked.asJson,
          "changeAddress" -> walletStatus.changeAddress.map(_.toString()).getOrElse("").asJson,
          "walletHeight" -> walletStatus.height.asJson
        )
      )
    }
  }

  def unconfirmedBalanceR: Route = (path("balances" / "withUnconfirmed") & get) {
    withWallet(_.balancesWithUnconfirmed)
  }

  def addressesR: Route = (path("addresses") & get) {
    withWallet(_.publicKeys(0, Int.MaxValue): Future[Seq[ErgoAddress]])
  }

  def unspentBoxesR: Route = (path("boxes" / "unspent") & get & boxParams) { (minConfNum, minHeight) =>
    withWallet {
      _.walletBoxes(unspentOnly = true)
        .map {
          _.filter(boxFilterPredicate(_, minConfNum, minHeight))
        }
    }
  }

  def boxesR: Route = (path("boxes") & get & boxParams) { (minConfNum, minHeight) =>
    withWallet {
      _.walletBoxes()
        .map {
          _.filter(boxFilterPredicate(_, minConfNum, minHeight))
        }
    }
  }

  def transactionsR: Route = (path("transactions") & get & txParams) {
    case (minHeight, maxHeight, minConfNum, maxConfNum) =>
      withWallet {
        _.transactions
          .map {
            _.filter(tx =>
              tx.wtx.inclusionHeight >= minHeight && tx.wtx.inclusionHeight <= maxHeight &&
                tx.numConfirmations >= minConfNum && tx.numConfirmations <= maxConfNum
            )
          }
      }
  }

  def getTransactionR: Route = (path("transactionById") & modifierIdGet & get) { id =>
    withWalletOp(_.transactionById(id)) {
      _.fold[Route](NotExists)(tx => ApiResponse(tx.asJson))
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

  def checkSeedR: Route = (path("check") & post & checkRequest) {
    case (mnemo, mnemoPassOpt) =>
      withWalletOp(_.checkSeed(mnemo, mnemoPassOpt)) { matched =>
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

  def updateChangeAddressR: Route = (path("updateChangeAddress") & post & p2pkAddress) { p2pk =>
    withWallet { w =>
      w.updateChangeAddress(p2pk)
      Future.successful(())
    }
  }

  def rescanWalletR: Route = (path("rescan") & get) {
    withWallet { w =>
      w.rescanWallet()
      Future.successful(())
    }
  }

}
