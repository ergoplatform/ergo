package org.ergoplatform.local

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import akka.util.Timeout
import io.circe.Encoder
import org.ergoplatform._
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.wallet.ErgoWalletReader
import org.ergoplatform.nodeView.wallet.requests._
import org.ergoplatform.settings.{Constants, ErgoSettings}
import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import scorex.core.settings.RESTApiSettings
import scorex.util.ScorexLogging

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/**
  * Actor that distributes all the coins kept in the wallet to config-defined addresses.
  */
class TxSender(readersHolderRef: ActorRef, nodeViewHolderRef: ActorRef, ergoSettings: ErgoSettings) extends Actor with ScorexLogging {

  implicit val paymentRequestDecoder: PaymentRequestDecoder = new PaymentRequestDecoder(ergoSettings)
  implicit val assetIssueRequestDecoder: AssetIssueRequestDecoder = new AssetIssueRequestDecoder(ergoSettings)
  implicit val requestsHolderDecoder: RequestsHolderDecoder = new RequestsHolderDecoder(ergoSettings)
  implicit val addressEncoder: ErgoAddressEncoder = ErgoAddressEncoder(ergoSettings.chainSettings.addressPrefix)
  implicit val addressJsonEncoder: Encoder[ErgoAddress] = paymentRequestDecoder.addressEncoders.encoder
  implicit val timeout: Timeout = new Timeout(60.seconds)

  val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi
  val fee = PaymentRequest(Pay2SAddress(ergoSettings.chainSettings.monetary.feeProposition), ergoSettings.walletSettings.defaultTransactionFee, Seq(), Map())
  lazy val withdrawDistribution: Map[P2PKAddress, Double] = ergoSettings
    .walletSettings
    .withdrawDistribution
    .map(a => addressEncoder.fromString(a._1).get.asInstanceOf[P2PKAddress] -> a._2)

  override def receive: Receive = {
    case GetReaders =>
      log.debug(s"Initialize TxSender with distribution ${withdrawDistribution.map(d => s"${d._1.toString()} -> ${d._2}")}")
      readersHolderRef ! GetReaders

    case Readers(h: ErgoHistoryReader, _, _, w: ErgoWalletReader) =>
      withdraw(h, w) match {
        case Success(_) =>
          context.system.scheduler.scheduleOnce(10.minutes)(readersHolderRef ! GetReaders)(context.system.dispatcher)
        case Failure(e) =>
          log.debug(s"Failed to generate transaction: ${e.getMessage}")
          context.system.scheduler.scheduleOnce(2.seconds)(readersHolderRef ! GetReaders)(context.system.dispatcher)
      }

    case m =>
      log.warn(s"Unhandled message $m")
  }

  /**
    * Will generate and broadcast transaction, that sends all coins in a wallet to addresses from withdrawDistribution.
    *
    * @return `Success` if transaction was sent,
    *         `Failure` otherwise
    */
  private def withdraw(hr: ErgoHistoryReader, wr: ErgoWalletReader): Try[Unit] = Try {
    val historyHeight = hr.headersHeight
    lazy val registry = Await.result(wr.confirmedBalances, timeout.duration)
    if (historyHeight != hr.fullBlockHeight || historyHeight < 24500) {
      throw new Exception("History is not synchronized yet")
    } else if (registry.height != historyHeight) {
      throw new Exception(s"Wallet is not synchronized yet. ${registry.height} != $historyHeight")
    } else {
      val unspentBoxes = Await.result(wr.boxes(true), timeout.duration)
      val balances = unspentBoxes.map(_.trackedBox.value).sortBy(f => -f)
      val balance = balances.sum
      // self-check assertion to catch https://github.com/ergoplatform/ergo/issues/853 bug. Remove after bug fix.
      if (balance != registry.balance) {
        val allBoxes = Await.result(wr.boxes(false), timeout.duration)
        throw new Exception(s"Balance from registry ${registry} does not match balance from boxes ${unspentBoxes}. All boxes are ${allBoxes}")
      }
      // If there is at least 1 ERG - send all coins (at most 10 boxes) to withdrawDistribution
      if (balance > Constants.CoinsInOneErgo) {
        val toSend = Math.min(balance - fee.value, balances.take(10).sum - fee.value)
        val payments = withdrawDistribution.map { a =>
          PaymentRequest(a._1, (toSend * a._2).toLong, Seq(), Map())
        }.toSeq
        val tx = Await.result(wr.generateTransaction(fee +: payments), timeout.duration).get
        log.info(s"Going to send $toSend ERG to $withdrawDistribution. Tx id ${tx.id}")
        nodeViewHolderRef ! LocallyGeneratedTransaction[ErgoTransaction](tx)
      } else {
        throw new Exception(s"No balance to send: $balance nanoErgs available.")
      }
    }
  }

}

object TxSenderRef {

  def props(readersHolderRef: ActorRef, nodeViewHolderRef: ActorRef, ergoSettings: ErgoSettings): Props =
    Props(new TxSender(readersHolderRef, nodeViewHolderRef: ActorRef, ergoSettings: ErgoSettings))

  def apply(readersHolderRef: ActorRef, nodeViewHolderRef: ActorRef, ergoSettings: ErgoSettings)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(readersHolderRef, nodeViewHolderRef: ActorRef, ergoSettings: ErgoSettings), "TxSender")

}
