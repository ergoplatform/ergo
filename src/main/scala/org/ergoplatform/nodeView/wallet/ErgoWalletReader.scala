package org.ergoplatform.nodeView.wallet

import java.util.concurrent.TimeUnit

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.wallet.ErgoWalletActor.GenerateTransaction
import scorex.core.transaction.wallet.VaultReader

import scala.concurrent.Future
import scala.util.Try



trait ErgoWalletReader extends VaultReader {
  val actor: ActorRef

  private implicit val timeout: Timeout = Timeout(5, TimeUnit.SECONDS)

  def balances(confirmed: Boolean): Future[BalancesSnapshot] = {
    (actor ? ErgoWalletActor.ReadBalances(confirmed)).mapTo[BalancesSnapshot]
  }

  def confirmedBalances(): Future[BalancesSnapshot] = balances(confirmed = true)

  def unconfirmedBalances(): Future[BalancesSnapshot] = balances(confirmed = false)

  def walletAddresses(): Future[Seq[ErgoAddress]] = {
    (actor ? ErgoWalletActor.ReadWalletAddresses).mapTo[Seq[ErgoAddress]]
  }

  def generateTransaction(paymentRequests: Seq[PaymentRequest]): Future[Try[ErgoTransaction]] = {
    val boxCandidates = paymentRequests.map(_.toBoxCandidate)
    (actor ? GenerateTransaction(boxCandidates)).mapTo[Try[ErgoTransaction]]
  }
}
