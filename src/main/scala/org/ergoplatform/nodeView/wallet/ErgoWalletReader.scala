package org.ergoplatform.nodeView.wallet

import java.util.concurrent.TimeUnit

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.wallet.ChainStatus.{Offchain, Onchain}
import org.ergoplatform.nodeView.wallet.ErgoWalletActor._
import scorex.core.transaction.wallet.VaultReader

import scala.concurrent.Future
import scala.util.Try


trait ErgoWalletReader extends VaultReader {
  val actor: ActorRef

  private implicit val timeout: Timeout = Timeout(5, TimeUnit.SECONDS)

  def balances(chainStatus: ChainStatus): Future[BalancesSnapshot] = {
    (actor ? ErgoWalletActor.ReadBalances(chainStatus)).mapTo[BalancesSnapshot]
  }

  def confirmedBalances(): Future[BalancesSnapshot] = balances(Onchain)

  def balancesWithUnconfirmed(): Future[BalancesSnapshot] = balances(Offchain)

  def publicKeys(from: Int, to: Int): Future[Seq[P2PKAddress]] = {
    (actor ? ReadPublicKeys(from, to)).mapTo[Seq[P2PKAddress]]
  }

  def randomPublicKey(): Future[P2PKAddress] = {
    (actor ? ReadRandomPublicKey).mapTo[P2PKAddress]
  }

  def trackedAddresses(): Future[Seq[ErgoAddress]] = {
    (actor ? ReadTrackedAddresses).mapTo[Seq[ErgoAddress]]
  }

  def generateTransaction(paymentRequests: Seq[PaymentRequest]): Future[Try[ErgoTransaction]] = {
    val boxCandidates = paymentRequests.map(_.toBoxCandidate)
    (actor ? GenerateTransaction(boxCandidates)).mapTo[Try[ErgoTransaction]]
  }
}
