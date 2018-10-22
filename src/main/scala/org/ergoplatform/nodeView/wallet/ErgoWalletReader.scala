package org.ergoplatform.nodeView.wallet

import java.util.concurrent.TimeUnit

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import org.ergoplatform.autoleakus.PrivateKey
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.wallet.ErgoWalletActor._
import org.ergoplatform.nodeView.wallet.requests.TransactionRequest
import scapi.sigma.DLogProtocol.DLogProverInput
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

  def publicKeys(from: Int, to: Int): Future[Seq[P2PKAddress]] = {
    (actor ? ReadPublicKeys(from, to)).mapTo[Seq[P2PKAddress]]
  }

  def firstSecret(): Future[DLogProverInput] = {
    (actor ? GetFirstSecret).mapTo[DLogProverInput]
  }

  def randomPublicKey(): Future[P2PKAddress] = {
    (actor ? ReadRandomPublicKey).mapTo[P2PKAddress]
  }

  def trackedAddresses(): Future[Seq[ErgoAddress]] = {
    (actor ? ReadTrackedAddresses).mapTo[Seq[ErgoAddress]]
  }

  def generateTransaction(requests: Seq[TransactionRequest]): Future[Try[ErgoTransaction]] = {
    (actor ? GenerateTransaction(requests)).mapTo[Try[ErgoTransaction]]
  }
}
