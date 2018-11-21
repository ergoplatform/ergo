package org.ergoplatform.nodeView.wallet

import java.util.concurrent.TimeUnit

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.wallet.ChainStatus.{Offchain, Onchain}
import org.ergoplatform.nodeView.wallet.ErgoWalletActor._
import org.ergoplatform.nodeView.wallet.requests.TransactionRequest
import org.ergoplatform.{ErgoAddress, ErgoBox, P2PKAddress}
import scapi.sigma.DLogProtocol.DLogProverInput
import scorex.core.transaction.wallet.VaultReader

import scala.concurrent.Future
import scala.util.Try

trait ErgoWalletReader extends VaultReader {
  val actor: ActorRef

  private implicit val timeout: Timeout = Timeout(60, TimeUnit.SECONDS)

  def balances(chainStatus: ChainStatus): Future[BalancesSnapshot] = {
    (actor ? ErgoWalletActor.ReadBalances(chainStatus)).mapTo[BalancesSnapshot]
  }

  def confirmedBalances(): Future[BalancesSnapshot] = balances(Onchain)

  def balancesWithUnconfirmed(): Future[BalancesSnapshot] = balances(Offchain)

  def publicKeys(from: Int, to: Int): Future[Seq[P2PKAddress]] = {
    (actor ? ReadPublicKeys(from, to)).mapTo[Seq[P2PKAddress]]
  }

  def firstSecret(): Future[DLogProverInput] = {
    (actor ? GetFirstSecret).mapTo[DLogProverInput]
  }

  def unspendBoxes(): Future[Iterator[ErgoBox]] = {
    (actor ? GetBoxes).mapTo[Iterator[ErgoBox]]
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
