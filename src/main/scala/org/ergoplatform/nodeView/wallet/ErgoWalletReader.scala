package org.ergoplatform.nodeView.wallet

import java.util.concurrent.TimeUnit
import akka.pattern.ask
import akka.actor.ActorRef
import akka.util.Timeout
import org.ergoplatform.ErgoBox.NonMandatoryRegisterId
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.wallet.ErgoWalletActor.GenerateTransaction
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate}
import scorex.core.transaction.wallet.VaultReader
import sigmastate.SType
import sigmastate.Values.EvaluatedValue
import scala.concurrent.Future



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

  def generateTransaction(paymentRequest: PaymentRequest): Future[Option[ErgoTransaction]] = {
    val boxCandidates = paymentRequest.to.map { t =>
      val script = t._1.script
      val value = t._2
      val assets = t._3.toSeq
      val regs: Map[NonMandatoryRegisterId, EvaluatedValue[_ <: SType]] = t._4.zipWithIndex.map { case (v, i) =>
        ErgoBox.nonMandatoryRegisters(i.toByte) -> v
      }.toMap
      new ErgoBoxCandidate(value, script, assets, regs)
    }

    (actor ? GenerateTransaction(boxCandidates)).mapTo[Option[ErgoTransaction]]
  }
}
