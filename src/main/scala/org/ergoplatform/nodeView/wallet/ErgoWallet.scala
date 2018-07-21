package org.ergoplatform.nodeView.wallet

import java.math.BigInteger
import java.util.concurrent.TimeUnit

import akka.actor.{ActorRef, ActorSystem, Props}
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.wallet.ErgoWalletActor.{Rollback, ScanOffchain, ScanOnchain}
import org.ergoplatform.settings.ErgoSettings
import scorex.core.{ModifierId, VersionTag}
import scorex.core.transaction.wallet.{Vault, VaultReader}
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base16
import scorex.crypto.hash.Blake2b256

import scala.util.{Success, Try}
import akka.pattern.ask
import akka.util.Timeout
import org.ergoplatform.local.TransactionGenerator.StartGeneration
import org.ergoplatform.local.TransactionGeneratorRef
import org.ergoplatform.nodeView.history.ErgoHistoryReader

import scala.concurrent.Future


trait ErgoWalletReader extends VaultReader {
  val actor: ActorRef

  def confirmedBalances(): Future[BalancesSnapshot] = {
    implicit val timeout = Timeout(5, TimeUnit.SECONDS)
    (actor ? ErgoWalletActor.ReadBalances(confirmed = true)).mapTo[BalancesSnapshot]
  }

  def unconfirmedBalances(): Future[BalancesSnapshot] = {
    implicit val timeout = Timeout(5, TimeUnit.SECONDS)
    (actor ? ErgoWalletActor.ReadBalances(confirmed = false)).mapTo[BalancesSnapshot]
  }
}

class ErgoWallet(actorSystem: ActorSystem,
                 nodeViewHolderRef: ActorRef,
                 historyReader: ErgoHistoryReader,
                 settings: ErgoSettings)
  extends Vault[ErgoTransaction, ErgoPersistentModifier, ErgoWallet] with ErgoWalletReader with ScorexLogging {

  private lazy val seed = settings.walletSettings.seed

  override lazy val actor: ActorRef = actorSystem.actorOf(Props(classOf[ErgoWalletActor], seed))

  implicit val system = actorSystem

  if (settings.testingSettings.transactionGeneration) {
    val txGen = TransactionGeneratorRef(nodeViewHolderRef, actor, settings.testingSettings)
    txGen ! StartGeneration
  }

  override def scanOffchain(tx: ErgoTransaction): ErgoWallet = {
    actor ! ScanOffchain(tx)
    this
  }

  override def scanOffchain(txs: Seq[ErgoTransaction]): ErgoWallet = {
    txs.foreach(tx => scanOffchain(tx))
    this
  }

  override def scanPersistent(modifier: ErgoPersistentModifier): ErgoWallet = {
    modifier match {
      case fb: ErgoFullBlock =>
        actor ! ScanOnchain(fb)
      case _ =>
        log.warn("Only full block is expected in ErgoWallet.scanPersistent")
    }
    this
  }

  override def rollback(to: VersionTag): Try[ErgoWallet] = {
    val height = historyReader.heightOf(ModifierId @@ to).get //todo: .get
    actor ! Rollback(height)
    Success(this)
  }

  override type NVCT = this.type
}


object ErgoWallet {

  def readOrGenerate(actorSystem: ActorSystem,
                     nodeViewHolderRef: ActorRef,
                     historyReader: ErgoHistoryReader,
                     settings: ErgoSettings): ErgoWallet = {
    new ErgoWallet(actorSystem, nodeViewHolderRef, historyReader, settings)
  }

  def secretsFromSeed(seedStr: String): IndexedSeq[BigInteger] = {
    val seed = Base16.decode(seedStr).get
    (1 to 4).map { i =>
      BigIntegers.fromUnsignedByteArray(Blake2b256.hash(i.toByte +: seed))
    }
  }
}