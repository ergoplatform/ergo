package org.ergoplatform.nodeView.wallet

import akka.actor.{ActorRef, ActorSystem, Props}
import org.ergoplatform.ErgoAddress
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.nodeView.wallet.ErgoWalletActor._
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.wallet.boxes.DefaultBoxSelector
import scorex.core.VersionTag
import scorex.core.transaction.wallet.Vault
import scorex.util.ScorexLogging

import scala.util.{Failure, Success, Try}

class ErgoWallet(historyReader: ErgoHistoryReader, settings: ErgoSettings)
                (implicit val actorSystem: ActorSystem)
  extends Vault[ErgoTransaction, ErgoPersistentModifier, ErgoWallet]
    with ErgoWalletReader
    with ScorexLogging {

  override type NVCT = this.type

  override val walletActor: ActorRef =
    actorSystem.actorOf(Props(classOf[ErgoWalletActor], settings, DefaultBoxSelector))

  def watchFor(address: ErgoAddress): ErgoWallet = {
    walletActor ! WatchFor(address)
    this
  }

  override def scanOffchain(tx: ErgoTransaction): ErgoWallet = {
    walletActor ! ScanOffChain(tx)
    this
  }

  override def scanOffchain(txs: Seq[ErgoTransaction]): ErgoWallet = {
    txs.foreach(tx => scanOffchain(tx))
    this
  }

  override def scanPersistent(modifier: ErgoPersistentModifier): ErgoWallet = {
    modifier match {
      case fb: ErgoFullBlock =>
        walletActor ! ScanOnChain(fb)
      case _ =>
        log.debug("Not full block in ErgoWallet.scanPersistent, which could be the case only if " +
          "state = digest when bootstrapping")
    }
    this
  }

  override def rollback(to: VersionTag): Try[ErgoWallet] =
    historyReader.heightOf(scorex.core.versionToId(to)) match {
      case Some(height) =>
        walletActor ! Rollback(to, height)
        Success(this)
      case None if to == ErgoState.genesisStateVersion =>
        walletActor ! Rollback(to, ErgoHistory.GenesisHeight)
        Success(this)
      case None =>
        Failure(new Exception(s"Height of a modifier with id $to not found"))
    }
}

object ErgoWallet {
  def readOrGenerate(historyReader: ErgoHistoryReader,
                     settings: ErgoSettings)(implicit actorSystem: ActorSystem): ErgoWallet = {
    new ErgoWallet(historyReader, settings)
  }
}
