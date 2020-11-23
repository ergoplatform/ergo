package org.ergoplatform.nodeView.wallet

import akka.actor.{ActorRef, ActorSystem, Props}
import org.ergoplatform.GlobalConstants
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.nodeView.wallet.ErgoWalletActor._
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.wallet.boxes.ReplaceCompactCollectBoxSelector
import scorex.core.VersionTag
import scorex.core.transaction.wallet.Vault
import scorex.util.ScorexLogging

import scala.util.{Failure, Success, Try}

class ErgoWallet(historyReader: ErgoHistoryReader, settings: ErgoSettings)
                (implicit val actorSystem: ActorSystem)
  extends Vault[ErgoTransaction, ErgoPersistentModifier, ErgoWallet]
    with ErgoWalletReader
    with ScorexLogging {

  // A replace-compact-collect selector is parameterized with max number of inputs a transaction could has,
  // and also optimal number of inputs(a selector is collecting dust if transaction has less inputs than optimal).
  // Now these settings are hard-coded, however, they should be parameterized
  // https://github.com/ergoplatform/ergo/issues/856
  val maxInputs = 64
  val optimalInputs = 3

  val boxSelector = new ReplaceCompactCollectBoxSelector(maxInputs, optimalInputs)

  override type NVCT = this.type

  override val walletActor: ActorRef = {
    val props = Props(classOf[ErgoWalletActor], settings, boxSelector, historyReader)
                  .withDispatcher(GlobalConstants.ApiDispatcher)
    actorSystem.actorOf(props)
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
      case Some(_) =>
        walletActor ! Rollback(to)
        Success(this)
      case None if to == ErgoState.genesisStateVersion =>
        walletActor ! Rollback(to)
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
