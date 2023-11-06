package org.ergoplatform.nodeView.wallet

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock}
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.history.UtxoSnapshotScanner.StartUtxoSetSnapshotScan
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.nodeView.wallet.ErgoWalletActor._
import org.ergoplatform.settings.{ErgoSettings, Parameters}
import org.ergoplatform.wallet.boxes.{ReemissionData, ReplaceCompactCollectBoxSelector}
import scorex.core.VersionTag
import scorex.util.ScorexLogging

import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

class ErgoWallet(historyReader: ErgoHistoryReader, settings: ErgoSettings, parameters: Parameters)
                (implicit val actorSystem: ActorSystem)
  extends ErgoWalletReader with ScorexLogging {

  private val walletSettings = settings.walletSettings

  // A replace-compact-collect selector is parameterized with max number of inputs a transaction could has,
  // and also optimal number of inputs(a selector is collecting dust if transaction has less inputs than optimal).
  private val maxInputs = walletSettings.maxInputs
  private val optimalInputs = walletSettings.optimalInputs

  // if checkEIP27 flag is on, we pass re-emission parameters to box selector
  private val reemissionDataOpt = if (walletSettings.checkEIP27) {
    val rs = settings.chainSettings.reemission
    Some(ReemissionData(rs.reemissionNftId, rs.reemissionTokenId))
  } else {
    None
  }
  private val boxSelector = new ReplaceCompactCollectBoxSelector(maxInputs, optimalInputs, reemissionDataOpt)

  override val walletActor: ActorRef =
    ErgoWalletActor(settings, parameters, new ErgoWalletServiceImpl(settings), boxSelector, historyReader)

  private val duration: Duration = Duration.create(10, TimeUnit.SECONDS)

  private var isUtxoSnapshotScannerRunning: Boolean = false
  private var isUtxoSnapshotScannerStarted: Boolean = false

  def scanUtxoSnapshot(msg: ScanBoxesFromUtxoSnapshot): ErgoWallet = {
    isUtxoSnapshotScannerRunning = msg.current < msg.total
    Await.result(walletActor ? msg, duration)
    this
  }

  def scanOffchain(tx: ErgoTransaction): ErgoWallet = {
    walletActor ! ScanOffChain(tx)
    this
  }

  def scanOffchain(txs: Seq[ErgoTransaction]): ErgoWallet = {
    txs.foreach(tx => scanOffchain(tx))
    this
  }

  def scanPersistent(modifier: BlockSection): ErgoWallet = {

    def isUtxoBootStrapping: Boolean = // height is kept at 0 while the scan is running
      settings.nodeSettings.utxoSettings.utxoBootstrap && Await.result(getWalletStatus, duration).height == 0
    val shouldStart: Boolean = !isUtxoSnapshotScannerRunning && !isUtxoSnapshotScannerStarted


    if(isUtxoSnapshotScannerRunning || // scan already running, dont process blocks
      (shouldStart && isUtxoBootStrapping)) { // scan not running, start scanner

      if(!isUtxoSnapshotScannerStarted) {
        actorSystem.eventStream.publish(StartUtxoSetSnapshotScan())
        isUtxoSnapshotScannerStarted = true
        isUtxoSnapshotScannerRunning = true
      }

    }else {
      isUtxoSnapshotScannerStarted = true // this prevents getWalletStatus getting called every time
      modifier match {
        case fb: ErgoFullBlock =>
          walletActor ! ScanOnChain(fb)
        case _ =>
          log.debug("Not full block in ErgoWallet.scanPersistent, which could be the case only if " +
            "state = digest when bootstrapping")
      }
    }
    this
  }

  def scanPersistent(modifiers: Option[BlockSection]): ErgoWallet = {
    modifiers.foldLeft(this) { case (v, mod) =>
      v.scanPersistent(mod)
    }
  }

  def rollback(to: VersionTag): Try[ErgoWallet] =
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

  /**
    * @return read-only copy of this state
    */
  def getReader: ErgoWalletReader = this
}


object ErgoWallet {

  def readOrGenerate(historyReader: ErgoHistoryReader,
                     settings: ErgoSettings,
                     parameters: Parameters)(implicit actorSystem: ActorSystem): ErgoWallet = {
    new ErgoWallet(historyReader, settings, parameters)
  }

}
