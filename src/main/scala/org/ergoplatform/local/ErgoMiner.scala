package org.ergoplatform.local

import akka.actor.{Actor, ActorRef, Cancellable}
import org.ergoplatform.local.ErgoMiner.{MineBlock, StartMining, StopMining}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.DigestState
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.{Constants, ErgoSettings}
import scorex.core.NodeViewHolder.GetDataFromCurrentView
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32


class ErgoMiner(ergoSettings: ErgoSettings, viewHolder: ActorRef) extends Actor with ScorexLogging {

  private var cancellableOpt: Option[Cancellable] = None
  private var mining = false

  private val powScheme = ergoSettings.chainSettings.poWScheme

  override def preStart(): Unit = {
  }

  override def receive: Receive = {
    case StartMining =>
      log.info("Starting Mining")
      viewHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[Header]]{v =>
        v.history.bestHeaderOpt
      }

    case headerOpt: Option[Header] =>
      self ! MineBlock(headerOpt)

    case StopMining =>

    case MineBlock(lastHeaderOpt) =>

      val emptyADDigest: ADDigest = ADDigest @@ Array.fill(33)(0: Byte)
      val emptyDigest32: Digest32 = Digest32 @@ Array.fill(32)(0: Byte)
      log.info("New block found: ")
      println(ergoSettings.chainSettings.poWScheme.prove(None, Constants.InitialNBits, emptyADDigest, emptyDigest32, emptyDigest32,
        1L, Array.fill(5)(0: Byte)))
  }
}


object ErgoMiner {
  case object StartMining

  case object StopMining

  case class MineBlock(lastHeaderOpt: Option[Header])
}