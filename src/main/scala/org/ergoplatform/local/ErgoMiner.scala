package org.ergoplatform.local

import akka.actor.{Actor, ActorRef, Cancellable}
import org.ergoplatform.local.ErgoMiner.{GetLastHeader, MineBlock, StartMining, StopMining}
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{DigestState, UtxoState}
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.{Constants, ErgoSettings}
import scorex.core.LocalInterface.LocallyGeneratedModifier
import scorex.core.NodeViewHolder.GetDataFromCurrentView
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32


class ErgoMiner(ergoSettings: ErgoSettings, viewHolder: ActorRef) extends Actor with ScorexLogging {

  private var cancellableOpt: Option[Cancellable] = None
  private var mining = false
  private var startingNonce = Long.MinValue

  private val powScheme = ergoSettings.chainSettings.poWScheme


  override def preStart(): Unit = {
  }

  override def receive: Receive = {
    case StartMining =>
      log.info("Starting Mining")
      self ! GetLastHeader

    case GetLastHeader =>
      viewHolder ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, Option[Header]] { v =>
        v.pool.take(1000)
        v.history.bestHeaderOpt
      }

    case headerOpt: Option[Header] =>
      startingNonce = Long.MinValue
      self ! MineBlock(headerOpt)

    case StopMining =>

    case MineBlock(lastHeaderOpt) =>

      val emptyADDigest: ADDigest = ADDigest @@ Array.fill(33)(0: Byte)
      val emptyDigest32: Digest32 = Digest32 @@ Array.fill(32)(0: Byte)

      val start = startingNonce
      val finish = start + 10
      startingNonce = finish

      log.info(s"Trying nonces from $start till $finish")

      ergoSettings.chainSettings.poWScheme.prove(
        lastHeaderOpt,
        Constants.InitialNBits,
        emptyADDigest,
        emptyDigest32,
        emptyDigest32,
        1L,
        Array.fill(5)(0: Byte),
        start,
        finish
      ) match {
        case Some(newHeader) =>
          log.info("New block found: " + newHeader)
          viewHolder ! LocallyGeneratedModifier[Header](newHeader)
          self ! MineBlock(Some(newHeader))
        case None =>
          self ! MineBlock(lastHeaderOpt)
      }
  }
}


object ErgoMiner {

  case object StartMining

  case object GetLastHeader

  case object StopMining

  case class MineBlock(lastHeaderOpt: Option[Header])

}