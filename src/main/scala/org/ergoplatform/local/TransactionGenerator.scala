package org.ergoplatform.local

import akka.actor.{Actor, ActorRef, Cancellable}
import org.ergoplatform.local.TransactionGenerator.{GetState, StartGeneration, StopGeneration}
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.{AnyoneCanSpendNoncedBox, AnyoneCanSpendProposition}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.UtxoState
import org.ergoplatform.nodeView.wallet.ErgoWallet
import scorex.core.LocalInterface.LocallyGeneratedTransaction
import scorex.core.NodeViewHolder.GetDataFromCurrentView
import scorex.core.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Random

class TransactionGenerator(viewHolder: ActorRef) extends Actor with ScorexLogging {
  var txGenerator: Cancellable = null

  override def receive: Receive = {
    case StartGeneration =>
      txGenerator = context.system.scheduler.schedule(50 millis, 500 millis)(self ! GetState)

    case GetState =>
      viewHolder ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool,
        IndexedSeq[AnyoneCanSpendNoncedBox]] { v =>

        val boxesToSpentCount = Random.nextInt(5) + 1
        (1 to boxesToSpentCount).flatMap(_ => v.state.randomBox())
      }

    case txBoxes: IndexedSeq[AnyoneCanSpendNoncedBox] =>
      val tx = AnyoneCanSpendTransaction(txBoxes.map(_.nonce), txBoxes.map(_.value))
      viewHolder ! LocallyGeneratedTransaction[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction](tx)

    case StopGeneration =>
      txGenerator.cancel()
  }

}

object TransactionGenerator {
  case object StartGeneration
  case object GetState
  case object StopGeneration
}
