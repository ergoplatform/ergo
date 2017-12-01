package org.ergoplatform.local

import akka.actor.{Actor, ActorRef, Cancellable}
import org.ergoplatform.local.TransactionGenerator.{FetchBoxes, StartGeneration, StopGeneration}
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.{AnyoneCanSpendNoncedBox, AnyoneCanSpendProposition}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.UtxoState
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.TestingSettings
import scorex.core.LocalInterface.LocallyGeneratedTransaction
import scorex.core.NodeViewHolder.GetDataFromCurrentView
import scorex.core.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Random

class TransactionGenerator(viewHolder: ActorRef, settings: TestingSettings) extends Actor with ScorexLogging {
  var txGenerator: Cancellable = _

  var isStarted = false

  override def receive: Receive = {
    case StartGeneration =>
      if (!isStarted) {
        context.system.scheduler.schedule(500.millis, 500.millis)(self ! FetchBoxes)
      }

    case FetchBoxes =>
      viewHolder ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool,
        IndexedSeq[AnyoneCanSpendNoncedBox]] { v =>
        if (v.pool.size < settings.keepPoolSize) {
          val boxesToSpentCount = Random.nextInt(10) + 2
          (1 to boxesToSpentCount).flatMap(_ => v.state.randomBox())
        } else {
          IndexedSeq()
        }
      }

    case txBoxes: IndexedSeq[AnyoneCanSpendNoncedBox] =>
      if (txBoxes.nonEmpty) {
        val tx = AnyoneCanSpendTransaction(txBoxes.map(_.nonce), txBoxes.map(_.value))
        viewHolder ! LocallyGeneratedTransaction[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction](tx)
      }

    case StopGeneration =>
      txGenerator.cancel()
  }
}

object TransactionGenerator {

  case object StartGeneration

  case object FetchBoxes

  case object StopGeneration

}