package org.ergoplatform.local

import akka.actor.{Actor, ActorRef, ActorRefFactory, Cancellable, Props}
import org.ergoplatform.ErgoBoxCandidate
import org.ergoplatform.local.TransactionGenerator.{FetchBoxes, StartGeneration, StopGeneration}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.UtxoState
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.nodeView.wallet.ErgoWalletActor.GenerateTransaction
import org.ergoplatform.settings.TestingSettings
import scorex.core.NodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, LocallyGeneratedTransaction}
import scorex.core.utils.ScorexLogging
import sigmastate.Values

import scala.concurrent.duration._
import scala.util.Random


class TransactionGenerator(viewHolder: ActorRef,
                           ergoWalletActor: ActorRef,
                           settings: TestingSettings) extends Actor with ScorexLogging {
  var txGenerator: Cancellable = _

  var isStarted = false

  var currentFullHeight = 0

  @SuppressWarnings(Array("TraversableHead"))
  override def receive: Receive = {
    case StartGeneration =>
      if (!isStarted) {
        viewHolder ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, Unit] { v =>
          currentFullHeight = v.history.headersHeight

          txGenerator = context.system.scheduler
            .schedule(1500.millis, 3000.millis)(self ! FetchBoxes)(context.system.dispatcher)
        }
      }

      isStarted = true

    case FetchBoxes =>
      viewHolder ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, Unit] { v =>
        val fbh = v.history.fullBlockHeight
        if (fbh > currentFullHeight) {
          currentFullHeight = fbh

          //todo: real prop, assets

          val txCount = Random.nextInt(20) + 1

          (1 to txCount).foreach { _ =>
            val newOutsCount = Random.nextInt(50) + 1
            val newOuts = (1 to newOutsCount).map { _ =>
              val value = Random.nextInt(50) + 1
              val prop = if (Random.nextBoolean()) Values.TrueLeaf else Values.FalseLeaf
              new ErgoBoxCandidate(value, prop)
            }

            ergoWalletActor ! GenerateTransaction(newOuts)
          }
        }
      }

    case txOpt: Option[ErgoTransaction]@unchecked =>
      txOpt.foreach { tx =>
        println("Locally generated tx: " + tx)
        viewHolder ! LocallyGeneratedTransaction[ErgoTransaction](tx)
      }

    case StopGeneration =>
      isStarted = false
      txGenerator.cancel()
  }
}

object TransactionGenerator {

  case object StartGeneration

  case object FetchBoxes

  case object StopGeneration

}

object TransactionGeneratorRef {
  def props(viewHolder: ActorRef, ergoWalletActor: ActorRef, settings: TestingSettings): Props =
    Props(new TransactionGenerator(viewHolder, ergoWalletActor, settings))

  def apply(viewHolder: ActorRef, ergoWalletActor: ActorRef, settings: TestingSettings)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(viewHolder, ergoWalletActor, settings))

  def apply(viewHolder: ActorRef, ergoWalletActor: ActorRef, settings: TestingSettings, name: String)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(viewHolder, ergoWalletActor, settings), name)
}
