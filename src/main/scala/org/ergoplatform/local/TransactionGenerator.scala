package org.ergoplatform.local

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import org.ergoplatform.local.TransactionGenerator.{Attempt, StartGeneration}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.UtxoState
import org.ergoplatform.nodeView.wallet.{ErgoAddressEncoder, ErgoWallet, Pay2SAddress, PaymentRequest}
import org.ergoplatform.settings.ErgoSettings
import scorex.core.NodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, LocallyGeneratedTransaction}
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{SemanticallySuccessfulModifier, SuccessfulTransaction}
import sigmastate.Values

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Random, Try}
import scorex.util.ScorexLogging


/**
  * Transaction generator, which is generating testing transactions (presumably, for testnets, but this is
  * not necessary).
  *
  * It is enough to send once "StartGeneration" signal to the generator and then it will generate a random number of
  * transactions (up to "ergo"/"testing"/"maxTransactionsPerBlock" parameter in the settings) per each block arrived.
  * When the block arrives a following working cycle happens: if counter is less than transactions generated,
  * try generate a transaction; if the transaction is being successfully generated, send it to the node view holder (to
  * check its correctness), then, if transaction is successfully adopted by node view holder components, repeat.
  */
class TransactionGenerator(viewHolder: ActorRef,
                           settings: ErgoSettings) extends Actor with ScorexLogging {

  private var transactionsPerBlock = 0
  private var currentFullHeight = 0

  private val MaxTransactionsPerBlock = settings.testingSettings.maxTransactionsPerBlock
  private implicit val ergoAddressEncoder = new ErgoAddressEncoder(settings)

  override def receive: Receive = {
    case StartGeneration =>
      log.info("Starting testing transactions generation, with maxTransactionsPerBlock = " + MaxTransactionsPerBlock)

      viewHolder ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, Unit] { v =>
        currentFullHeight = v.history.headersHeight

        context.system.eventStream.subscribe(self, classOf[SuccessfulTransaction[ErgoTransaction]])

        context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[ErgoFullBlock]])
      }

    case SemanticallySuccessfulModifier(fb: ErgoFullBlock) if fb.isInstanceOf[ErgoFullBlock] =>
      val fbh = fb.header.height
      if (fbh > currentFullHeight) {
        currentFullHeight = fbh
        transactionsPerBlock = Random.nextInt(MaxTransactionsPerBlock) + 1
        log.info(s"Going to generate $transactionsPerBlock transactions upon receiving a block at height $fbh")
        self ! Attempt
      }

    case Attempt =>
      //todo: real prop, assets
      transactionsPerBlock = transactionsPerBlock - 1
      if (transactionsPerBlock >= 0) {
        val newOutsCount = Random.nextInt(50) + 1
        val newOuts = (1 to newOutsCount).map { _ =>
          val value = Random.nextInt(50) + 1
          val prop = if (Random.nextBoolean()) Values.TrueLeaf else Values.FalseLeaf

          PaymentRequest(Pay2SAddress(prop), value, None, None)
        }

        viewHolder ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, Unit] { v =>
          v.vault.generateTransaction(newOuts).onComplete(t => self ! t.flatten)
        }
      }

    case txTry: Try[ErgoTransaction]@unchecked =>
      txTry.foreach { tx =>
        log.info("Locally generated tx: " + tx)
        viewHolder ! LocallyGeneratedTransaction[ErgoTransaction](tx)
      }

    case SuccessfulTransaction(_) => self ! Attempt
  }
}

object TransactionGenerator {

  case object StartGeneration

  case object CheckGeneratingConditions

  case object Attempt

}

object TransactionGeneratorRef {
  def props(viewHolder: ActorRef, settings: ErgoSettings): Props =
    Props(new TransactionGenerator(viewHolder, settings))

  def apply(viewHolder: ActorRef, settings: ErgoSettings)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(viewHolder, settings))

  def apply(viewHolder: ActorRef, settings: ErgoSettings, name: String)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(viewHolder, settings), name)
}
