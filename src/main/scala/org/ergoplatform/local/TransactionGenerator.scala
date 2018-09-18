package org.ergoplatform.local

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import org.ergoplatform.local.TransactionGenerator.{Attempt, StartGeneration}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.UtxoState
import org.ergoplatform.nodeView.wallet._
import org.ergoplatform.settings.ErgoSettings
import scorex.core.NodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, LocallyGeneratedTransaction}
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{SemanticallySuccessfulModifier, SuccessfulTransaction}
import scorex.util.ScorexLogging
import sigmastate.Values

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Random, Try}


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

  private val fee: Long = 100000
  private var transactionsPerBlock = 0
  private var currentFullHeight = 0
  private var propositions: Seq[P2PKAddress] = Seq()

  private val MaxTransactionsPerBlock = settings.testingSettings.maxTransactionsPerBlock
  private implicit val ergoAddressEncoder = new ErgoAddressEncoder(settings)

  override def receive: Receive = {
    case StartGeneration =>
      log.info("Starting testing transactions generation, with maxTransactionsPerBlock = " + MaxTransactionsPerBlock)

      viewHolder ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, Unit] { v =>
        currentFullHeight = v.history.headersHeight
        v.vault.publicKeys(0, 100).onComplete(_.foreach(pks => propositions = pks))

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
      //todo: assets
      transactionsPerBlock = transactionsPerBlock - 1
      if (transactionsPerBlock >= 0 && propositions.nonEmpty) {
        val feeOut = PaymentRequest(Pay2SAddress(Values.TrueLeaf), fee, None, None)
        val amountToPay = (Random.nextInt(10) + 1) * 100000000
        val paymentOut = PaymentRequest(propositions(Random.nextInt(propositions.size)), amountToPay, None, None)
        viewHolder ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, Unit] { v =>
          v.vault.generateTransaction(Seq(feeOut, paymentOut)).onComplete(t => self ! t.flatten)
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
