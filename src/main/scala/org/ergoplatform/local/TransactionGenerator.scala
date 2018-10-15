package org.ergoplatform.local

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import org.ergoplatform.local.TransactionGenerator.{Attempt, StartGeneration}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.UtxoState
import org.ergoplatform.nodeView.wallet._
import org.ergoplatform.nodeView.wallet.requests.{AssetIssueRequest, PaymentRequest}
import org.ergoplatform.settings.{Algos, ErgoSettings}
import scorex.core.NodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, LocallyGeneratedTransaction}
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{SemanticallySuccessfulModifier, SuccessfulTransaction}
import scorex.crypto.hash.Digest32
import scorex.util.ScorexLogging
import scorex.util.encode.Base16
import sigmastate.Values

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Random, Success, Try}


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
  @volatile private var propositions: Seq[P2PKAddress] = Seq()

  // The greater skip bias of particular transaction type the less frequently it is generated.
  private val assetTransferSkipBias = 5
  private val assetIssueSkipBias = 15

  private val MaxTransactionsPerBlock = settings.testingSettings.maxTransactionsPerBlock
  private implicit val ergoAddressEncoder: ErgoAddressEncoder = new ErgoAddressEncoder(settings)

  override def receive: Receive = {
    case StartGeneration =>
      log.info("Starting testing transactions generation, with maxTransactionsPerBlock = " + MaxTransactionsPerBlock)
      context.system.eventStream.subscribe(self, classOf[SuccessfulTransaction[ErgoTransaction]])
      context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[ErgoFullBlock]])

      viewHolder ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, Unit] { v =>
        currentFullHeight = v.history.headersHeight
        v.vault.publicKeys(0, 100).onComplete(_.foreach(pks => propositions = pks))
      }

    case SemanticallySuccessfulModifier(fullBlock: ErgoFullBlock) if fullBlock.isInstanceOf[ErgoFullBlock] =>
      val blockHeight = fullBlock.header.height
      if (blockHeight > currentFullHeight) {
        currentFullHeight = blockHeight
        transactionsPerBlock = Random.nextInt(MaxTransactionsPerBlock) + 1
        log.info(s"Going to generate $transactionsPerBlock transactions upon receiving a block at height $blockHeight")
        self ! Attempt
      }

    case Attempt =>
      transactionsPerBlock -= 1
      if (transactionsPerBlock >= 0 && propositions.nonEmpty) {
        viewHolder ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, Unit] { v =>
          genTransaction(v.vault).onComplete(t => self ! t.flatten)
        }
      }

    case Success(tx: ErgoTransaction@unchecked) =>
      log.info("Locally generated tx: " + tx)
      viewHolder ! LocallyGeneratedTransaction[ErgoTransaction](tx)

    case Failure(e) =>
      log.info(s"Failed to generate tx: ${e.getMessage}")

    case SuccessfulTransaction(_) => self ! Attempt
  }

  private def genTransaction(wallet: ErgoWallet): Future[Try[ErgoTransaction]] = {
    val feeReq = PaymentRequest(Pay2SAddress(Values.TrueLeaf), fee, None, None)
    val amountToPay = (Random.nextInt(10) + 1) * 100000000
    val tokenPaymentReq = wallet.confirmedBalances().map { balances =>
      if (balances.assetBalances.nonEmpty && probabilisticPredicate(assetTransferSkipBias)) {
        val tokenToSpend = balances.assetBalances.toSeq(Random.nextInt(balances.assetBalances.size))
        val tokenAmountToSpend = tokenToSpend._2 / 4
        Algos.decode(tokenToSpend._1).map { id =>
          PaymentRequest(randProposition, amountToPay, Some(Seq(Digest32 @@ id -> tokenAmountToSpend)), None)
        }.toOption
      } else {
        Some(PaymentRequest(randProposition, amountToPay, None, None))
      }
    }
    val assetIssueReq = tokenPaymentReq
      .flatMap(reqOpt => wallet.inputsFor(reqOpt.map(Seq(feeReq) :+ _).getOrElse(Seq(feeReq))))
      .map { inputs =>
        if (probabilisticPredicate(assetIssueSkipBias)) {
          inputs.headOption.map { firstInput =>
            val assetId = Digest32 !@@ firstInput.id
            val assetInfo = genNewAssetInfo
            AssetIssueRequest(randProposition, assetId, assetInfo._1, assetInfo._2, assetInfo._3, assetInfo._4)
          }
        } else {
          None
        }
      }
    tokenPaymentReq.flatMap { tpOutOpt =>
      assetIssueReq.flatMap { aiOutOpt =>
        val requests = Seq(tpOutOpt, aiOutOpt, Some(feeReq)).flatten
        wallet.generateTransaction(requests)
      }
    }
  }

  private def randProposition = propositions(Random.nextInt(propositions.size))

  // p = 1/i
  private def probabilisticPredicate(i: Int): Boolean = Random.nextInt(Int.MaxValue) < Int.MaxValue / i

  private def genNewAssetInfo = {
    val emissionAmount: Int = (Random.nextInt(10) + 1) * 100000000
    val tokenName: String = Base16.encode(scorex.util.Random.randomBytes(4)).toUpperCase
    val tokenDescription: String = s"$tokenName description"
    val tokenDecimals: Int = Random.nextInt(8) + 4
    (emissionAmount, tokenName, tokenDescription, tokenDecimals)
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
