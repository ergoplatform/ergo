package org.ergoplatform.local

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import akka.testkit.TestKit
import org.ergoplatform.P2PKAddress
import org.ergoplatform.local.ErgoMiner.StartMining
import org.ergoplatform.local.TransactionGenerator.StartGeneration
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.nodeView.wallet.ErgoWalletReader
import org.ergoplatform.nodeView.{ErgoNodeViewRef, ErgoReadersHolderRef}
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.{ErgoTestHelpers, WalletTestOps}
import org.scalatest.FlatSpec
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.SuccessfulTransaction

import scala.collection.immutable
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class TransactionGeneratorSpec extends FlatSpec with ErgoTestHelpers with WalletTestOps {

  type MsgType = SuccessfulTransaction[_]
  val newTransaction: Class[MsgType] = classOf[MsgType]

  val defaultAwaitDuration: FiniteDuration = 10.seconds
  val paymentTransactionAwaitDuration: FiniteDuration = 30.seconds
  val assetTransferTransactionAwaitDuration: FiniteDuration = 1.minute
  val assetIssueTransactionAwaitDuration: FiniteDuration = 2.minutes

  val defaultSettings: ErgoSettings = {
    val empty = ErgoSettings.read(None)

    val nodeSettings = empty.nodeSettings.copy(mining = true,
      stateType = StateType.Utxo,
      miningDelay = 1.second,
      offlineGeneration = true,
      verifyTransactions = true)
    val chainSettings = empty.chainSettings.copy(blockInterval = 1.seconds)
    empty.copy(nodeSettings = nodeSettings, chainSettings = chainSettings)
  }

  def await[A](f: Future[A]): A = Await.result[A](f, defaultAwaitDuration)

  def containsAssetIssuingBox(tx: ErgoTransaction): Boolean = {
    val firstInputId = tx.inputs.head.boxId
    val assetIds = tx.outAssetsOpt.get.map(_._1.data).toSeq
    assetIds.exists(x => java.util.Arrays.equals(x, firstInputId))
  }

  it should "Generate valid transactions of all types" in new TestKit(ActorSystem()) {

    val ergoSettings: ErgoSettings = defaultSettings.copy(directory = createTempDir.getAbsolutePath)

    system.eventStream.subscribe(testActor, newTransaction)

    val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(ergoSettings, timeProvider)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(nodeViewHolderRef)
    expectNoMessage(1.second)
    val r: Readers = await((readersHolderRef ? GetReaders).mapTo[Readers])
    val wallet: ErgoWalletReader = r.w
    val address: P2PKAddress = await(wallet.randomPublicKey())

    val minerRef: ActorRef = ErgoMinerRef(
      ergoSettings,
      nodeViewHolderRef,
      readersHolderRef,
      timeProvider,
      Some(address.script)
    )
    minerRef ! StartMining

    val txGenRef = TransactionGeneratorRef(nodeViewHolderRef, ergoSettings)
    txGenRef ! StartGeneration

    val paymentTxs: immutable.Seq[ErgoTransaction] = receiveWhile(paymentTransactionAwaitDuration) {
      case SuccessfulTransaction(tx: ErgoTransaction) => tx
    }

    val assetTxs: immutable.Seq[ErgoTransaction] = receiveWhile(assetTransferTransactionAwaitDuration) {
      case SuccessfulTransaction(tx: ErgoTransaction) if tx.outAssetsOpt.get.nonEmpty => tx
    }

    val assetIssueTxs: immutable.Seq[ErgoTransaction] = receiveWhile(assetIssueTransactionAwaitDuration) {
      case SuccessfulTransaction(tx: ErgoTransaction) if containsAssetIssuingBox(tx) => tx
    }
  }

}
