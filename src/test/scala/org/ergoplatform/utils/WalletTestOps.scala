package org.ergoplatform.utils

import org.ergoplatform.ErgoBox.{NonMandatoryRegisterId, R4}
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.state.{StateType, UtxoState}
import org.ergoplatform.nodeView.wallet.{BalancesSnapshot, ErgoAddress, ErgoWallet}
import org.ergoplatform.settings.ErgoSettings
import sigmastate.{SBoolean, SLong}
import sigmastate.Values.{EvaluatedValue, LongConstant, TrueLeaf, Value}
import sigmastate.interpreter.{ContextExtension, ProverResult}

import scala.concurrent.Await
import scala.concurrent.duration._

trait WalletTestOps extends NodeViewBaseOps {

  override def initSettings: ErgoSettings = {
    val settings = NodeViewTestConfig(StateType.Utxo, verifyTransactions = true, popowBootstrap = false).toSettings
    settings.copy(walletSettings = settings.walletSettings.copy(scanningInterval = 15.millis))
  }

  def withFixture[T](test: WalletFixture => T): T = {
    new WalletFixture(settings, getCurrentView(_).vault).apply(test)
  }

  def wallet(implicit w: WalletFixture): ErgoWallet = w.wallet

  def getTrackedAddresses(implicit w: WalletFixture): Seq[ErgoAddress] =
    Await.result(w.wallet.trackedAddresses(), awaitDuration)

  def getConfirmedBalances(implicit w: WalletFixture): BalancesSnapshot =
    Await.result(w.wallet.confirmedBalances(), awaitDuration)

  def getUnconfirmedBalances(implicit w: WalletFixture): BalancesSnapshot =
    Await.result(w.wallet.unconfirmedBalances(), awaitDuration)

  def scanningInterval(implicit ctx: Ctx): Long = ctx.settings.walletSettings.scanningInterval.toMillis
  def scanTime(block: ErgoFullBlock)(implicit ctx: Ctx): Long = scanTime(block.transactions.flatMap(_.outputs).size)
  def scanTime(boxCount: Int)(implicit ctx: Ctx): Long = boxCount * scanningInterval + 1000
  def offchainScanTime(tx: ErgoTransaction): Long = tx.outputs.size * 100 + 300

  def sum(boxes: Seq[ErgoBox]):Long = boxes.map(_.value).sum

  def boxesAvailable(block: ErgoFullBlock, script: Value[SBoolean.type]): Seq[ErgoBox] = {
    block.transactions.flatMap(boxesAvailable(_, script))
  }

  def boxesAvailable(tx: ErgoTransaction, script: Value[SBoolean.type]): Seq[ErgoBox] = {
    tx.outputs.filter(_.proposition == script)
  }

  def getUtxoState(implicit ctx: Ctx): UtxoState = getCurrentState.asInstanceOf[UtxoState]

  def makeGenesisBlock(script: Value[SBoolean.type])(implicit ctx: Ctx): ErgoFullBlock = {
    makeNextBlock(getUtxoState, Seq(makeGenesisTx(script)))
  }

  def makeGenesisTx(script: Value[SBoolean.type]): ErgoTransaction = {
    //ErgoMiner.createCoinbase(Some(genesisEmissionBox), 0, Seq.empty, script, emission)
    val emissionBox = genesisEmissionBox
    val height = 0
    val emissionAmount = emission.emissionAtHeight(height)
    val newEmissionAmount = emissionBox.value - emissionAmount
    val emissionRegs = Map[NonMandatoryRegisterId, EvaluatedValue[SLong.type]](R4 -> LongConstant(height))
    val inputs = IndexedSeq(new Input(emissionBox.id, ProverResult(Array.emptyByteArray, ContextExtension.empty)))
    val newEmissionBox = new ErgoBoxCandidate(newEmissionAmount, emissionBox.proposition, Seq(), emissionRegs)
    val minerBox = new ErgoBoxCandidate(emissionAmount, script, Seq.empty, Map.empty)
    ErgoTransaction(inputs, IndexedSeq(newEmissionBox, minerBox))
  }

  def makeSpendingTx(boxesToSpend: Seq[ErgoBox],
                     addressToSpend: ErgoAddress,
                     balanceToReturn: Long = 0): ErgoTransaction = {
    val proof = ProverResult(addressToSpend.contentBytes, ContextExtension.empty)
    val inputs = boxesToSpend.map(box => Input(box.id, proof))
    val balanceToSpend = boxesToSpend.map(_.value).sum - balanceToReturn
    val spendingOutput = IndexedSeq(new ErgoBoxCandidate(balanceToSpend, TrueLeaf))
    val outputs = if (balanceToReturn == 0) {
      spendingOutput
    } else {
      new ErgoBoxCandidate(balanceToReturn, addressToSpend.script) +: spendingOutput
    }
    ErgoTransaction(inputs.toIndexedSeq, outputs)
  }
}
