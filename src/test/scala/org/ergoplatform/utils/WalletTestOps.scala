package org.ergoplatform.utils

import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.wallet.{BalancesSnapshot, ErgoAddress, ErgoWallet, P2PKAddress}
import sigmastate.SBoolean
import sigmastate.Values.Value

import scala.concurrent.Await

trait WalletTestOps extends NodeViewBaseOps {

  def wallet(implicit ctx: Ctx): ErgoWallet = getCurrentView.vault
  def defaultAddress(implicit ctx: Ctx): P2PKAddress = addresses.head.asInstanceOf[P2PKAddress]
  def defaultPubKey(implicit ctx: Ctx): Value[SBoolean.type] = defaultAddress.pubkey
  def defaultProofBytes(implicit ctx: Ctx): Array[Byte] = defaultAddress.contentBytes

  def addresses(implicit ctx: Ctx): Seq[ErgoAddress] =
    Await.result(wallet.trackedAddresses(), awaitDuration)

  def getConfirmedBalances(implicit ctx: Ctx): BalancesSnapshot =
    Await.result(wallet.confirmedBalances(), awaitDuration)

  def getUnconfirmedBalances(implicit ctx: Ctx): BalancesSnapshot =
    Await.result(wallet.unconfirmedBalances(), awaitDuration)

  def scanningInterval(implicit ctx: Ctx): Long = ctx.settings.walletSettings.scanningInterval.toMillis
  def sumOutputs(block: ErgoFullBlock): Long = sum(outputs(block))
  def sum(boxes: Seq[ErgoBox]): Long = boxes.map(_.value).sum
  def outputs(block: ErgoFullBlock): Seq[ErgoBox] = block.transactions.flatMap(_.outputs)

  def scanTime(block: ErgoFullBlock)(implicit ctx: Ctx): Long = scanTime(outputs(block).size)
  def scanTime(boxCount: Int)(implicit ctx: Ctx): Long = boxCount * scanningInterval + 1000
  def offlineScanTime(tx: ErgoTransaction): Long = tx.outputs.size * 100 + 300
}
