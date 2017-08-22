package org.ergoplatform.nodeView.wallet

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.{AnyoneCanSpendNoncedBox, AnyoneCanSpendProposition}
import org.ergoplatform.settings.ErgoSettingsT
import scorex.core.transaction.box.Box
import scorex.core.transaction.wallet.{Wallet, WalletBox, WalletTransaction}
import scorex.core.utils.ScorexLogging

import scala.util.Try

class ErgoWallet extends Wallet[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction, ErgoPersistentModifier, ErgoWallet]
  with ScorexLogging {
  override type S = Nothing
  override type PI = AnyoneCanSpendProposition.type

  override def secretByPublicImage(publicImage: AnyoneCanSpendProposition.type): Option[Nothing] = None

  override def generateNewSecret(): ErgoWallet = this

  override def historyTransactions: Seq[WalletTransaction[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction]] = ???

  override def boxes(): Seq[WalletBox[AnyoneCanSpendProposition.type, AnyoneCanSpendNoncedBox]] = ???

  override def publicKeys: Set[PI] = Set()

  override def secrets: Set[S] = Set()

  override def scanOffchain(tx: AnyoneCanSpendTransaction): ErgoWallet = ???

  override def scanOffchain(txs: Seq[AnyoneCanSpendTransaction]): ErgoWallet = ???

  override def scanPersistent(modifier: ErgoPersistentModifier): ErgoWallet = ???

  override def rollback(to: VersionTag): Try[ErgoWallet] = ???

  override type NVCT = this.type
}

object ErgoWallet {
  def readOrGenerate(settings: ErgoSettingsT): ErgoWallet = new ErgoWallet
}