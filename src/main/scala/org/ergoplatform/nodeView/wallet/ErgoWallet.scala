package org.ergoplatform.nodeView.wallet

import org.ergoplatform.{ErgoBox, ErgoTransaction}
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.settings.ErgoSettings
import scorex.core.VersionTag
import scorex.core.transaction.wallet.{Wallet, WalletBox, WalletTransaction}
import scorex.core.utils.ScorexLogging

import scala.util.{Success, Try}

class ErgoWallet extends Wallet[AnyoneCanSpendProposition.type, ErgoTransaction, ErgoPersistentModifier, ErgoWallet]
  with ScorexLogging {
  override type S = Nothing
  override type PI = AnyoneCanSpendProposition.type

  override def secretByPublicImage(publicImage: AnyoneCanSpendProposition.type): Option[Nothing] = None

  override def generateNewSecret(): ErgoWallet = this

  override def historyTransactions: Seq[WalletTransaction[AnyoneCanSpendProposition.type, ErgoTransaction]] = ???

  override def boxes(): Seq[WalletBox[AnyoneCanSpendProposition.type, ErgoBox]] = ???

  override def publicKeys: Set[PI] = Set.empty

  override def secrets: Set[S] = Set.empty

  //todo: implement
  override def scanOffchain(tx: ErgoTransaction): ErgoWallet = this

  //todo: implement
  override def scanOffchain(txs: Seq[ErgoTransaction]): ErgoWallet = this

  //todo: implement
  override def scanPersistent(modifier: ErgoPersistentModifier): ErgoWallet = this

  //todo: implement
  override def rollback(to: VersionTag): Try[ErgoWallet] = Success(this)

  override type NVCT = this.type
}

object ErgoWallet {
  @SuppressWarnings(Array("UnusedMethodParameter"))
  def readOrGenerate(settings: ErgoSettings): ErgoWallet = new ErgoWallet
}
