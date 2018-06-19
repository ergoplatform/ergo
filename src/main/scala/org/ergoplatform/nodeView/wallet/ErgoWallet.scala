package org.ergoplatform.nodeView.wallet


import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.settings.ErgoSettings
import scorex.core.VersionTag
import scorex.core.transaction.wallet.Vault
import scorex.core.utils.ScorexLogging

import scala.util.{Success, Try}

class ErgoWallet extends Vault[ErgoTransaction, ErgoPersistentModifier, ErgoWallet]
  with ScorexLogging {



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
