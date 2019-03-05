package org.ergoplatform.utils.fixtures

import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.ErgoSettings

class WalletFixture(settings: ErgoSettings, getWallet: WalletFixture => ErgoWallet) extends NodeViewFixture(settings) {
  val wallet: ErgoWallet = getWallet(this)
}
