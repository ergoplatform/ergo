package org.ergoplatform.utils.fixtures

import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.{ErgoSettings, Parameters}

class WalletFixture(
  settings: ErgoSettings,
  params: Parameters,
  getWallet: WalletFixture => ErgoWallet
) extends NodeViewFixture(settings, params) {
  val wallet: ErgoWallet = getWallet(this)
}
