package org.ergoplatform.nodeView.wallet

import org.ergoplatform.nodeView.history.ErgoHistory.Height
import org.ergoplatform.wallet.TokensMap

final case class BalancesSnapshot(height: Height, balance: Long, assetBalances: TokensMap)
