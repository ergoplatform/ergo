package org.ergoplatform.nodeView.wallet

import org.ergoplatform.nodeView.history.ErgoHistoryConstants.Height
import org.ergoplatform.sdk.wallet.TokensMap

final case class BalancesSnapshot(height: Height, balance: Long, assetBalances: TokensMap)
