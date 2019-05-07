package org.ergoplatform.nodeView.wallet

import org.ergoplatform.nodeView.history.ErgoHistory.Height
import scorex.util.ModifierId

import scala.collection.immutable

final case class BalancesSnapshot(height: Height, balance: Long, assetBalances: immutable.Map[ModifierId, Long])
