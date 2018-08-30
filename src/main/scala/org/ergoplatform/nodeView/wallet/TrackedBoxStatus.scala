package org.ergoplatform.nodeView.wallet

sealed abstract class BoxCertainty(val certain: Boolean)

object BoxCertainty {
  case object Certain extends BoxCertainty(true)
  case object Uncertain extends BoxCertainty(false)
}

sealed abstract class ChainStatus(val onchain: Boolean)

object ChainStatus {
  case object Onchain extends ChainStatus(true)
  case object Offchain extends ChainStatus(false)
}

sealed abstract class SpendingStatus(val spent: Boolean)

object SpendingStatus {
  case object Spent extends SpendingStatus(true)
  case object Unspent extends SpendingStatus(false)
}
