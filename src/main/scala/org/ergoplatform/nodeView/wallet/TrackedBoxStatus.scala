package org.ergoplatform.nodeView.wallet

sealed abstract class BoxCertainty(val certain: Boolean)

object BoxCertainty {
  case object Certain extends BoxCertainty(true)
  case object Uncertain extends BoxCertainty(false)
}

sealed abstract class OnchainStatus(val onchain: Boolean)

object OnchainStatus {
  case object Onchain extends OnchainStatus(true)
  case object Offchain extends OnchainStatus(false)
}

sealed abstract class SpendingStatus(val spent: Boolean)

object SpendingStatus {
  case object Spent extends SpendingStatus(true)
  case object Unspent extends SpendingStatus(false)
}

