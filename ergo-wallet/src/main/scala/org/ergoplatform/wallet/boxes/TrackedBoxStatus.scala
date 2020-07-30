package org.ergoplatform.wallet.boxes


sealed abstract class ChainStatus(val onChain: Boolean)

object ChainStatus {

  /** Box is presented in blockchain (main chain only). */
  case object OnChain extends ChainStatus(onChain = true)

  /** Box isn't presented in blockchain (awaiting in mempool to be added). */
  case object OffChain extends ChainStatus(onChain = false)

}

sealed abstract class SpendingStatus(val spent: Boolean)

object SpendingStatus {

  case object Spent extends SpendingStatus(spent = true)

  case object Unspent extends SpendingStatus(spent = false)

}
