package org.ergoplatform.wallet.boxes

sealed abstract class BoxCertainty(val certain: Boolean)

object BoxCertainty {

  /** The fact that box could be spent with known key is confirmed. */
  case object Certain extends BoxCertainty(certain = true)

  /** Known public key(s) was(were) detected in box proposition,
    * but prover isn't certain whether he could spend it */
  case object Uncertain extends BoxCertainty(certain = false)

  def fromBoolean(value: Boolean): BoxCertainty = if(value) {
    Certain
  } else {
    Uncertain
  }

}

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
