package org.ergoplatform.nodeView.wallet

import org.ergoplatform.sdk.SecretString

/**
  * Domain-specific type wrappers for wallet operations
  * 
  * These types replace primitive types in communication between the API layer
  * and core wallet logic, making the code more type-safe and self-documenting.
  */
object WalletTypes {

  /**
    * Wrapper for wallet encryption password
    * 
    * Used when initializing, restoring, or unlocking a wallet
    */
  final case class WalletPassword(value: SecretString) extends AnyVal

  /**
    * Wrapper for mnemonic password (BIP-39 passphrase)
    * 
    * Optional password that can be used with a mnemonic for extra security
    */
  final case class MnemonicPassword(value: SecretString) extends AnyVal

  /**
    * Wrapper for wallet mnemonic phrase
    * 
    * The secret recovery phrase used to restore wallet
    */
  final case class WalletMnemonic(value: SecretString) extends AnyVal

  /**
    * Wrapper for BIP-32 derivation path
    * 
    * String representation of a hierarchical deterministic derivation path
    */
  final case class DerivationPathString(value: String) extends AnyVal

  /**
    * Wrapper for scan identifier
    * 
    * Numeric identifier for a wallet scan operation
    */
  final case class ScanIdentifier(value: Int) extends AnyVal

  /**
    * Wrapper for minimum inclusion height
    * 
    * Minimum blockchain height for transaction inclusion
    */
  final case class MinInclusionHeight(value: Int) extends AnyVal

  /**
    * Wrapper for maximum inclusion height
    * 
    * Maximum blockchain height for transaction inclusion
    */
  final case class MaxInclusionHeight(value: Int) extends AnyVal

  /**
    * Wrapper for minimum confirmations count
    * 
    * Minimum number of confirmations required for a transaction
    */
  final case class MinConfirmations(value: Int) extends AnyVal

  /**
    * Wrapper for maximum confirmations count
    * 
    * Maximum number of confirmations to consider for a transaction
    */
  final case class MaxConfirmations(value: Int) extends AnyVal

  /**
    * Wrapper for box index parameter
    * 
    * Index used when reading public keys or wallet boxes
    */
  final case class BoxIndex(value: Int) extends AnyVal

  /**
    * Wrapper for target balance in nanoERG
    * 
    * The amount of ERG requested for box collection
    */
  final case class TargetBalance(value: Long) extends AnyVal

  /**
    * Wrapper for whether to use pre-1627 key derivation
    * 
    * Flag indicating if legacy key derivation should be used
    */
  final case class UsePre1627KeyDerivation(value: Boolean) extends AnyVal

  /**
    * Wrapper for unspent-only filter
    * 
    * Flag indicating whether to return only unspent boxes
    */
  final case class UnspentOnly(value: Boolean) extends AnyVal

  /**
    * Wrapper for consider-unconfirmed filter
    * 
    * Flag indicating whether to consider mempool transactions
    */
  final case class ConsiderUnconfirmed(value: Boolean) extends AnyVal

  /**
    * Wrapper for include-unconfirmed filter
    * 
    * Flag indicating whether to include unconfirmed transactions
    */
  final case class IncludeUnconfirmed(value: Boolean) extends AnyVal

  /**
    * Wrapper for sign transaction flag
    * 
    * Indicates whether to sign a generated transaction
    */
  final case class ShouldSignTransaction(value: Boolean) extends AnyVal

}
