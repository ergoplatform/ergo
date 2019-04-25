package org.ergoplatform.nodeView.wallet.persistence

/**
  * Stores version-sensitive indexes (outputs, balances).
  */
final class WalletRegistry[B <: TransactionalStorageBackend](storage: B) {}
