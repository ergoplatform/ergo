package org.ergoplatform.nodeView.wallet.persistence

import io.iohk.iodb.Store

/**
  * Stores version-sensitive indexes (outputs, balances).
  */
final class WalletRegistry(versionalStorage: Store, freeStorage: Store) {}
