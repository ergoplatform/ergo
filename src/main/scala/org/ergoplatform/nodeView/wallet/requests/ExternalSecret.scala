package org.ergoplatform.nodeView.wallet.requests

import org.ergoplatform.wallet.secrets.PrimitiveSecretKey

/**
  * Externally provided secret (to be used once for a transaction to sign)
  *
  * @param key - the secret
  */
case class ExternalSecret(key: PrimitiveSecretKey)
