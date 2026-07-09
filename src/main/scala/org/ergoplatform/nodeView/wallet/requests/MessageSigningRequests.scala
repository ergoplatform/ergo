package org.ergoplatform.nodeView.wallet.requests

/**
  * A request to sign an arbitrary message with wallet's private key
  *
  * @param message - message to sign (will be UTF-8 encoded)
  * @param address - optional address to use for signing (if not provided, uses first available key)
  */
case class SignMessageRequest(message: String, address: Option[String])

/**
  * A request to verify a signed message
  *
  * @param message    - original message
  * @param signature  - signature bytes in Base16 encoding
  * @param publicKey  - public key bytes in Base16 encoding
  */
case class VerifySignatureRequest(message: String, signature: String, publicKey: String)
