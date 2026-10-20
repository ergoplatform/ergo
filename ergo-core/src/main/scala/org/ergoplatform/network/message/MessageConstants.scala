package org.ergoplatform.network.message

/**
  * Type aliases and constants related to P2P network messages formats
  */
object MessageConstants {
  type MessageCode = Byte

  val MagicLength: Int = 4

  val ChecksumLength: Int = 4

  val HeaderLength: Int = MagicLength + 5

  /**
    * Max serialized payload length accepted for any P2P message, including message headers.
    *
    * Set to twice `ModifiersSpec.maxMsgSizeWithReserve` (~16.4 MB), which itself is 4x the
    * `ModifiersSpec.maxMessageSize` base (~2 MB) to accommodate large ADProofs. This bound
    * comfortably exceeds the largest legitimate Ergo messages:
    *   - `Modifiers` messages: up to `ModifiersSpec.maxMsgSizeWithReserve` (~8.4 MB payload)
    *   - UTXO snapshot manifest/chunk messages: up to ~4 MB
    *   - Inv/Request/Sync/Peers messages: kilobytes
    *
    * It also bounds per-peer buffering: 30 peers by default * 16 MB ≈ 480 MB worst-case memory.
    */
  val MaxMessageSize: Int = ModifiersSpec.maxMsgSizeWithReserve * 2

}
