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
    * Max message in p2p networking protocol, including headers. Reasonably bigger than max modifier size (8M),
    * defined by `ModifiersSpec.maxMsgSizeWithReserve`. Also enough to have reasonable guaranteed max memory consumption
    * by p2p messaging buffers (30 peers by default * 16M = 480M).
    */
  val MaxMessageSize: Int = 16 * 1024 * 1024
}
