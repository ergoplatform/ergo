package org.ergoplatform.network.message

/**
  * Type aliases and constants related to P2P network messages formats
  */
object MessageConstants {
  type MessageCode = Byte

  val MagicLength: Int = 4

  val ChecksumLength: Int = 4

  val HeaderLength: Int = MagicLength + 5

  val MaxMessageSize: Int = 16 * 1024 * 1024
}
