package org.ergoplatform.wallet.secrets

import scorex.crypto.hash.Sha256
import scorex.util.encode.Base58

import java.util
import scala.util.{Failure, Success, Try}

/**
  * Wallet Import Format (WIF) codec for secp256k1 private keys.
  *
  * Layout (pre-Base58): version(1) || key(32) || [0x01] || checksum(4)
  * where checksum = first 4 bytes of SHA256(SHA256(version || key || [0x01])).
  *
  * On decode we accept WIFs that carry either the standard Bitcoin version bytes
  * (0x80 mainnet, 0xEF testnet) or the Ergo-specific bytes defined here.
  * The optional 0x01 "compressed pubkey" flag is accepted but not required.
  * On encode we always emit the Ergo-specific version byte and include the flag.
  */
object Wif {

  val MainnetByte: Byte = 0x88.toByte
  val TestnetByte: Byte = 0xC8.toByte

  val BitcoinMainnetByte: Byte = 0x80.toByte
  val BitcoinTestnetByte: Byte = 0xEF.toByte

  val AcceptedVersionBytes: Set[Byte] =
    Set(MainnetByte, TestnetByte, BitcoinMainnetByte, BitcoinTestnetByte)

  val CompressionFlag: Byte = 0x01

  val SecretLength: Int = 32
  val ChecksumLength: Int = 4

  /**
    * Encode a 32-byte secp256k1 scalar as a WIF string with the compression flag set.
    */
  def encode(keyBytes: Array[Byte], mainnet: Boolean): String = {
    require(
      keyBytes.length == SecretLength,
      s"WIF expects $SecretLength-byte secret, got ${keyBytes.length}"
    )
    val version = if (mainnet) MainnetByte else TestnetByte
    val payload = (version +: keyBytes) :+ CompressionFlag
    val checksum = doubleSha256(payload).take(ChecksumLength)
    Base58.encode(payload ++ checksum)
  }

  /**
    * Decode a WIF string into a 32-byte scalar. Validates Base58, length, version
    * byte and checksum. The optional compression flag (0x01) is accepted whether
    * present or absent.
    */
  def decode(wif: String): Try[Array[Byte]] =
    Base58.decode(wif).flatMap { bytes =>
      val expectedWithFlag = 1 + SecretLength + 1 + ChecksumLength
      val expectedNoFlag   = 1 + SecretLength + ChecksumLength
      if (bytes.length != expectedWithFlag && bytes.length != expectedNoFlag) {
        Failure(new IllegalArgumentException(
          s"WIF payload of unexpected length: ${bytes.length}"
        ))
      } else if (!AcceptedVersionBytes.contains(bytes(0))) {
        Failure(new IllegalArgumentException(
          f"WIF version byte 0x${bytes(0) & 0xff}%02x not accepted"
        ))
      } else {
        val (payload, checksum) = bytes.splitAt(bytes.length - ChecksumLength)
        val expectedChecksum = doubleSha256(payload).take(ChecksumLength)
        if (!util.Arrays.equals(checksum, expectedChecksum)) {
          Failure(new IllegalArgumentException("WIF checksum mismatch"))
        } else if (bytes.length == expectedWithFlag &&
                   payload(payload.length - 1) != CompressionFlag) {
          Failure(new IllegalArgumentException(
            "WIF trailing byte must be 0x01 (compression flag)"
          ))
        } else {
          val secretEnd = if (bytes.length == expectedWithFlag) payload.length - 1
                          else payload.length
          Success(util.Arrays.copyOfRange(payload, 1, secretEnd))
        }
      }
    }

  private def doubleSha256(data: Array[Byte]): Array[Byte] =
    Sha256.hash(Sha256.hash(data))

}
