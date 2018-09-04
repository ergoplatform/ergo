package org.ergoplatform.nodeView.wallet

import java.nio.ByteBuffer
import java.util

import com.google.common.primitives.Ints
import io.circe._
import io.circe.syntax._
import org.ergoplatform.settings.ErgoSettings
import scapi.sigma.DLogProtocol.ProveDlog
import scorex.crypto.encode.Base58
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigmastate.Values.{IntConstant, TaggedByteArray, Value}
import sigmastate._
import sigmastate.serialization.{DataSerializer, ValueSerializer}
import sigmastate.utils.ByteBufferReader
import sigmastate.utxo.{DeserializeContext, Slice}

import scala.util.{Failure, Success, Try}


/**
  * An address is a short string corresponding to some script used to protect a box. Unlike (string-encoded) binary
  * representation of a script, an address has some useful characteristics:
  *
  * - Integrity of an address could be checked., as it is incorporating a checksum.
  * - A prefix of address is showing network and an address type.
  * - An address is using an encoding (namely, Base58) which is avoiding similarly l0Oking characters, friendly to
  * double-clicking and line-breaking in emails.
  *
  *
  *
  * An address is encoding network type, address type, checksum, and enough information to watch for a particular scripts.
  *
  * Possible network types are:
  * Mainnet - 0x00
  * Testnet - 0x10
  *
  * Address types are, semantics is described below:
  * 0x01 - Pay-to-PublicKey(P2PK) address
  * 0x02 - Pay-to-Script-Hash(P2SH)
  * 0x03 - Pay-to-Script(P2S)
  *
  * For an address type, we form content bytes as follows:
  *
  * P2PK - serialized (compressed) public key
  * P2sH - 192 bit of the script
  * P2S  - serialized script
  *
  * Address examples for testnet:
  *
  * 3   - P2PK (3WvsT2Gm4EpsM9Pg18PdY6XyhNNMqXDsvJTbbf6ihLvAmSb7u5RN)
  * 8   - P2SH (8UmyuJuQ3FS9ts7j72fn3fKChXSGzbL9WC, 8LnSX95GAWdbDZWJZQ73Uth4uE8HqN3emJ)
  * ?   - P2S (imdaM2NzX, z4hAmfvfSnQJPChMWzfBzJjpB8ei2HoLCZ2RHTaNArMNHFirdJTc7E)
  *
  * for mainnet:
  *
  * 9  - P2PK (9fRAWhdxEsTcdb8PhGNrZfwqa65zfkuYHAMmkQLcic1gdLSV5vA)
  * 2  - P2SH (25qGdVWg2yyYho8uC1pLtc7KxFn4nEEAwD, 23NL9a8ngN28ovtLiKLgHexcdTKBbUMLhH)
  * ?  - P2S (7bwdkU5V8, BxKBaHkvrTvLZrDcZjcsxsF7aSsrN73ijeFZXtbj4CXZHHcvBtqSxQ)
  *
  *
  * Prefix byte = network type + address type
  *
  * checksum = blake2b256(prefix byte ++ content bytes)
  *
  * address = prefix byte ++ content bytes ++ checksum
  *
  */

sealed trait ErgoAddress {
  val addressTypePrefix: Byte

  val contentBytes: Array[Byte]

  val script: Value[SBoolean.type]
}

class P2PKAddress(val pubkey: ProveDlog,
                  val pubkeyBytes: Array[Byte])
                 (implicit val encoder: ErgoAddressEncoder) extends ErgoAddress {

  override val addressTypePrefix: Byte = P2PKAddress.addressTypePrefix

  override val contentBytes: Array[Byte] = pubkeyBytes

  override val script: ProveDlog = pubkey

  override def equals(obj: Any): Boolean = obj match {
    case p2pk: P2PKAddress => util.Arrays.equals(pubkeyBytes, p2pk.pubkeyBytes)
    case _ => false
  }

  override def hashCode(): Int = Ints.fromByteArray(pubkeyBytes.takeRight(4))

  override def toString: String = encoder.toString(this)
}

object P2PKAddress {
  val addressTypePrefix: Byte = 1: Byte

  def apply(pubkey: ProveDlog)(implicit encoder: ErgoAddressEncoder): P2PKAddress = {
    val bs = ValueSerializer.serialize(pubkey)
    new P2PKAddress(pubkey, bs)
  }
}

class Pay2SHAddress(val scriptHash: Array[Byte])(implicit val encoder: ErgoAddressEncoder) extends ErgoAddress {
  override val addressTypePrefix: Byte = Pay2SHAddress.addressTypePrefix

  override val contentBytes: Array[Byte] = scriptHash

  //similar script checked in "P2SH - 160 bits" test in sigma repository, but here we use 192 bits
  override val script: Value[SBoolean.type] = {
    val scriptId = 1: Byte
    val hashEquals = EQ(Slice(CalcBlake2b256(TaggedByteArray(scriptId)), IntConstant(0), IntConstant(24)),
      scriptHash)
    val scriptIsCorrect = DeserializeContext(scriptId, SBoolean)
    AND(hashEquals, scriptIsCorrect)
  }

  override def equals(obj: Any): Boolean = obj match {
    case p2sh: Pay2SHAddress => util.Arrays.equals(scriptHash, p2sh.scriptHash)
    case _ => false
  }

  override def hashCode(): Int = Ints.fromByteArray(scriptHash.takeRight(4))

  override def toString: String = encoder.toString(this)
}

object Pay2SHAddress {
  def apply(pubkey: ProveDlog)(implicit encoder: ErgoAddressEncoder): Pay2SHAddress = apply(pubkey)

  def apply(script: Value[SBoolean.type])(implicit encoder: ErgoAddressEncoder): Pay2SHAddress = {
    val sb = ValueSerializer.serialize(script)
    val sbh = ErgoAddressEncoder.hash192(sb)
    new Pay2SHAddress(sbh)
  }

  val addressTypePrefix: Byte = 2: Byte
}

class Pay2SAddress(override val script: Value[SBoolean.type],
                   val scriptBytes: Array[Byte])
                  (implicit val encoder: ErgoAddressEncoder) extends ErgoAddress {
  override val addressTypePrefix: Byte = Pay2SAddress.addressTypePrefix

  override val contentBytes: Array[Byte] = scriptBytes

  override def equals(obj: Any): Boolean = obj match {
    case p2s: Pay2SAddress => util.Arrays.equals(scriptBytes, p2s.scriptBytes)
    case _ => false
  }

  override def hashCode(): Int = Ints.fromByteArray(scriptBytes.takeRight(4))

  override def toString: String = encoder.toString(this)
}

object Pay2SAddress {
  def apply(script: Value[SBoolean.type])(implicit encoder: ErgoAddressEncoder): Pay2SAddress = {
    val sb = ValueSerializer.serialize(script)
    new Pay2SAddress(script, sb)
  }

  val addressTypePrefix: Byte = 3: Byte
}


case class ErgoAddressEncoder(settings: ErgoSettings) {

  import ErgoAddressEncoder._

  implicit private val ergoAddressEncoder: ErgoAddressEncoder = this

  val ChecksumLength = 4

  private val networkPrefix = settings.chainSettings.addressPrefix

  def toString(address: ErgoAddress): String = {
    val withNetworkByte = (networkPrefix + address.addressTypePrefix).toByte +: address.contentBytes

    val checksum = hash256(withNetworkByte).take(ChecksumLength)
    Base58.encode(withNetworkByte ++ checksum)
  }

  def fromString(addrStr: String): Try[ErgoAddress] = Base58.decode(addrStr).flatMap { bytes =>
    Try {
      val headByte = bytes.head
      require(headByte >= networkPrefix)
      val addressType = (headByte - networkPrefix).toByte
      val (withoutChecksum, checksum) = bytes.splitAt(bytes.length - ChecksumLength)

      if (!util.Arrays.equals(hash256(withoutChecksum).take(ChecksumLength), checksum)) {
        throw new Exception(s"Checksum check fails for $addrStr")
      }

      val bs = withoutChecksum.tail

      addressType match {
        case P2PKAddress.addressTypePrefix =>
          new P2PKAddress(ValueSerializer.deserialize(bs).asInstanceOf[ProveDlog], bs)
        case Pay2SHAddress.addressTypePrefix =>
          new Pay2SHAddress(bs)
        case Pay2SAddress.addressTypePrefix =>
          new Pay2SAddress(ValueSerializer.deserialize(bs).asInstanceOf[Value[SBoolean.type]], bs)
        case _ => throw new Exception("Unsupported address type: " + addressType)
      }
    }
  }

  implicit val encoder: Encoder[ErgoAddress] = { address =>
    toString(address).asJson
  }

  implicit val decoder: Decoder[ErgoAddress] = { cursor =>
    def decodeString(addrStr: String) = {
      fromString(addrStr) match {
        case Success(address) => Right(address)
        case Failure(exception) => Left(DecodingFailure(exception.toString, cursor.history))
      }
    }

    for {
      addressStr <- cursor.as[String]
      address <- decodeString(addressStr)
    } yield address
  }
}

object ErgoAddressEncoder {
  def hash256(input: Array[Byte]): Digest32 = Blake2b256(input)

  def hash192(input: Array[Byte]): Array[Byte] = hash256(input).take(24)
}
