package org.ergoplatform.nodeView.wallet

import java.nio.ByteBuffer
import java.util

import com.google.common.primitives.Ints
import org.ergoplatform.settings.{Algos, ErgoSettings}
import scapi.sigma.DLogProtocol.ProveDlog
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256
import sigmastate.{SBoolean, SGroupElement}
import sigmastate.Values.Value
import sigmastate.serialization.{DataSerializer, ValueSerializer}
import sigmastate.utils.ByteBufferReader

import scala.util.Try


/**
  * An address is a short string which integrity could be checked. A prefix of address is showing network and
  * address type. An address type is showing a script used to protect a box.
  *
  * An address is encoding network type, address type, checksum, and enough information to watch for a particular
  * script.
  *
  * Possible network types are:
  * Mainnet - 0x00
  * Testnet - 0x10
  *
  * //todo: write concrete ErgoScript scripts
  * Address types are:
  * 0x00 - Pay-to-PublicKey-Hash(P2PKH) address which is corresponding to the script ???
  * 0x01 - Pay-to-PublicKey(P2PK) address which is corresponding to the script ???
  * 0x02 - Pay-to-Script-Hash(P2SH) which is corresponding to the script ???
  * 0x03 - Pay-to-Script(P2S) which is representing a script as is
  *
  * For an address type, we form content bytes as follows:
  *
  * P2PKH - 160-bit hash of the publicKey
  * P2PK - serialized (compressed) public key
  * P2sH - 160 bit of the script
  * P2S  - serialized script
  *
  * Address examples for testnet:
  *
  * 7   - P2PKH (7aqf1Do41Vw4fEKYhbWwohQrtHoMRSVCZx)
  * 3   - P2PK (3WvsT2Gm4EpsM9Pg18PdY6XyhNNMqXDsvJTbbf6ihLvAmSb7u5RN)
  * 8   - P2SH (8UmyuJuQ3FS9ts7j72fn3fKChXSGzbL9WC, 8LnSX95GAWdbDZWJZQ73Uth4uE8HqN3emJ)
  * ?   - P2S (imdaM2NzX, z4hAmfvfSnQJPChMWzfBzJjpB8ei2HoLCZ2RHTaNArMNHFirdJTc7E)
  *
  * for mainnet:
  *
  * 1  - P2PKH (1H1Z9V7mmhnARuFNrig3jMPmAWEirSKHza)
  * 9  - P2PK (9fRAWhdxEsTcdb8PhGNrZfwqa65zfkuYHAMmkQLcic1gdLSV5vA)
  * 2  - P2SH (25qGdVWg2yyYho8uC1pLtc7KxFn4nEEAwD, 23NL9a8ngN28ovtLiKLgHexcdTKBbUMLhH)
  * ?  - P2S (7bwdkU5V8, BxKBaHkvrTvLZrDcZjcsxsF7aSsrN73ijeFZXtbj4CXZHHcvBtqSxQ)
  *
  *
  * Prefix byte = network type + address type
  *
  * checksum = hash256(prefix byte ++ content bytes)
  *
  * address = prefix byte ++ content bytes ++ checksum
  *
  */

sealed trait ErgoAddress {
  val addressTypePrefix: Byte
}

case class P2PKHAddress(addressHash: Array[Byte]) extends ErgoAddress {
  override val addressTypePrefix: Byte = P2PKHAddress.addressTypePrefix

  override def equals(obj: scala.Any): Boolean = obj match {
    case P2PKHAddress(otherHash) => util.Arrays.equals(addressHash, otherHash)
    case _ => false
  }

  override def hashCode(): Int = Ints.fromByteArray(addressHash.takeRight(4))

  override def toString = s"P2PKH(${Algos.encode(addressHash)})"
}

object P2PKHAddress {
  val addressTypePrefix: Byte = 0: Byte

  def apply(pubkey: ProveDlog): P2PKHAddress = {
    val bt = ValueSerializer.serialize(pubkey)
    P2PKHAddress(ErgoAddressEncoder.hash160(bt))
  }
}

case class P2PKAddress(pubkey: ProveDlog, pubkeyBytes: Array[Byte]) extends ErgoAddress {
  override val addressTypePrefix: Byte = P2PKAddress.addressTypePrefix

  override def equals(obj: scala.Any): Boolean = obj match {
    case P2PKAddress(pk, pkb) => util.Arrays.equals(pubkeyBytes, pkb) && pk == pubkey
    case _ => false
  }

  override def hashCode(): Int = Ints.fromByteArray(pubkeyBytes.takeRight(4))

  override def toString = s"P2PK(${Algos.encode(pubkeyBytes)})"
}

object P2PKAddress {
  val addressTypePrefix: Byte = 1: Byte

  def apply(pubkey: ProveDlog): P2PKAddress = {
    val bs = pubkey.h.getEncoded(true)
    P2PKAddress(pubkey, bs)
  }
}

case class ScriptHashAddress(scriptHash: Array[Byte]) extends ErgoAddress {
  override val addressTypePrefix: Byte = ScriptHashAddress.addressTypePrefix

  override def equals(obj: scala.Any): Boolean = obj match {
    case ScriptHashAddress(otherHash) => util.Arrays.equals(scriptHash, otherHash)
    case _ => false
  }

  override def hashCode(): Int = Ints.fromByteArray(scriptHash.takeRight(4))

  override def toString = s"P2SH(${Algos.encode(scriptHash)})"
}

object ScriptHashAddress {
  def apply(script: Value[SBoolean.type]): ScriptHashAddress = {
    val sb = ValueSerializer.serialize(script)
    val sbh = ErgoAddressEncoder.hash160(sb)
    ScriptHashAddress(sbh)
  }

  val addressTypePrefix: Byte = 2: Byte
}

case class ScriptAddress(script: Value[SBoolean.type], scriptBytes: Array[Byte]) extends ErgoAddress {
  override val addressTypePrefix: Byte = ScriptAddress.addressTypePrefix

  override def equals(obj: scala.Any): Boolean = obj match {
    case ScriptAddress(_, sb) => util.Arrays.equals(scriptBytes, sb)
    case _ => false
  }

  override def hashCode(): Int = Ints.fromByteArray(scriptBytes.takeRight(4))

  override def toString = s"P2S(${Algos.encode(scriptBytes)})"
}

object ScriptAddress {
  def apply(script: Value[SBoolean.type]): ScriptAddress = {
    val sb = ValueSerializer.serialize(script)
    ScriptAddress(script, sb)
  }

  val addressTypePrefix: Byte = 3: Byte
}


case class ErgoAddressEncoder(settings: ErgoSettings) {

  import ErgoAddressEncoder._

  val ChecksumLength = 4

  val networkPrefix = settings.chainSettings.addressPrefix

  def contentBytes(address: ErgoAddress): Array[Byte] = address match {
    case P2PKHAddress(addressHash) => addressHash
    case P2PKAddress(_, pubkeyBytes) => pubkeyBytes
    case ScriptHashAddress(scriptHash) => scriptHash
    case ScriptAddress(_, scriptBytes) => scriptBytes
  }

  def toString(address: ErgoAddress): String = {
    val withNetworkByte = (networkPrefix + address.addressTypePrefix).toByte +: contentBytes(address)

    val checksum = hash256(withNetworkByte).take(ChecksumLength)
    Base58.encode(withNetworkByte ++ checksum)
  }

  def fromString(addrStr: String): Try[ErgoAddress] = Base58.decode(addrStr).flatMap { bytes =>
    Try {
      val headByte = bytes.head
      require(headByte >= networkPrefix)
      val addressType = (headByte - networkPrefix).toByte
      val (withoutChecksum, checksum) = bytes.splitAt(bytes.length - ChecksumLength)

      if (!util.Arrays.equals(hash256(withoutChecksum).take(ChecksumLength), checksum))
        throw new Exception(s"Checksum check fails for $addrStr")

      val bs = withoutChecksum.tail

      addressType match {
        case P2PKHAddress.addressTypePrefix =>
          P2PKHAddress(bs)
        case P2PKAddress.addressTypePrefix =>
          val buf = ByteBuffer.wrap(bs)
          val r = new ByteBufferReader(buf).mark()
          val ge = DataSerializer.deserialize[SGroupElement.type](SGroupElement, r)
          val pd = ProveDlog(ge)
          P2PKAddress(pd, bs)
        case ScriptHashAddress.addressTypePrefix =>
          ScriptHashAddress(bs)
        case ScriptAddress.addressTypePrefix =>
          ScriptAddress(ValueSerializer.deserialize(bs).asInstanceOf[Value[SBoolean.type]], bs)
        case _ => throw new Exception("Unsupported address type: " + addressType)
      }
    }
  }
}

object ErgoAddressEncoder {
  def hash256(input: Array[Byte]) = Blake2b256(input)

  //todo: take Blake2b160 ?
  def hash160(input: Array[Byte]) = hash256(input).take(20)
}