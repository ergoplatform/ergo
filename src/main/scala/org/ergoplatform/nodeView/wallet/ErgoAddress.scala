package org.ergoplatform.nodeView.wallet

import java.nio.ByteBuffer
import java.util

import org.ergoplatform.settings.ErgoSettings
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
  * address type. An address type is showing a script used to protect a box. Possible prefixes and address types for
  * testnet:
  *
  * 7   - P2PKH (7aqf1Do41Vw4fEKYhbWwohQrtHoMRSVCZx)
  * 3NP - P2PK (3NPoGuXAhh1BqGXhdWpzi6kQQ7r8iPGZLL1J32eXPYPrLTM2w7aW)
  *     - SHA (8UmyuJuQ3FS9ts7j72fn3fKChXSGzbL9WC)
  *     - SA (imdaM2NzX)
  *
  * for mainnet:
  *
  *   - P2PKH (1Eh8BMYDG16PB4cAfUg3KQR3NSoJAYN3RG)
  *   - P2PK (17Xtw9ZTz1jE92UDq4j19jKi8fUkdzoaB4UsmnXYdJejzpZvX1F)
  *   - SHA (25qGdVWg2yyYho8uC1pLtc7KxFn4nEEAwD, 23NL9a8ngN28ovtLiKLgHexcdTKBbUMLhH)
  *   - SA (7bwdkU5V8)
  */
sealed trait ErgoAddress {
  val addressTypePrefix: Byte
}

object ErgoAddressTester extends App {
  val prover = new ErgoProvingInterpreter("6abb44c6f5")

  val pk = prover.dlogPubkeys.head

  val settings = ErgoSettings.read(None)

  val encoder = ErgoAddressEncoder(settings)

  println(encoder.toString(ScriptAddress(pk)))
}

case class P2PKHAddress(addressHash: Array[Byte]) extends ErgoAddress {
  override val addressTypePrefix: Byte = P2PKHAddress.addressTypePrefix
}

object P2PKHAddress {
  val addressTypePrefix: Byte = 0: Byte

  def apply(pubkey: ProveDlog): P2PKHAddress = {
    val bt = ValueSerializer.serialize(pubkey)
    P2PKHAddress(ErgoAddressEncoder.hash160(bt))
  }
}

case class P2PKAddress(pubkey: ProveDlog, pubkeyBytes: Array[Byte]) extends ErgoAddress {
  override val addressTypePrefix: Byte = P2PKHAddress.addressTypePrefix
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

  private def bodyBytes(address: ErgoAddress): Array[Byte] = address match {
    case P2PKHAddress(addressHash) => addressHash
    case P2PKAddress(_, pubkeyBytes) => pubkeyBytes
    case ScriptHashAddress(scriptHash) => scriptHash
    case ScriptAddress(_, scriptBytes) => scriptBytes
  }

  def definitiveBytes(address: ErgoAddress): Array[Byte] = bodyBytes(address)

  def toString(address: ErgoAddress): String = {
    val withNetworkByte = (networkPrefix + address.addressTypePrefix).toByte +: bodyBytes(address)

    val checksum = hash256(withNetworkByte).take(ChecksumLength)
    Base58.encode(withNetworkByte ++ checksum)
  }

  def fromString(addrStr: String): Try[ErgoAddress] = Base58.decode(addrStr).flatMap{ bytes =>
    Try {
      val headByte = bytes.head
      require(headByte >= networkPrefix)
      val addressType = headByte - networkPrefix
      val (bs, checksum) = bytes.splitAt(bytes.length - ChecksumLength)

      if(!util.Arrays.equals(hash256(bs), checksum)) throw new Exception(s"Checksum check fails for $addrStr")

      addressType match {
        case b: Int if b == P2PKHAddress.addressTypePrefix =>
          P2PKHAddress(bs)
        case b: Int if b == P2PKAddress.addressTypePrefix =>
          val buf = ByteBuffer.wrap(bytes)
          val r = new ByteBufferReader(buf).mark()
          val ge = DataSerializer.deserialize[SGroupElement.type](SGroupElement, r)
          val pd = ProveDlog(ge)
          P2PKAddress(pd, bs)
        case b: Int if b == ScriptHashAddress.addressTypePrefix =>
          ScriptHashAddress(bs)
        case b: Int if b == ScriptAddress.addressTypePrefix =>
          ScriptAddress(ValueSerializer.deserialize(bs).asInstanceOf[Value[SBoolean.type]], bs)
      }
    }
  }
}

object ErgoAddressEncoder {
  def hash256(input: Array[Byte]) = Blake2b256(input)

  //todo: take Blake2b160 ?
  def hash160(input: Array[Byte]) = hash256(input).take(20)
}