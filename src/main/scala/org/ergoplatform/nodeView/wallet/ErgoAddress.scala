package org.ergoplatform.nodeView.wallet

import scapi.sigma.DLogProtocol.ProveDlog
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256
import sigmastate.SBoolean
import sigmastate.Values.Value
import sigmastate.serialization.ValueSerializer

object ErgoAddress {

  val MainnetByte = 0: Byte
  val TestnetByte = 16: Byte

  //todo: put in configs which network we're running
  val NetworkByte = TestnetByte

  val P2PKH = 0: Byte
  val P2PK = 1: Byte
  val P2SH = 2: Byte
  val P2S = 3: Byte

  def hash256(input: Array[Byte]) = Blake2b256(input)

  //todo: take Blake2b160 ?
  def hash160(input: Array[Byte]) = hash256(input).take(20)

  //todo: reduce boilerplate below
  def p2pkh(pubkey: ProveDlog): String = {
    val bt = ValueSerializer.serialize(pubkey)
    val bth160 = hash160(bt)

    //add network identifier
    val withNetworkByte = (NetworkByte + P2PKH).toByte +: bth160

    val checksum = hash256(withNetworkByte).take(4)
    Base58.encode(withNetworkByte ++ checksum)
  }

  def p2pk(pubkey: ProveDlog): String = {
    val bt = ValueSerializer.serialize(pubkey)

    //add network identifier
    val withNetworkByte = (NetworkByte + P2PK).toByte +: bt

    val checksum = hash256(withNetworkByte).take(4)
    Base58.encode(withNetworkByte ++ checksum)
  }

  def p2sh(script: Value[SBoolean.type]): String = {
    val bt = ValueSerializer.serialize(script)
    val bth160 = hash160(bt)

    //add network identifier
    val withNetworkByte = (NetworkByte + P2SH).toByte +: bth160

    val checksum = hash256(withNetworkByte).take(4)
    Base58.encode(withNetworkByte ++ checksum)
  }


  def p2s(script: Value[SBoolean.type]): String = {
    val bt = ValueSerializer.serialize(script)

    //add network identifier
    val withNetworkByte = (NetworkByte + P2S).toByte +: bt

    val checksum = hash256(withNetworkByte).take(4)
    Base58.encode(withNetworkByte ++ checksum)
  }
}
