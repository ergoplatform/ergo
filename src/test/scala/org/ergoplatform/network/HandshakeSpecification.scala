package org.ergoplatform.network

import java.nio.ByteBuffer

import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.PeerFeatureIds
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.core.app.Version
import scorex.core.network.message.HandshakeSpec
import scorex.util.encode.Base16

class HandshakeSpecification extends ErgoPropertyTest {
  private val maxHandshakeSize = 10000

  def getUByte(buf: ByteBuffer): Int = getByte(buf) & 0xFF

  def getByte(buf: ByteBuffer): Byte = buf.get

  def getULong(buf: ByteBuffer): Long = {
    var result: Long = 0
    var shift = 0
    while (shift < 64) {
      val b = getByte(buf)
      result = result | ((b & 0x7F).toLong << shift)
      if ((b & 0x80) == 0) return result
      shift += 7
    }
    ???
  }

  def getBytes(buf: ByteBuffer, size: Int): Array[Byte] = {
    val res = new Array[Byte](size)
    buf.get(res)
    res
  }

  def decodeZigZagInt(n: Int): Int = {
    // source: http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/main/java/com/google/protobuf/CodedInputStream.java#L553
    (n >>> 1) ^ -(n & 1)
  }

  def getInt(buf: ByteBuffer): Int = {
    // should only be changed simultaneously with `putInt`
    decodeZigZagInt(getULong(buf).toInt)
  }

  property("handshake test vectors") {
    // bytes got from a real node
    val hsBase16 = "bcd2919cee2e076572676f726566030306126572676f2d6d61696e6e65742d332e332e36000210040001000102067f000001ae46"
    val hsBytes = Base16.decode(hsBase16).get

    val agentName = "ergoref"

    val peerName = "ergo-mainnet-3.3.6"

    val handshakeSerializer0 = new HandshakeSpec(Map.empty, maxHandshakeSize)
    val hs0 = handshakeSerializer0.parseBytes(hsBytes)
    hs0.time shouldBe 1610134874428L // Friday, 8 January 2021, 19:41:14
    hs0.peerSpec.protocolVersion shouldBe Version(3, 3, 6)
    hs0.peerSpec.agentName shouldBe agentName
    hs0.peerSpec.nodeName shouldBe peerName

    val handshakeSerializer1 = new HandshakeSpec(Map(PeerFeatureIds.ModeFeatureId -> ModeFeatureSerializer), maxHandshakeSize)
    val hs1 = handshakeSerializer1.parseBytes(hsBytes)
    hs1.time shouldBe 1610134874428L
    val mf = hs1.peerSpec.features.find(_.isInstanceOf[ModeFeature]).head.asInstanceOf[ModeFeature]
    mf.stateType shouldBe StateType.Utxo

    // Byte-by-byte parsing below, according to the spec https://github.com/ergoplatform/ergo/wiki/P2P-Handshaking

    val bb = ByteBuffer.wrap(hsBytes)
    val time = getULong(bb)
    time shouldBe 1610134874428L

    val agentNameLength = getUByte(bb)
    agentNameLength shouldBe agentName.length

    val agentNameParsed = getBytes(bb, agentNameLength)
    new String(agentNameParsed, "UTF-8") shouldBe agentName

    val version = getBytes(bb, 3)
    version(0) shouldBe 3
    version(1) shouldBe 3
    version(2) shouldBe 6

    val peerNameLength = getUByte(bb)
    peerNameLength shouldBe peerName.length

    val peerNameParsed = getBytes(bb, peerNameLength)
    new String(peerNameParsed, "UTF-8") shouldBe peerName

    val pubNode = getUByte(bb)

    pubNode shouldBe 0

    val featuresCount = getUByte(bb)

    featuresCount shouldBe 2

    val firstFeatureId = getUByte(bb)

    firstFeatureId shouldBe 16 //mode feature

    val firstFeatureLength = getULong(bb) //should read one byte only

    firstFeatureLength shouldBe 4

    val stateTypeCode = getUByte(bb).toByte

    val stateType = StateType.fromCode(stateTypeCode)

    stateType shouldBe StateType.Utxo

    val verifyTransactions = getUByte(bb).toByte

    verifyTransactions shouldBe 1 // true

    val nipopowSuffixLength = getUByte(bb).toByte

    nipopowSuffixLength shouldBe 0 // no nipopow suffix, the peer has full header-chain

    val blocksToKeep = getInt(bb)

    blocksToKeep shouldBe -1 // all the full blocks stored

    val secondFeatureId = getUByte(bb)

    secondFeatureId shouldBe 2 // local address feature id
  }

}
