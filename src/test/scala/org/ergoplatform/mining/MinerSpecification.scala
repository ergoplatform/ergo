package org.ergoplatform.mining

import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.ADProof
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.settings.Constants
import org.ergoplatform.utils.{ErgoGenerators, NoShrink}
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.crypto.encode.Base58

class MinerSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators
  with NoShrink {

  private val n = 48: Char
  private val k = 5: Char

  private def createValidBlock: ErgoFullBlock = {
    Miner.genBlock(
      RequiredDifficulty.encodeCompactBits(Constants.InitialDifficulty),
      None, Array.emptyByteArray, ADProof(Array(1.toByte), Array(1.toByte)),
      Seq(AnyoneCanSpendTransaction(IndexedSeq.empty, IndexedSeq(10L))), 1L, Array.emptyByteArray, n, k)
  }

  property("constructInterlinks() vector") {
    val parentId = Base58.decode("6pwRCCgbGBson6JkCwCAW4C6AE3X3ZWLqeD3cX712dzr").get
    val oldId = Base58.decode("4ahpTQ3cTbLDdCXbWuHuaazSbsPZS5LZ7RF6L8UFXaLb").get
    val genesisId = Base58.decode("3yHgzU5Q48EueocnGUgQu3sS24XcbTHi7VYMvveQUeze").get
    val parentInterlinks = List(genesisId, oldId, oldId)

    val calculated = Miner.constructInterlinks(parentInterlinks: Seq[Array[Byte]], BigInt(2), parentId)
    calculated.map(Base58.encode) shouldEqual Seq(genesisId, parentId, oldId).map(Base58.encode)
  }

  //todo: for Tolsi: make real prop test, what are accepted n & k values?
  property("Miner should generate valid block") {
   // forAll(Gen.choose(2, 100), Gen.choose(3, 5)) {case (nV, kV) =>
        val b = createValidBlock
        Miner.isValidBlock(b, n.toChar, k.toChar) shouldBe true
   // }
  }

  property("Valid block should be invalid by pow after equihash solutions modification") {
    val b = createValidBlock
    val invB = b.header.equihashSolutions.clone()
    invB(0) = 1
    Miner.isValidBlock(b.copy(header = b.header.copy(equihashSolutions = invB)), n, k) shouldBe false
  }

  property("Valid block should be invalid by pow after height modification") {
    val b = createValidBlock
    assert(!Miner.isValidBlock(b.copy(header = b.header.copy(height = 3)), n, k))
  }

  property("Valid block should be invalid by pow after AD proofs root modification") {
    val b = createValidBlock
    Miner.isValidBlock(b.copy(header = b.header.copy(ADProofsRoot = Array.emptyByteArray)), n, k) shouldBe false
  }
}