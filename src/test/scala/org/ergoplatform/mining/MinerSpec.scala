package org.ergoplatform.mining

import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.{ChainGenerator, ErgoGenerators}
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.core.NodeViewModifier._
import scorex.crypto.encode.Base58
import scorex.testkit.TestkitHelpers

class MinerSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators
  with TestkitHelpers
  with ChainGenerator {

  property("constructInterlinks() vector") {
    val parentId =  Base58.decode("6pwRCCgbGBson6JkCwCAW4C6AE3X3ZWLqeD3cX712dzr").get
    val oldId =  Base58.decode("4ahpTQ3cTbLDdCXbWuHuaazSbsPZS5LZ7RF6L8UFXaLb").get
    val genesisId = Base58.decode("3yHgzU5Q48EueocnGUgQu3sS24XcbTHi7VYMvveQUeze").get
    val parentInterlinks = List(genesisId, oldId, oldId)

    val calculated = Miner.constructInterlinks(parentInterlinks: Seq[Array[Byte]], BigInt(2), parentId)
    calculated.map(Base58.encode) shouldEqual Seq(genesisId, parentId, oldId).map(Base58.encode)
  }
}
