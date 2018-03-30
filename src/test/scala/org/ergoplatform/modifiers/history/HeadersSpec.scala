package org.ergoplatform.modifiers.history

import org.ergoplatform.utils.{ChainGenerator, ErgoGenerators}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.hash.Blake2b256
import scorex.testkit.TestkitHelpers

class HeadersSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators
  with TestkitHelpers
  with ChainGenerator {

  val chain = genHeaderChain(50)
  val genesisId = chain.head.id

  property("Any field change should lead to different id") {
    forAll(invalidHeaderGen) { header =>
      val initialId = header.id
      header.copy(version = (header.version + 1).toByte).id should not equal initialId
      header.copy(parentId = initialId).id should not equal initialId
      header.copy(interlinks = header.interlinks ++ Seq(initialId)).id should not equal initialId
      header.copy(ADProofsRoot = Blake2b256(header.ADProofsRoot)).id should not equal initialId
      header.copy(transactionsRoot = Blake2b256(header.transactionsRoot)).id should not equal initialId
      header.copy(timestamp = header.timestamp + 1).id should not equal initialId
      header.copy(nBits = header.nBits + 1).id should not equal initialId
      header.copy(height = header.height + 1).id should not equal initialId
      header.copy(nonce = header.nonce + 1).id should not equal initialId
      header.copy(votes = Blake2b256(header.votes).take(header.votes.length)).id should not equal initialId
      header.copy(equihashSolutions = Array.fill(header.equihashSolutions.length)(11.toByte)).id should not equal initialId
    }
  }

  property("Header.interlinks.tail should not contain genesis id") {
    chain.headers.tail.foreach { r =>
      r.interlinks.tail.find(_ sameElements genesisId) shouldBe None
    }
  }

  property("Header Interlink(0) should always link to genesis block") {
    chain.headers.tail.foreach { r =>
      r.interlinks.head shouldEqual genesisId
    }
  }

  property("Header interlink length should not be less than parents") {
    chain.headers.sliding(2).foreach { r =>
      val block1 = r.head
      val block2 = r(1)
      block2.interlinks.length should be >= block1.interlinks.length
    }
  }
}
