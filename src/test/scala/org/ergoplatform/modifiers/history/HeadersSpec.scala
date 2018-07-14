package org.ergoplatform.modifiers.history

import org.ergoplatform.mining.EquihashSolution
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.crypto.hash.Blake2b256

class HeadersSpec extends ErgoPropertyTest {

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
      header.copy(extensionHash = Blake2b256(header.extensionHash)).id should not equal initialId
      header.copy(equihashSolution = EquihashSolution(Seq.fill(header.equihashSolution.ints.length)(0x0B0B0B0B))).id should not equal initialId
    }
  }

  property("Header.interlinks.tail should not contain genesis id") {
    chain.headers.tail.foreach { r =>
      r.interlinks.tail.find(_ == genesisId) shouldBe None
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
