package org.ergoplatform.modifiers.history

import org.ergoplatform.utils.{ChainGenerator, ErgoGenerators}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.testkit.TestkitHelpers

class HeadersSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators
  with TestkitHelpers
  with ChainGenerator {

  val chain = genHeaderChain(50, Seq(), defaultDifficultyControl)
  val genesisId = chain.head.id

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
