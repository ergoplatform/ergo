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

  val chain = genHeaderChain(50, Seq())

  property("Header Interlink(0) should always link to genesis block") {
    chain.headers.tail.foreach { r =>
      r.interlinks(0) shouldEqual chain.head.id
    }
  }

  property("Header Interlink(1) should always link to previous block") {
    chain.headers.sliding(2).foreach { r =>
      val block1 = r.head
      val block2 = r(1)
      block2.parentId shouldEqual block1.id
      block2.interlinks(1) shouldEqual block1.id
    }
  }

}
