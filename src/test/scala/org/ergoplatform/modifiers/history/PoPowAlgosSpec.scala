package org.ergoplatform.modifiers.history

import org.ergoplatform.utils.generators.ChainGenerator
import org.scalatest.{FlatSpec, Matchers}
import scorex.util.ModifierId

class PoPowAlgosSpec extends FlatSpec with Matchers with ChainGenerator {

  import PoPowAlgos._

  private val ChainLength = 10

  it should "calculate correct interlinks" in {
    val chain = genChain(ChainLength)
    val genesis = chain.head
    val interlinks = chain.foldLeft(Seq.empty[Seq[ModifierId]]) { case (acc, b) =>
      acc :+ (if (acc.isEmpty) updateInterlinks(b.header, Seq.empty) else updateInterlinks(b.header, acc.last))
    }

    interlinks.foreach { links =>
      links.head shouldEqual genesis.header.id
      links.tail should not contain genesis.header.id
    }
  }

}
