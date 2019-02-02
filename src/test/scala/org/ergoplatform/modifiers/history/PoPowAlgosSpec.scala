package org.ergoplatform.modifiers.history

import org.ergoplatform.utils.generators.{ChainGenerator, ErgoGenerators}
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import scorex.util.ModifierId

class PoPowAlgosSpec extends PropSpec with Matchers with ChainGenerator with ErgoGenerators {

  import PoPowAlgos._

  private val ChainLength = 10

  property("updateInterlinks") {
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

  property("packInterlinks") {
    val diffInterlinks = Gen.listOfN(255, modifierIdGen).sample.get
    val modId = modifierIdGen.sample.get
    val sameInterlinks = List.fill(255)(modId)
    val packedDiff = PoPowAlgos.packInterlinks(diffInterlinks)
    val packedSame = PoPowAlgos.packInterlinks(sameInterlinks)

    packedDiff.map(_._1.last).toSet.size shouldEqual diffInterlinks.size
    packedSame.map(_._1.last).toSet.size shouldEqual 1
  }

}
