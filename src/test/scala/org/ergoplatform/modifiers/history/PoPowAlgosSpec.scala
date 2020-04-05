package org.ergoplatform.modifiers.history

import Extension.InterlinksVectorPrefix
import org.ergoplatform.utils.generators.{ChainGenerator, ErgoGenerators}
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import scorex.util.{ModifierId, bytesToId}

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

    interlinks.zipWithIndex.foreach { case (links, idx) =>
      if (idx > 0) links.size >= interlinks(idx - 1).size shouldBe true
    }
  }

  property("packInterlinks") {
    val diffInterlinks = Gen.listOfN(255, modifierIdGen).sample.get
    val modId = modifierIdGen.sample.get
    val sameInterlinks = List.fill(255)(modId)
    val packedDiff = PoPowAlgos.interlinksToExtension(diffInterlinks).fields
    val packedSame = PoPowAlgos.interlinksToExtension(sameInterlinks).fields

    packedDiff.map(_._1.last).toSet.size shouldEqual diffInterlinks.size
    packedSame.map(_._1.last).toSet.size shouldEqual 1
  }

  property("unpackInterlinks") {
    val interlinks = Gen.listOfN(255, modifierIdGen).sample.get
    val packed = PoPowAlgos.interlinksToExtension(interlinks).fields
    val improperlyPacked = packed.map(x => x._1 -> (x._2 :+ (127: Byte)))

    val unpackedTry = unpackInterlinks(packed)

    unpackedTry shouldBe 'success
    unpackInterlinks(improperlyPacked) shouldBe 'failure

    unpackedTry.get shouldEqual interlinks
  }

  property("proofForInterlink") {
    val blockIds = Gen.listOfN(255, modifierIdGen).sample.get
    val extension = PoPowAlgos.interlinksToExtension(blockIds)
    val blockIdToProve = blockIds.head
    val proof = proofForInterlink(extension, blockIdToProve)

    proof shouldBe defined
    val encodedField = proof.get.leafData
    val numBytesKey = encodedField.head
    val key = encodedField.tail.take(numBytesKey)
    val prefix = key.head
    val value = encodedField.drop(numBytesKey + 1)
    val blockId = value.tail
    numBytesKey shouldBe 2
    prefix shouldBe InterlinksVectorPrefix
    bytesToId(blockId) shouldBe blockIdToProve
    proof.get.valid(extension.digest) shouldBe true
  }

}
