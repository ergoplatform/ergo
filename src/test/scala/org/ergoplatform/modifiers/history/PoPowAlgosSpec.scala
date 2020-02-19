package org.ergoplatform.modifiers.history

import org.ergoplatform.modifiers.history.popow.{PoPowProof, PoPowHeader, PoPowParams}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.utils.generators.{ChainGenerator, ErgoGenerators}
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.util.ModifierId

class PoPowAlgosSpec
  extends PropSpec
    with Matchers
    with HistoryTestHelpers
    with ChainGenerator
    with ErgoGenerators
    with GeneratorDrivenPropertyChecks {

  import org.ergoplatform.modifiers.history.popow.PoPowAlgos
  import PoPowAlgos._

  private val poPowParams = PoPowParams(30, 30)
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

  property("0 level is always valid for any block") {
    val chain = genChain(10)
    chain.foreach(x => maxLevelOf(x.header) >= 0 shouldBe true)
  }

  property("lowestCommonAncestor - diverging") {
    val sizes = Seq(10, 100, 1000)
    sizes.foreach { size =>
      val chain0 = genChain(size)
      val branchPoint = chain0(size / 2)
      val chain1 = chain0.take(size / 2) ++ genChain(size / 2, branchPoint)

      lowestCommonAncestor(chain0.map(_.header), chain1.map(_.header)) shouldBe Some(branchPoint.header)
    }
  }

  property("bestArg - always equal for equal proofs") {
    val chain0 = genChain(100).map(b => PoPowHeader(b.header, unpackInterlinks(b.extension.fields).get))
    val proof0 = prove(chain0)(poPowParams)
    val chain1 = genChain(100).map(b => PoPowHeader(b.header, unpackInterlinks(b.extension.fields).get))
    val proof1 = prove(chain1)(poPowParams)
    val m = poPowParams.m

    proof0.prefix.size shouldEqual proof1.prefix.size

    bestArg(proof0.prefix.map(_.header))(m) shouldEqual bestArg(proof1.prefix.map(_.header))(m)
  }

  property("bestArg - always greater for better proof") {
    val chain0 = genChain(100).map(b => PoPowHeader(b.header, unpackInterlinks(b.extension.fields).get))
    val proof0 = prove(chain0)(poPowParams)
    val chain1 = genChain(70).map(b => PoPowHeader(b.header, unpackInterlinks(b.extension.fields).get))
    val proof1 = prove(chain1)(poPowParams)
    val m = poPowParams.m

    proof0.prefix.size > proof1.prefix.size shouldBe true

    bestArg(proof0.prefix.map(_.header))(m) > bestArg(proof1.prefix.map(_.header))(m) shouldBe true
  }

  property("proof(chain) is equivalent to proof(histReader)") {
    val poPowParams = PoPowParams(5, 6)
    val blocksChain = genChain(300)
    val pchain = blocksChain.map(b => PoPowHeader(b.header, unpackInterlinks(b.extension.fields).get))
    val proof0 = prove(pchain)(poPowParams)

    val h = generateHistory(true, StateType.Digest, false,
      10000, 10000, 10, None)
    val hr = applyChain(h, blocksChain)
    val proof1 = prove(hr)(poPowParams)

    proof0.suffixHead.id shouldBe proof1.suffixHead.id
    proof0.suffixTail.map(_.id) shouldBe proof1.suffixTail.map(_.id)

    proof0.prefix.map(_.id).length shouldBe proof1.prefix.map(_.id).length
    proof0.prefix.map(_.id).sorted.toList shouldBe proof1.prefix.map(_.id).sorted.toList
  }

  property("proof(histReader) for a header in the past") {
    val poPowParams = PoPowParams(5, 6)
    val blocksChain = genChain(300)

    val at = 200

    val h = generateHistory(true, StateType.Digest, false,
      10000, 10000, 10, None)
    val hr = applyChain(h, blocksChain.take(at))
    val proof0 = prove(hr, None)(poPowParams)

    val id = proof0.suffixHead.header.id

    val hrf = applyChain(hr, blocksChain.drop(at))
    val proof1 = prove(hrf, Some(id))(poPowParams)


    proof0.suffixHead.id shouldBe proof1.suffixHead.id
    proof0.suffixTail.map(_.id) shouldBe proof1.suffixTail.map(_.id)

    proof0.prefix.map(_.id).length shouldBe proof1.prefix.map(_.id).length
    proof0.prefix.map(_.id).sorted.toList shouldBe proof1.prefix.map(_.id).sorted.toList
  }

  property("isBetterThan - marginally longer chain should be better") {
    val sizes = Seq(1000)
    val toPoPoWChain = (c: Seq[ErgoFullBlock]) =>
      c.map(b => PoPowHeader(b.header, unpackInterlinks(b.extension.fields).get))
    sizes.foreach { size =>
      val baseChain = genChain(size)
      val branchPoint = baseChain(baseChain.length - 1)
      val shortChain = toPoPoWChain(baseChain)
      val longChain = toPoPoWChain(baseChain ++ genChain(1, branchPoint).takeRight(1))

      val shortProof = prove(shortChain)(poPowParams)
      val longProof = prove(longChain)(poPowParams)

      shortProof.isBetterThan(longProof) shouldBe false
    }
  }

  property("isBetterThan - a disconnected prefix chain should not win") {
    val smallPoPowParams = PoPowParams(50, 1)
    val size = 100
    val toPoPoWChain = (c: Seq[ErgoFullBlock]) =>
      c.map(b => PoPowHeader(b.header, unpackInterlinks(b.extension.fields).get))
    val chain = toPoPoWChain(genChain(size))
    val proof = prove(chain)(smallPoPowParams)

    val longerChain = toPoPoWChain(genChain(size*2))
    val longerProof = prove(longerChain)(smallPoPowParams)

    val disconnectedProofPrefix = proof.prefix.take(proof.prefix.length/2) ++ longerProof.prefix
    val disconnectedProof = PoPowProof(proof.m, proof.k, disconnectedProofPrefix, proof.suffix)
    proof.isBetterThan(disconnectedProof) shouldBe true
  }


  property("isConnected - ensures a connected prefix chain") {
    val smallPoPowParams = PoPowParams(5, 5)
    val sizes = Seq(100, 200)
    val toPoPoWChain = (c: Seq[ErgoFullBlock]) =>
      c.map(b => PoPowHeader(b.header, unpackInterlinks(b.extension.fields).get))
    sizes.foreach { size =>
      val chain = toPoPoWChain(genChain(size))
      val randomBlock = toPoPoWChain(genChain(1)).head
      val proof = prove(chain)(smallPoPowParams)
      val disconnectedProofPrefix = proof.prefix.updated(proof.prefix.length/2, randomBlock)
      val disconnectedProof = PoPowProof(proof.m, proof.k, disconnectedProofPrefix, proof.suffix)
      proof.isConnected() shouldBe true
      disconnectedProof.isConnected() shouldBe false
    }
  }

  property("isConnected - ensures a connected suffix chain") {
    val smallPoPowParams = PoPowParams(5, 5)
    val sizes = Seq(100, 200)
    val toPoPoWChain = (c: Seq[ErgoFullBlock]) =>
      c.map(b => PoPowHeader(b.header, unpackInterlinks(b.extension.fields).get))
    sizes.foreach { size =>
      val chain = toPoPoWChain(genChain(size))
      val randomBlock = toPoPoWChain(genChain(1)).head
      val proof = prove(chain)(smallPoPowParams)
      val disconnectedProofSuffix = proof.suffix.updated(proof.suffix.length/2, randomBlock)
      val disconnectedProof = PoPowProof(proof.m, proof.k, proof.prefix, disconnectedProofSuffix)
      proof.isConnected() shouldBe true
      disconnectedProof.isConnected() shouldBe false
    }
  }

  property("isConnected - ensures prefix.last & suffix.head are linked") {
    val toPoPoWChain = (c: Seq[ErgoFullBlock]) =>
      c.map(b => PoPowHeader(b.header, unpackInterlinks(b.extension.fields).get))
    val prefix = toPoPoWChain(genChain(1))
    val suffix = toPoPoWChain(genChain(1))
    PoPowProof(0, 0, prefix, suffix).isConnected() shouldBe false
  }
}
