package org.ergoplatform.modifiers.history

import org.ergoplatform.modifiers.history.popow.{NipopowAlgos, NipopowProof, PoPowHeader, PoPowParams}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.generators.ChainGenerator
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.util.ModifierId
import org.ergoplatform.utils.HistoryTestHelpers

class PoPowAlgosSpec extends AnyPropSpec with Matchers with HistoryTestHelpers with ChainGenerator {

  private val poPowParams = PoPowParams(30, 30, continuous = false)
  private val ChainLength = 10

  property("updateInterlinks") {
    val chain = genChain(ChainLength)
    val genesis = chain.head
    val interlinks = chain.foldLeft(Seq.empty[Seq[ModifierId]]) { case (acc, b) =>
      acc :+ (if (acc.isEmpty) {
        nipopowAlgos.updateInterlinks(b.header, Seq.empty)
      } else {
        nipopowAlgos.updateInterlinks(b.header, acc.last)
      })
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
    val packedDiff = nipopowAlgos.interlinksToExtension(diffInterlinks).fields
    val packedSame = nipopowAlgos.interlinksToExtension(sameInterlinks).fields

    packedDiff.map(_._1.last).toSet.size shouldEqual diffInterlinks.size
    packedSame.map(_._1.last).toSet.size shouldEqual 1
  }

  property("unpackInterlinks") {
    val interlinks = Gen.listOfN(255, modifierIdGen).sample.get
    val packed = nipopowAlgos.interlinksToExtension(interlinks).fields
    val improperlyPacked = packed.map(x => x._1 -> (x._2 :+ (127: Byte)))

    val unpackedTry = NipopowAlgos.unpackInterlinks(packed)

    unpackedTry shouldBe 'success
    NipopowAlgos.unpackInterlinks(improperlyPacked) shouldBe 'failure

    unpackedTry.get shouldEqual interlinks
  }

  property("proofForInterlinkVector") {
    val blockIds = Gen.listOfN(255, modifierIdGen).sample.get
    val extension = nipopowAlgos.interlinksToExtension(blockIds)
    val proof = NipopowAlgos.proofForInterlinkVector(extension)
    proof.get.valid(extension.digest) shouldBe true
  }

  property("empty proofForInterlinkVector should be None") {
    val blockIds = Seq.empty[ModifierId]
    val extension = nipopowAlgos.interlinksToExtension(blockIds)
    val proof = NipopowAlgos.proofForInterlinkVector(extension)
    proof shouldBe defined
    proof.get.proofs should have length 0
    proof.get.indices should have length 0
  }

  property("0 level is always valid for any block") {
    val chain = genChain(10)
    chain.foreach(x => nipopowAlgos.maxLevelOf(x.header) >= 0 shouldBe true)
  }

  property("lowestCommonAncestor - diverging") {
    val sizes = Seq(10, 100, 1000)
    sizes.foreach { size =>
      val chain0 = genChain(size)
      val branchPoint = chain0(size / 2)
      val chain1 = chain0.take(size / 2) ++ genChain(size / 2, branchPoint)

      nipopowAlgos.lowestCommonAncestor(chain0.map(_.header), chain1.map(_.header)) shouldBe Some(branchPoint.header)
    }
  }

  property("bestArg - always equal for equal proofs") {
    val chain0 = genChain(100).map(b => PoPowHeader.fromBlock(b).get)
    val proof0 = nipopowAlgos.prove(chain0)(poPowParams).get
    val chain1 = genChain(100).map(b => PoPowHeader.fromBlock(b).get)
    val proof1 = nipopowAlgos.prove(chain1)(poPowParams).get
    val m = poPowParams.m

    proof0.prefix.size shouldEqual proof1.prefix.size

    nipopowAlgos.bestArg(proof0.prefix.map(_.header))(m) shouldEqual nipopowAlgos.bestArg(proof1.prefix.map(_.header))(m)
  }

  property("bestArg - always greater for better proof") {
    val chain0 = genChain(100).map(b => PoPowHeader.fromBlock(b).get)
    val proof0 = nipopowAlgos.prove(chain0)(poPowParams).get
    val chain1 = genChain(70).map(b => PoPowHeader.fromBlock(b).get)
    val proof1 = nipopowAlgos.prove(chain1)(poPowParams).get
    val m = poPowParams.m

    proof0.prefix.size > proof1.prefix.size shouldBe true

    nipopowAlgos.bestArg(proof0.prefix.map(_.header))(m) > nipopowAlgos.bestArg(proof1.prefix.map(_.header))(m) shouldBe true
  }

  property("proof(chain) is equivalent to proof(histReader)") {
    val poPowParams = PoPowParams(m = 5, k = 6, continuous = false)
    val blocksChain = genChain(3000)
    val pchain = blocksChain.map(b => PoPowHeader.fromBlock(b).get)
    val proof0 = nipopowAlgos.prove(pchain)(poPowParams).get

    val h = generateHistory(true, StateType.Digest, false,
      10000, 10000, 10, None)
    val hr = applyChain(h, blocksChain)
    val proof1 = nipopowAlgos.prove(hr)(poPowParams).get

    proof0.suffixHead.id shouldBe proof1.suffixHead.id
    proof0.suffixTail.map(_.id) shouldBe proof1.suffixTail.map(_.id)

    proof0.prefix.map(_.id).length shouldBe proof1.prefix.map(_.id).length
    proof0.prefix.map(_.id).toList shouldBe proof1.prefix.map(_.id).toList
  }

  property("proof(histReader) for a header in the past") {
    val poPowParams = PoPowParams(5, 6, continuous = false)
    val blocksChain = genChain(300)

    val at = 200

    val h = generateHistory(true, StateType.Digest, false,
      10000, 10000, 10, None)
    val hr = applyChain(h, blocksChain.take(at))
    val proof0 = nipopowAlgos.prove(hr, None)(poPowParams).get

    val id = proof0.suffixHead.header.id

    val hrf = applyChain(hr, blocksChain.drop(at))
    val proof1 = nipopowAlgos.prove(hrf, Some(id))(poPowParams).get

    proof0.suffixHead.id shouldBe proof1.suffixHead.id
    proof0.suffixTail.map(_.id) shouldBe proof1.suffixTail.map(_.id)

    proof0.prefix.map(_.id).length shouldBe proof1.prefix.map(_.id).length
    proof0.prefix.map(_.id).sorted.toList shouldBe proof1.prefix.map(_.id).sorted.toList
  }

  property("isBetterThan - marginally longer chain should be better") {
    val sizes = Seq(1000)
    val toPoPoWChain = (c: Seq[ErgoFullBlock]) => c.map(b => PoPowHeader.fromBlock(b).get)
    sizes.foreach { size =>
      val baseChain = genChain(size)
      val branchPoint = baseChain(baseChain.length - 1)
      val shortChain = toPoPoWChain(baseChain)
      val longChain = toPoPoWChain(baseChain ++ genChain(1, branchPoint).takeRight(1))

      val shortProof = nipopowAlgos.prove(shortChain)(poPowParams).get
      val longProof = nipopowAlgos.prove(longChain)(poPowParams).get

      shortProof.isBetterThan(longProof) shouldBe false
    }
  }

  property("isBetterThan - a disconnected prefix chain should not win") {
    val smallPoPowParams = PoPowParams(50, 1, continuous = false)
    val size = 100
    val toPoPoWChain = (c: Seq[ErgoFullBlock]) => c.map(b => PoPowHeader.fromBlock(b).get)
    val chain = toPoPoWChain(genChain(size))
    val proof = nipopowAlgos.prove(chain)(smallPoPowParams).get

    val longerChain = toPoPoWChain(genChain(size * 2))
    val longerProof = nipopowAlgos.prove(longerChain)(smallPoPowParams).get

    val disconnectedProofPrefix = proof.prefix.take(proof.prefix.length / 2) ++ longerProof.prefix
    val disconnectedProof = NipopowProof(nipopowAlgos, proof.m, proof.k, disconnectedProofPrefix, proof.suffixHead, proof.suffixTail, continuous = false)
    proof.isBetterThan(disconnectedProof) shouldBe true
  }

  property("hasValidConnections - ensures a connected prefix chain") {
    val smallPoPowParams = PoPowParams(5, 5, continuous = false)
    val sizes = Seq(100, 200)
    val toPoPoWChain = (c: Seq[ErgoFullBlock]) => c.map(b => PoPowHeader.fromBlock(b).get)
    sizes.foreach { size =>
      val chain = toPoPoWChain(genChain(size))
      val randomBlock = toPoPoWChain(genChain(1)).head
      val proof = nipopowAlgos.prove(chain)(smallPoPowParams).get
      val disconnectedProofPrefix = proof.prefix.updated(proof.prefix.length / 2, randomBlock)
      val disconnectedProof = NipopowProof(nipopowAlgos, proof.m, proof.k, disconnectedProofPrefix, proof.suffixHead, proof.suffixTail, continuous = false)
      proof.hasValidConnections shouldBe true
      disconnectedProof.hasValidConnections shouldBe false
    }
  }

  property("hasValidConnections - ensures a connected suffix chain") {
    val smallPoPowParams = PoPowParams(5, 5, continuous = false)
    val sizes = Seq(100, 200)
    val toPoPoWChain = (c: Seq[ErgoFullBlock]) => c.map(b => PoPowHeader.fromBlock(b).get)
    sizes.foreach { size =>
      val chain = toPoPoWChain(genChain(size))
      val randomBlock = genChain(1).head.header
      val proof = nipopowAlgos.prove(chain)(smallPoPowParams).get
      val disconnectedProofSuffixTail = proof.suffixTail.updated(proof.suffixTail.length / 2, randomBlock)
      val disconnectedProof = NipopowProof(nipopowAlgos, proof.m, proof.k, proof.prefix, proof.suffixHead, disconnectedProofSuffixTail, continuous = false)
      proof.hasValidConnections shouldBe true
      disconnectedProof.hasValidConnections shouldBe false
    }
  }

  property("hasValidConnections - ensures prefix.last & suffix.head are linked") {
    val toPoPoWChain = (c: Seq[ErgoFullBlock]) => c.map(b => PoPowHeader.fromBlock(b).get)
    val prefix = toPoPoWChain(genChain(1))
    val suffix = toPoPoWChain(genChain(1))
    NipopowProof(nipopowAlgos, 0, 0, prefix, suffix.head, suffix.tail.map(_.header), continuous = false).hasValidConnections shouldBe false
  }

}
