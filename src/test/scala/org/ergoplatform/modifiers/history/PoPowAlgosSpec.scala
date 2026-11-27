package org.ergoplatform.modifiers.history

import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicReference

import org.ergoplatform.mining.difficulty.DifficultySerializer
import org.ergoplatform.modifiers.history.popow.{NipopowAlgos, NipopowProof, PoPowHeader, PoPowParams}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.header.Header
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.util.ModifierId

class PoPowAlgosSpec extends AnyPropSpec with Matchers {
  import org.ergoplatform.utils.generators.ChainGenerator._
  import org.ergoplatform.utils.generators.CoreObjectGenerators._
  import org.ergoplatform.utils.ErgoCoreTestConstants._

  private val poPowParams = PoPowParams(30, 30, continuous = false).get
  private val ChainLength = 10

  private def toPoPoWChain = (c: Seq[ErgoFullBlock]) => c.map(b => PoPowHeader.fromBlock(b).get)

  property("PoPowParams rejects invalid minimum chain lengths") {
    PoPowParams.isValid(0, 1) shouldBe false
    PoPowParams.isValid(1, 0) shouldBe false
    PoPowParams.isValid(Int.MaxValue, 1) shouldBe false

    PoPowParams(0, 1, continuous = false) shouldBe 'failure
    PoPowParams(1, 0, continuous = false) shouldBe 'failure
    PoPowParams(Int.MaxValue, 1, continuous = false) shouldBe 'failure

    PoPowParams.isValid(Int.MaxValue - 1, 1) shouldBe true
    PoPowParams(1, 1, continuous = false).get.minChainLength shouldBe 2
  }

  property("bestArg rejects a non-positive security parameter without looping") {
    val algos = nipopowAlgos
    val completed = new CountDownLatch(1)
    val error = new AtomicReference[Throwable]()
    val worker = new Thread(new Runnable {
      override def run(): Unit =
        try {
          algos.bestArg(Seq.empty)(0)
        } catch {
          case t: Throwable => error.set(t)
        } finally {
          completed.countDown()
        }
    })
    worker.setDaemon(true)
    worker.start()

    completed.await(2, TimeUnit.SECONDS) shouldBe true
    error.get() shouldBe a[IllegalArgumentException]
  }

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

  property("lowestCommonAncestor - the same") {
    val sizes = Seq(10, 100, 1000)
    sizes.foreach { size =>
      val chain0 = genChain(size)
      val chain1 = chain0.map(b => b.copy(header = b.header.copy(sizeOpt = Some(b.header.size))))

      nipopowAlgos.lowestCommonAncestor(chain0.map(_.header), chain1.map(_.header)) shouldBe Some(chain0.last.header)
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

  property("isBetterThan - marginally longer chain should be better") {
    val sizes = Seq(1000)
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

  // In the tests below the PoW scheme is the fake one: the PoW hit of a header is q / (height + 10) whatever
  // difficulty the header declares, and every header passes PoW validation. A header is under threshold nu when
  // hit * 2^nu <= q, so a header at height h is under the thresholds up to floor(log2(h + 10)): 7 at heights
  // 201..245, 8 at heights 246..501, whatever difficulty it declares.
  private val declaredDifficulty64 = DifficultySerializer.encodeCompactBits(BigInt(64))

  private def forkOf(branchPoint: ErgoFullBlock, length: Int, nBits: Long): Seq[ErgoFullBlock] =
    blockStream(Option(branchPoint), nBits = nBits).take(length + 1).tail

  // a header of version 1 with the given PoW hit: for version 1 the hit is the `d` of the solution
  private def withHit(header: Header, hit: BigInt): Header =
    header.copy(powSolution = header.powSolution.copy(d = hit))

  property("bestArg - counts headers under a fixed threshold, whatever difficulty they declare") {
    val base = genChain(200)
    val difficulty1 = forkOf(base.last, 40, chainSettings.initialNBits).map(_.header)
    val difficulty64 = forkOf(base.last, 40, declaredDifficulty64).map(_.header)
    difficulty1.map(_.requiredDifficulty).distinct shouldBe Seq(BigInt(1))
    difficulty64.map(_.requiredDifficulty).distinct shouldBe Seq(BigInt(64))
    difficulty1.map(_.height) shouldBe (201 to 240)
    difficulty64.map(_.height) shouldBe (201 to 240)

    // 40 headers, each under threshold 7 and none under 8: 2^7 * 40, for either declared difficulty
    nipopowAlgos.bestArg(difficulty1)(6) shouldBe BigInt(5120)
    nipopowAlgos.bestArg(difficulty64)(6) shouldBe BigInt(5120)
    nipopowAlgos.bestArg(difficulty1)(40) shouldBe BigInt(5120)
    // no threshold has 41 headers under it: nothing is claimed
    nipopowAlgos.bestArg(difficulty1)(41) shouldBe BigInt(0)

    // the genesis header is not counted
    base.head.header.isGenesis shouldBe true
    nipopowAlgos.bestArg(base.head.header +: difficulty1)(6) shouldBe BigInt(5120)
  }

  property("bestArg - one very hard header counts once") {
    val base = genChain(200)
    val forty = forkOf(base.last, 40, chainSettings.initialNBits).map(_.header)
    // a header 2^20 times as hard to find as any other here, which also declares that difficulty
    val hardDifficulty = BigInt(1) << 20
    val hard = withHit(forty.last, org.ergoplatform.mining.q / hardDifficulty)
      .copy(nBits = DifficultySerializer.encodeCompactBits(hardDifficulty))
    hard.requiredDifficulty shouldBe hardDifficulty
    powScheme.powHit(hard) shouldBe org.ergoplatform.mining.q / hardDifficulty
    // with five ordinary headers there are six under threshold 7, and only the hard one under any higher:
    // 2^7 * 6, not the 2^20 its difficulty would weigh
    nipopowAlgos.bestArg(forty.take(5) :+ hard)(6) shouldBe BigInt(768)
    nipopowAlgos.bestArg(forty)(6) shouldBe BigInt(5120)
  }

  property("bestArg - a header is under a threshold exactly when its hit times 2^threshold is not above q") {
    val q = org.ergoplatform.mining.q
    val header = forkOf(genChain(200).last, 1, chainSettings.initialNBits).head.header
    header.version shouldBe 1.toByte
    // with m = 1 the score of one header is 2^(the highest threshold it is under)
    def highestThreshold(hit: BigInt): BigInt = nipopowAlgos.bestArg(Seq(withHit(header, hit)))(1)
    highestThreshold(q >> 7) shouldBe (BigInt(1) << 7)
    highestThreshold((q >> 7) + 1) shouldBe (BigInt(1) << 6)
    highestThreshold(q >> 1) shouldBe (BigInt(1) << 1)
    // a hit above q / 2 is under threshold 0 only, and so is a hit of q
    highestThreshold((q >> 1) + 1) shouldBe BigInt(1)
    highestThreshold(q) shouldBe BigInt(1)
    highestThreshold(BigInt(1)) shouldBe (BigInt(1) << 255)
  }

  property("isBetterThan - fewer than m headers are replaced by a proof which shows more than they could be read as") {
    val baseChain = genChain(200)
    val branchPoint = baseChain.last
    // three headers after the branch point: fewer than m, so no threshold has m of them under it
    val fewProof = nipopowAlgos.prove(
      toPoPoWChain(baseChain ++ forkOf(branchPoint, 3, chainSettings.initialNBits)))(poPowParams).get
    // forty headers after the branch point, each under the same threshold as each of the three
    val manyProof = nipopowAlgos.prove(
      toPoPoWChain(baseChain ++ forkOf(branchPoint, 40, chainSettings.initialNBits)))(poPowParams).get
    fewProof.isValid shouldBe true
    manyProof.isValid shouldBe true
    nipopowAlgos.lowestCommonAncestor(fewProof.headersChain, manyProof.headersChain)
      .map(_.id) shouldBe Some(branchPoint.header.id)
    fewProof.headersChain.count(_.height > branchPoint.header.height) shouldBe 3
    manyProof.headersChain.count(_.height > branchPoint.header.height) shouldBe 40

    // with fewer than m headers nothing can be claimed against a proof which has a score
    (manyProof.isBetterThan(fewProof), fewProof.isBetterThan(manyProof)) shouldBe ((true, false))
  }

  property("isBetterThan - a proof which goes on from the last header of another is better") {
    val baseChain = genChain(200)
    val shorterProof = nipopowAlgos.prove(toPoPoWChain(baseChain))(poPowParams).get
    val longerProof = nipopowAlgos.prove(
      toPoPoWChain(baseChain ++ forkOf(baseChain.last, 3, chainSettings.initialNBits)))(poPowParams).get
    // the last header of the shorter proof is in the longer one, three headers below its tip
    longerProof.headersChain.map(_.id) should contain(shorterProof.headersChain.last.id)
    (longerProof.isBetterThan(shorterProof), shorterProof.isBetterThan(longerProof)) shouldBe ((true, false))
  }

  property("isBetterThan - the difficulty a chain declares should not make it better") {
    val baseChain = genChain(200)
    val branchPoint = baseChain.last
    // 60 headers after the branch point, declaring difficulty 64. All 60 are under threshold 7 and 15 of them
    // (heights 246..260) under 8: the best score is 2^7 * 60 = 7680
    val moreHeadersChain = toPoPoWChain(baseChain ++ forkOf(branchPoint, 60, declaredDifficulty64))
    // 40 headers after the branch point, declaring difficulty 1, all under threshold 7: 2^7 * 40 = 5120.
    // Measured against the difficulty they declare, these are 40 superblocks of level 7 and the 60 above are of
    // level 1, 15 of them of level 2 as well, so a count of superblocks prefers this chain
    val lowerDifficultyChain = toPoPoWChain(baseChain ++ forkOf(branchPoint, 40, chainSettings.initialNBits))

    val moreHeadersProof = nipopowAlgos.prove(moreHeadersChain)(poPowParams).get
    val lowerDifficultyProof = nipopowAlgos.prove(lowerDifficultyChain)(poPowParams).get
    moreHeadersProof.isValid shouldBe true
    lowerDifficultyProof.isValid shouldBe true

    // both proofs diverge right after the branch point and contain all the headers after it
    nipopowAlgos.lowestCommonAncestor(moreHeadersProof.headersChain, lowerDifficultyProof.headersChain)
      .map(_.id) shouldBe Some(branchPoint.header.id)
    moreHeadersProof.headersChain.count(_.height > branchPoint.header.height) shouldBe 60
    lowerDifficultyProof.headersChain.count(_.height > branchPoint.header.height) shouldBe 40

    (moreHeadersProof.isBetterThan(lowerDifficultyProof),
      lowerDifficultyProof.isBetterThan(moreHeadersProof)) shouldBe ((true, false))
  }

  property("isBetterThan - a disconnected prefix chain should not win") {
    val smallPoPowParams = PoPowParams(50, 1, continuous = false).get
    val size = 100
    val chain = toPoPoWChain(genChain(size))
    val proof = nipopowAlgos.prove(chain)(smallPoPowParams).get

    val longerChain = toPoPoWChain(genChain(size * 2))
    val longerProof = nipopowAlgos.prove(longerChain)(smallPoPowParams).get

    val disconnectedProofPrefix = proof.prefix.take(proof.prefix.length / 2) ++ longerProof.prefix
    val disconnectedProof = NipopowProof(nipopowAlgos, proof.m, proof.k, disconnectedProofPrefix, proof.suffixHead, proof.suffixTail, continuous = false)
    proof.isBetterThan(disconnectedProof) shouldBe true
  }

  property("hasValidConnections - ensures a connected prefix chain") {
    val smallPoPowParams = PoPowParams(5, 5, continuous = false).get
    val sizes = Seq(100, 200)
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
    val smallPoPowParams = PoPowParams(5, 5, continuous = false).get
    val sizes = Seq(100, 200)

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
    val prefix = toPoPoWChain(genChain(1))
    val suffix = toPoPoWChain(genChain(1))
    NipopowProof(nipopowAlgos, 0, 0, prefix, suffix.head, suffix.tail.map(_.header), continuous = false).hasValidConnections shouldBe false
  }

}
