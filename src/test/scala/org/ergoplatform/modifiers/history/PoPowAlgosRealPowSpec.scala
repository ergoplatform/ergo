package org.ergoplatform.modifiers.history

import org.ergoplatform.local.NipopowVerifier
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.mining.difficulty.DifficultySerializer
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.popow.{NipopowAlgos, NipopowProof, PoPowHeader, PoPowParams}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.crypto.hash.Digest32

/**
  * Comparison of NiPoPoW proofs over headers mined with the real Autolykos PoW scheme: a header is valid only
  * if its PoW hit is below the target it declares, and what a header counts for comes from that hit.
  * Headers are of version 2 and mined with a fixed key and fixed timestamps, so the chains are the same in every run.
  */
class PoPowAlgosRealPowSpec extends AnyPropSpec with Matchers {
  import org.ergoplatform.utils.ErgoCoreTestConstants._

  private val realPow = new AutolykosPowScheme(powScheme.k, powScheme.n)
  private val algos = new NipopowAlgos(chainSettings.copy(powScheme = realPow))
  private val poPowParams = PoPowParams(6, 10, continuous = false).get
  private val emptyDigest = Digest32 @@ Array.fill(32)(0: Byte)
  private val votes = Array.fill(3)(0: Byte)
  private val firstTimestamp = 1600000000000L
  private val blockInterval = 120000L

  // mines a header which commits, through its extension root, to the interlinks it carries in a proof
  private def mine(prev: Option[PoPowHeader], difficulty: Int): PoPowHeader = {
    val interlinks = prev.toSeq.flatMap(p => algos.updateInterlinks(p.header, p.interlinks))
    val extension = algos.interlinksToExtension(interlinks)
    val height = prev.map(_.header.height + 1).getOrElse(1)
    val header = realPow.prove(prev.map(_.header), Header.HardeningVersion,
      DifficultySerializer.encodeCompactBits(BigInt(difficulty)), EmptyStateRoot, emptyDigest, emptyDigest,
      firstTimestamp + height * blockInterval, extension.digest, votes, defaultMinerSecretNumber).get
    PoPowHeader(header, interlinks, NipopowAlgos.proofForInterlinkVector(extension).get)
  }

  private def mineChain(prev: Option[PoPowHeader], length: Int, difficulty: Int): Seq[PoPowHeader] =
    Iterator.iterate(mine(prev, difficulty))(h => mine(Some(h), difficulty)).take(length).toVector

  property("isBetterThan - the proof of the chain which took more work should be better") {
    val shared = mineChain(None, 40, difficulty = 1)
    // after the shared part: 40 blocks of difficulty 64 on one chain, 120 blocks of difficulty 1 on another
    val moreWorkProof = algos.prove(shared ++ mineChain(shared.lastOption, 40, difficulty = 64))(poPowParams).get
    val moreBlocksProof = algos.prove(shared ++ mineChain(shared.lastOption, 120, difficulty = 1))(poPowParams).get
    moreWorkProof.isValid shouldBe true
    moreBlocksProof.isValid shouldBe true

    val lastCommon = algos.lowestCommonAncestor(moreWorkProof.headersChain, moreBlocksProof.headersChain).get
    def difficultyAfterLastCommon(proof: NipopowProof): BigInt =
      proof.headersChain.filter(_.height > lastCommon.height).map(_.requiredDifficulty).sum
    difficultyAfterLastCommon(moreWorkProof) should be > difficultyAfterLastCommon(moreBlocksProof)

    (moreWorkProof.isBetterThan(moreBlocksProof), moreBlocksProof.isBetterThan(moreWorkProof)) shouldBe ((true, false))
  }

  property("isBetterThan - a few blocks which took much work should not be replaced by more blocks which took little") {
    val shared = mineChain(None, 40, difficulty = 1)
    // after the shared part: 3 blocks of difficulty 64 on one chain, which is fewer than m; 9 blocks of
    // difficulty 1 on another, about a twentieth of the work. Both forks are within the suffix of their proof
    val fewHardProof = algos.prove(shared ++ mineChain(shared.lastOption, 3, difficulty = 64))(poPowParams).get
    val moreEasyProof = algos.prove(shared ++ mineChain(shared.lastOption, 9, difficulty = 1))(poPowParams).get
    fewHardProof.isValid shouldBe true
    moreEasyProof.isValid shouldBe true

    val lastCommon = algos.lowestCommonAncestor(fewHardProof.headersChain, moreEasyProof.headersChain).get
    lastCommon.id shouldBe shared.last.header.id
    fewHardProof.headersChain.count(_.height > lastCommon.height) shouldBe 3
    moreEasyProof.headersChain.count(_.height > lastCommon.height) shouldBe 9

    // the nine do not replace the three, and the three, too few to be weighed against a score, do not replace the nine
    (moreEasyProof.isBetterThan(fewHardProof), fewHardProof.isBetterThan(moreEasyProof)) shouldBe ((false, false))
  }

  property("isBetterThan - of two sides with fewer than m headers each, the one which shows more work should be better") {
    val shared = mineChain(None, 40, difficulty = 1)
    // after the shared part: 1 block of difficulty 1 on one chain, 5 blocks of difficulty 64 on another.
    // Both sides are fewer than m, so neither has a score
    val oneEasyProof = algos.prove(shared ++ mineChain(shared.lastOption, 1, difficulty = 1))(poPowParams).get
    val fiveHardProof = algos.prove(shared ++ mineChain(shared.lastOption, 5, difficulty = 64))(poPowParams).get
    oneEasyProof.isValid shouldBe true
    fiveHardProof.isValid shouldBe true

    val lastCommon = algos.lowestCommonAncestor(oneEasyProof.headersChain, fiveHardProof.headersChain).get
    lastCommon.id shouldBe shared.last.header.id
    oneEasyProof.headersChain.count(_.height > lastCommon.height) shouldBe 1
    fiveHardProof.headersChain.count(_.height > lastCommon.height) shouldBe 5

    (fiveHardProof.isBetterThan(oneEasyProof), oneEasyProof.isBetterThan(fiveHardProof)) shouldBe ((true, false))
  }

  property("isBetterThan - of two sides with fewer than m headers each, more headers should not be better for their number") {
    val shared = mineChain(None, 40, difficulty = 1)
    // after the shared part: 2 blocks of difficulty 64 on one chain, 5 blocks of difficulty 1 on another
    val twoHardProof = algos.prove(shared ++ mineChain(shared.lastOption, 2, difficulty = 64))(poPowParams).get
    val fiveEasyProof = algos.prove(shared ++ mineChain(shared.lastOption, 5, difficulty = 1))(poPowParams).get
    twoHardProof.isValid shouldBe true
    fiveEasyProof.isValid shouldBe true

    val lastCommon = algos.lowestCommonAncestor(twoHardProof.headersChain, fiveEasyProof.headersChain).get
    lastCommon.id shouldBe shared.last.header.id
    twoHardProof.headersChain.count(_.height > lastCommon.height) shouldBe 2
    fiveEasyProof.headersChain.count(_.height > lastCommon.height) shouldBe 5

    (twoHardProof.isBetterThan(fiveEasyProof), fiveEasyProof.isBetterThan(twoHardProof)) shouldBe ((true, false))
  }

  property("NipopowVerifier - the proof kept should not depend on which of two short sides arrived first") {
    val shared = mineChain(None, 40, difficulty = 1)
    val sharedProof = algos.prove(shared)(poPowParams).get
    val oneEasyProof = algos.prove(shared ++ mineChain(shared.lastOption, 1, difficulty = 1))(poPowParams).get
    val fiveHardProof = algos.prove(shared ++ mineChain(shared.lastOption, 5, difficulty = 64))(poPowParams).get

    // both go on from the last header of the shared proof, so each replaces it
    def keptAfter(proofs: NipopowProof*): Header = {
      val verifier = new NipopowVerifier(Some(shared.head.header.id))
      proofs.foreach(verifier.process)
      verifier.bestChain.last
    }
    keptAfter(sharedProof, oneEasyProof).id shouldBe oneEasyProof.headersChain.last.id
    keptAfter(sharedProof, oneEasyProof, fiveHardProof).id shouldBe fiveHardProof.headersChain.last.id
    keptAfter(sharedProof, fiveHardProof, oneEasyProof).id shouldBe fiveHardProof.headersChain.last.id
  }

}
