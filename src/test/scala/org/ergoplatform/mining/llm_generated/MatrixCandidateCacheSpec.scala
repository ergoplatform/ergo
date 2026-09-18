package org.ergoplatform.mining

import org.ergoplatform.mining.CandidateGenerator.Candidate
import org.ergoplatform.modifiers.history.extension.ExtensionCandidate
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoCoreTestConstants.{defaultMinerPk, emptyStateContext, parameters}
import org.ergoplatform.utils.ErgoNodeTestConstants.settings
import org.ergoplatform.utils.generators.ErgoCoreGenerators.proveDlogGen
import org.ergoplatform.utils.generators.ValidBlocksGenerators.{createUtxoState, validFullBlock}
import scorex.util.bytesToId
import scorex.crypto.authds.LeafData
import scorex.crypto.hash.Digest32

class MatrixCandidateCacheSpec extends ErgoCorePropertyTest {
  private lazy val candidate: Candidate = {
    val state = createUtxoState(settings)._1
    val txs = CandidateGenerator.collectEmission(state, defaultMinerPk, emptyStateContext).toSeq
    val block = validFullBlock(None, state, txs)
    val header = block.header
    val candidateBlock = CandidateBlock(None, header.version, header.nBits, header.stateRoot,
      block.adProofs.get.proofBytes, txs, header.timestamp, ExtensionCandidate(block.extension.fields),
      header.votes, InputBlockFields.empty, Seq.empty, txs)
    Candidate(candidateBlock, settings.chainSettings.powScheme.deriveExternalCandidate(candidateBlock, defaultMinerPk),
      Seq.empty, parameters)
  }

  private def withInputTip(bytes: Array[Byte]): Candidate = {
    val fields = candidate.candidateBlock.inputBlockFields
    candidate.copy(candidateBlock = candidate.candidateBlock.copy(inputBlockFields =
      new InputBlockFields(Some(bytes), fields.transactionsDigest, Algos.emptyMerkleTreeRoot,
        fields.inputBlockFieldsProof)))
  }

  property("an absent candidate is never a cache hit") {
    CandidateGenerator.cachedFor(None, Seq.empty, defaultMinerPk, None) shouldBe false
  }

  property("a legacy candidate without an input tip remains a cache hit") {
    CandidateGenerator.cachedFor(Some(candidate), Seq.empty, defaultMinerPk, None) shouldBe true
  }

  property("the same selected input ID matches independently allocated bytes") {
    val stored = Array.fill[Byte](32)(1)
    val selected = stored.clone()
    (stored eq selected) shouldBe false
    CandidateGenerator.cachedFor(Some(withInputTip(stored)), Seq.empty, defaultMinerPk,
      Some(bytesToId(selected))) shouldBe true
  }

  property("selection of a first remote input tip invalidates a no-input candidate") {
    CandidateGenerator.cachedFor(Some(candidate), Seq.empty, defaultMinerPk,
      Some(bytesToId(Array.fill[Byte](32)(1)))) shouldBe false
  }

  property("selection of a different input fork invalidates the previous candidate") {
    CandidateGenerator.cachedFor(Some(withInputTip(Array.fill[Byte](32)(1))), Seq.empty, defaultMinerPk,
      Some(bytesToId(Array.fill[Byte](32)(2)))) shouldBe false
  }

  property("clearing the selected input tip invalidates an input-linked candidate") {
    CandidateGenerator.cachedFor(Some(withInputTip(Array.fill[Byte](32)(1))), Seq.empty,
      defaultMinerPk, None) shouldBe false
  }

  property("processing new transaction bodies invalidates a candidate with the same input tip") {
    val tip = Array.fill[Byte](32)(1)
    val advancedDigest = Algos.merkleTreeRoot(candidate.candidateBlock.transactions.map(tx =>
      LeafData @@ tx.serializedId))
    advancedDigest.toSeq should not be Algos.emptyMerkleTreeRoot.toSeq
    CandidateGenerator.cachedFor(Some(withInputTip(tip)), Seq.empty, defaultMinerPk,
      Some(bytesToId(tip)), advancedDigest) shouldBe false
  }

  property("equal processed-prefix digests match independently allocated bytes") {
    val tip = Array.fill[Byte](32)(1)
    val cached = withInputTip(tip)
    val stored = cached.candidateBlock.inputBlockFields.prevTransactionsDigest
    val selected = Digest32 @@ stored.clone()
    (stored.asInstanceOf[AnyRef] eq selected.asInstanceOf[AnyRef]) shouldBe false
    CandidateGenerator.cachedFor(Some(cached), Seq.empty, defaultMinerPk,
      Some(bytesToId(tip)), selected) shouldBe true
  }

  property("the selected input key does not bypass the miner public key") {
    val differentPk = proveDlogGen.sample.get
    differentPk should not be defaultMinerPk
    CandidateGenerator.cachedFor(Some(candidate), Seq.empty, differentPk, None) shouldBe false
  }

  property("matching explicitly requested transactions remain a cache hit") {
    val requested = candidate.candidateBlock.transactions
    requested should not be empty
    val explicitCandidate = candidate.copy(txsToInclude = requested)
    CandidateGenerator.cachedFor(Some(explicitCandidate), requested, defaultMinerPk, None) shouldBe true
  }

  property("a new explicit transaction request still invalidates an empty-request cache key") {
    val requested = candidate.candidateBlock.transactions
    requested should not be empty
    CandidateGenerator.cachedFor(Some(candidate), requested, defaultMinerPk, None) shouldBe false
  }
}
