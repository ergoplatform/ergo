package org.ergoplatform.mining

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.state.{ErgoStateContext, VotingData}
import org.ergoplatform.settings.Constants
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.ergoplatform.ErgoBox
import scorex.util.{ModifierId, bytesToId}
import sigma.Colls
import sigma.ast.{ErgoTree, ShortConstant}
import sigma.data.{Digest32Coll, ProveDlog}
import sigmastate.helpers.TestingHelpers._

/**
  * Pins `StorageRentClaimBuilder` against the real consensus validator: every claim the
  * builder produces must pass `statefulValidity` through the storage-rent interpreter path,
  * and every box the builder must skip must produce no claim.
  */
class StorageRentClaimBuilderSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators._

  private implicit val verifier: ErgoInterpreter = ErgoInterpreter(parameters)

  private val minerPk: ProveDlog = defaultMinerPk
  private val MinerTree: ErgoTree = ErgoTree.fromSigmaBoolean(minerPk)

  /** Height of the block being assembled in tests. */
  private val H: Int = 3 * Constants.StoragePeriod

  /** Box old enough to be rent-eligible, grace period not yet passed. */
  private def agedBox(value: Long, withToken: Boolean = false): ErgoBox =
    tokenizedBox(value, Constants.StoragePeriod, withToken)

  private def tokenizedBox(value: Long, age: Int, withToken: Boolean): ErgoBox = {
    val tokens = if (withToken) {
      Seq((Digest32Coll @@ Colls.fromArray(Array.fill(32)(7.toByte))) -> 5L)
    } else {
      Seq.empty
    }
    testBox(value, Constants.TrueTree, H - age, tokens, Map.empty)
  }

  private val WhitelistedTokenId: ModifierId = bytesToId(Array.fill(32)(7.toByte))

  private def minValueOf(box: ErgoBox): Long = parameters.minValuePerByte.toLong * box.bytes.length

  /** A box whose value is just below the protocol minimum for its size (VLQ fixpoint). */
  private def belowMinBox(age: Int, withToken: Boolean): ErgoBox = {
    var b = tokenizedBox(10000000000L, age, withToken)
    while (b.value >= minValueOf(b)) {
      b = tokenizedBox(minValueOf(b) - 1, age, withToken)
    }
    b
  }

  private def buildAndValidate(boxes: Seq[ErgoBox],
                               reemissionTokenId: Option[ModifierId] = None,
                               whitelist: Set[ModifierId] = Set.empty): Option[ErgoTransaction] = {
    val txOpt = StorageRentClaimBuilder.buildClaim(boxes, H, parameters, minerPk, reemissionTokenId, whitelist)
    txOpt.foreach { tx =>
      val fb0 = invalidErgoFullBlockGen.sample.get
      val fakeHeader = fb0.header.copy(height = H - 1)
      val fb = fb0.copy(fb0.header.copy(height = H, parentId = fakeHeader.id))
      val updContext = {
        val inContext = new ErgoStateContext(Seq(fakeHeader), None, genesisStateDigest, parameters,
          validationSettingsNoIl, VotingData.empty)(settings.chainSettings)
        inContext.appendFullBlock(fb).get
      }
      withClue("built claim must pass consensus validation: ") {
        tx.statefulValidity(boxes.filter(b => tx.inputs.map(_.boxId).contains(b.id)).toIndexedSeq,
          emptyDataBoxes, updContext).isSuccess shouldBe true
      }
    }
    txOpt
  }

  property("recreate box builds a valid zero-fee claim paying the fee to the miner P2PK") {
    val b = agedBox(10000000000L)
    val tx = buildAndValidate(Seq(b)).get

    tx.inputs.length shouldBe 1
    tx.outputCandidates.length shouldBe 2 // recreated + aggregate P2PK

    val storageFee = parameters.storageFeeFactor * b.bytes.length
    val recreated = tx.outputCandidates.head
    recreated.value shouldBe b.value - storageFee
    recreated.ergoTree shouldBe b.ergoTree
    recreated.creationHeight shouldBe H

    val proceeds = tx.outputCandidates.last
    proceeds.value shouldBe storageFee
    proceeds.ergoTree shouldBe MinerTree

    // zero fee: outputs balance inputs exactly
    tx.outputCandidates.map(_.value).sum shouldBe b.value
  }

  property("token box with value above the fee is refreshed towards the minimum only as much as the fee") {
    val b = agedBox(10000000000L, withToken = true)
    val tx = buildAndValidate(Seq(b)).get
    val recreated = tx.outputCandidates.head
    recreated.additionalTokens.length shouldBe 1 // tokens preserved
    recreated.value shouldBe b.value - parameters.storageFeeFactor * b.bytes.length
  }

  property("box with value between minimum and fee is refreshed towards the minimum") {
    // grace period: the charge stops at the minimum allowed value instead of seizing the box
    val probe = agedBox(10000000000L, withToken = true)
    val minV = minValueOf(probe)
    val fee = parameters.storageFeeFactor.toLong * probe.bytes.length
    val value = minV + fee / 2 // minV < value < fee
    val b = tokenizedBox(value, Constants.StoragePeriod, withToken = true)

    val tx = buildAndValidate(Seq(b)).get
    val recreated = tx.outputCandidates.head
    recreated.value shouldBe minValueOf(b)
    tx.outputCandidates.last.value shouldBe value - minValueOf(b)
    tx.outputCandidates.map(_.value).sum shouldBe value
  }

  property("box below the minimum is burned only after the grace period, tokens destroyed") {
    val b = belowMinBox(Constants.StoragePeriod + StorageRentClaimBuilder.StorageGracePeriod, withToken = true)
    val tx = buildAndValidate(Seq(b)).get

    tx.inputs.length shouldBe 1
    tx.outputCandidates.length shouldBe 1 // only the aggregate P2PK
    val proceeds = tx.outputCandidates.head
    proceeds.value shouldBe b.value
    proceeds.ergoTree shouldBe MinerTree
    proceeds.additionalTokens.length shouldBe 0 // tokens burned with the box
    // the full-consume input names the aggregate output in var #127
    tx.inputs.head.spendingProof.extension.values(Constants.StorageIndexVarId) shouldBe ShortConstant(0)
  }

  property("box below the minimum is not touched during the grace period") {
    val b = belowMinBox(Constants.StoragePeriod, withToken = true) // expired but grace period not over
    buildAndValidate(Seq(b)) shouldBe None
  }

  property("box with a whitelisted token is never burned") {
    val b = belowMinBox(Constants.StoragePeriod + StorageRentClaimBuilder.StorageGracePeriod, withToken = true)
    // whitelisted: skipped entirely, left for sponsors
    buildAndValidate(Seq(b), whitelist = Set(WhitelistedTokenId)) shouldBe None
    // not whitelisted: burned after the grace period
    buildAndValidate(Seq(b), whitelist = Set.empty).isDefined shouldBe true
  }

  property("too young box is skipped") {
    val b = tokenizedBox(10000000000L, Constants.StoragePeriod - 1, withToken = false)
    buildAndValidate(Seq(b)) shouldBe None
  }

  property("fee-overflow box is skipped") {
    // box big enough that storageFeeFactor * bytes wraps Int negative is consensus-uncollectable
    val tokens = (0 until 70).map(i =>
      (Digest32Coll @@ Colls.fromArray(Array.fill(32)(i.toByte))) -> 1L)
    val big = testBox(10000000000L, Constants.TrueTree, 0, tokens, Map.empty)
    parameters.storageFeeFactor * big.bytes.length should be < 0 // sanity: fee wraps negative
    buildAndValidate(Seq(big)) shouldBe None
  }

  property("reemission-token box is skipped on EIP-27 networks") {
    val reemissionId = bytesToId(Array.fill(32)(9.toByte))
    val b = testBox(10000000000L, Constants.TrueTree, 0,
      Seq((Digest32Coll @@ Colls.fromArray(Array.fill(32)(9.toByte))) -> 5L), Map.empty)
    buildAndValidate(Seq(b), reemissionTokenId = Some(reemissionId)) shouldBe None
    // the same box is an ordinary claimable box on networks without EIP-27
    buildAndValidate(Seq(b), reemissionTokenId = None).isDefined shouldBe true
  }

  property("mixed branches validate and index outputs correctly") {
    val recreateBox = agedBox(10000000000L)
    val consumeBox = belowMinBox(Constants.StoragePeriod + StorageRentClaimBuilder.StorageGracePeriod, withToken = false)
    val tx = buildAndValidate(Seq(recreateBox, consumeBox)).get

    tx.inputs.length shouldBe 2
    tx.outputCandidates.length shouldBe 2 // recreated + aggregate

    // recreate input names its recreated output (index 0), full-consume names the aggregate (1)
    tx.inputs(0).spendingProof.extension.values(Constants.StorageIndexVarId) shouldBe ShortConstant(0)
    tx.inputs(1).spendingProof.extension.values(Constants.StorageIndexVarId) shouldBe ShortConstant(1)
  }

  property("rent proceeds go to the canonical miner P2PK, not the delayed reward script") {
    val b = agedBox(10000000000L)
    val tx = buildAndValidate(Seq(b)).get
    tx.outputCandidates.last.ergoTree shouldBe ErgoTree.fromSigmaBoolean(minerPk)
  }

  property("claim inputs carry empty proofs with var #127") {
    val b = agedBox(10000000000L)
    val tx = buildAndValidate(Seq(b)).get
    tx.inputs.foreach { in =>
      in.spendingProof.proof.isEmpty shouldBe true
      in.spendingProof.extension.values.contains(Constants.StorageIndexVarId) shouldBe true
    }
  }

  property("no eligible boxes produce no claim") {
    buildAndValidate(Seq.empty) shouldBe None
  }
}
