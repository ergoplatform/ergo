package org.ergoplatform.mining

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.state.{ErgoStateContext, VotingData}
import org.ergoplatform.settings.{Constants, ValidationRules}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.ergoplatform.ErgoBox
import scorex.util.{ModifierId, bytesToId}
import sigma.Colls
import sigma.ast.{ErgoTree, ShortConstant}
import sigma.Extensions.CollBytesOps
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

  /** Token id built from a single byte, matching the ids used by `tokenizedBox`. */
  private def tokenIdOf(b: Byte): ModifierId = bytesToId(Array.fill(32)(b))

  /** One token entry (quantity 5) for the id built from byte `b`. */
  private def tokenEntry(b: Byte): (Digest32Coll, Long) =
    (Digest32Coll @@ Colls.fromArray(Array.fill(32)(b))) -> 5L

  /** Box carrying one entry of quantity 5 for every id in `tokenIds`. */
  private def boxWithTokens(value: Long, age: Int, tokenIds: Seq[Byte]): ErgoBox =
    testBox(value, Constants.TrueTree, H - age, tokenIds.map(tokenEntry), Map.empty)

  /**
    * A box that can not be charged any more (its value is below the protocol minimum, so
    * it is a burn candidate) and carries `tokenIds`.
    */
  private def burnCandidateWithTokens(tokenIds: Seq[Byte]): ErgoBox = {
    val age = Constants.StoragePeriod + StorageRentClaimBuilder.StorageGracePeriod
    var b = boxWithTokens(10000000000L, age, tokenIds)
    while (b.value >= minValueOf(b)) {
      b = boxWithTokens(minValueOf(b) - 1, age, tokenIds)
    }
    b
  }

  private val WhitelistedTokenId: ModifierId = tokenIdOf(7.toByte)

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
    tx.outputCandidates.length shouldBe 2 // recreated + one proceeds output

    // recreate input names its recreated output (index 0), full-consume names the proceeds (1)
    tx.inputs(0).spendingProof.extension.values(Constants.StorageIndexVarId) shouldBe ShortConstant(0)
    tx.inputs(1).spendingProof.extension.values(Constants.StorageIndexVarId) shouldBe ShortConstant(1)

    // zero fee: outputs balance inputs exactly
    tx.outputCandidates.map(_.value).sum shouldBe recreateBox.value + consumeBox.value
  }

  property("multiple burned boxes get distinct outputs and validate") {
    val b1 = belowMinBox(Constants.StoragePeriod + StorageRentClaimBuilder.StorageGracePeriod, withToken = true)
    val b2 = belowMinBox(Constants.StoragePeriod + StorageRentClaimBuilder.StorageGracePeriod, withToken = true)
    val tx = buildAndValidate(Seq(b1, b2)).get

    tx.inputs.length shouldBe 2
    tx.outputCandidates.length shouldBe 2 // one P2PK output per burned box, tokens burned
    tx.inputs(0).spendingProof.extension.values(Constants.StorageIndexVarId) shouldBe ShortConstant(0)
    tx.inputs(1).spendingProof.extension.values(Constants.StorageIndexVarId) shouldBe ShortConstant(1)
    tx.outputCandidates.foreach(_.additionalTokens.length shouldBe 0)
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

  /**
    * Height at which the duplicate-var-127 consensus rule `txRentDistinctOutputs`
    * (ValidationRules #125) starts rejecting claims whose inputs share a recreated
    * output index.
    */
  private val RentDistinctOutputsActivation: Int =
    ValidationRules.StorageRentDistinctOutputsActivationHeight

  /**
    * The var #127 output index of every input of `tx`, as `Int`. Read exactly the way
    * `ErgoInterpreter` reads it (`extension.values(varId).value.asInstanceOf[Short]`), so
    * these tests observe the same value the consensus rule and the interpreter do.
    */
  private def var127Indices(tx: ErgoTransaction): IndexedSeq[Int] =
    tx.inputs.map(_.spendingProof.extension.values(Constants.StorageIndexVarId)
      .value.asInstanceOf[Short].toInt)

  /**
    * Boxes past the storage period + grace period, i.e. claimable by the full-consume
    * (burn) branch. The age is varied per box so that every box (and thus every input) is
    * distinct - a claim with duplicate inputs would be rejected by `txInputsUnique`
    * instead, masking the var #127 assertions.
    */
  private def burnBoxAt(i: Int): ErgoBox = {
    val age = Constants.StoragePeriod + StorageRentClaimBuilder.StorageGracePeriod + i
    belowMinBox(age, withToken = true)
  }

  property("the test height is at or above the duplicate var #127 activation height") {
    // Guards the whole spec: `buildAndValidate` runs `statefulValidity`, which only
    // enforces `txRentDistinctOutputs` while the context height is at or above the
    // activation height. Lowering H (or moving the activation height up) would silently
    // switch off the duplicate-var-127 coverage asserted below.
    H should be >= RentDistinctOutputsActivation
  }

  property("a claim over many recreated and burned boxes keeps var #127 distinct") {
    // distinct values => distinct box ids
    val recreateBoxes = (0 until 25).map(i => agedBox(10000000000L + i))
    val burnBoxes = (0 until 15).map(burnBoxAt)
    val tx = buildAndValidate(recreateBoxes ++ burnBoxes).get

    tx.inputs.length shouldBe recreateBoxes.length + burnBoxes.length

    // exactly the predicate the consensus rule txRentDistinctOutputs asserts
    val indices = var127Indices(tx)
    withClue("var #127 values must be pairwise distinct: ") {
      indices.distinct.length shouldBe indices.length
    }
  }

  property("every var #127 value names an existing output of the claim") {
    // The interpreter resolves var #127 with `outputCandidates(idx)` directly; an
    // out-of-range index throws, falls back to signature verification, claim rejected.
    val recreateBoxes = (0 until 12).map(i => agedBox(10000000000L + i))
    val burnBoxes = (0 until 8).map(burnBoxAt)
    val tx = buildAndValidate(recreateBoxes ++ burnBoxes).get

    val indices = var127Indices(tx)
    val nOutputs = tx.outputCandidates.length
    withClue(s"var #127 values $indices must address outputs 0..${nOutputs - 1}: ") {
      indices.foreach { idx =>
        idx should be >= 0
        idx should be < tx.outputCandidates.length
      }
    }
  }

  property("recreated inputs name recreated outputs, burned inputs name proceeds") {
    val recreateBoxes = (0 until 5).map(i => agedBox(10000000000L + i))
    val burnBoxes = (0 until 3).map(burnBoxAt)
    val tx = buildAndValidate(recreateBoxes ++ burnBoxes).get

    val nRecreated = recreateBoxes.length
    val indices = var127Indices(tx)

    // recreated boxes are the first `nRecreated` outputs, proceeds come after: the two
    // branches occupy disjoint index ranges, which is what keeps the values distinct
    indices.take(nRecreated) should contain theSameElementsAs (0 until nRecreated)
    indices.drop(nRecreated) should contain theSameElementsAs
      (nRecreated until tx.outputCandidates.length)
  }

  property("claims stay valid when burned boxes are dropped for lack of clean proceeds") {
    // Burning splits the aggregate proceeds evenly, one P2PK output per burned box. When
    // the proceeds cannot give every burned box a dust-clean output the builder drops the
    // smallest burned boxes; the survivors must still name pairwise distinct outputs.
    val burnBoxes = (0 until 20).map { i =>
      val age = Constants.StoragePeriod + StorageRentClaimBuilder.StorageGracePeriod + i
      belowMinBox(age, withToken = false)
    }
    buildAndValidate(burnBoxes).foreach { tx =>
      val indices = var127Indices(tx)
      withClue("surviving var #127 values must stay pairwise distinct: ") {
        indices.distinct.length shouldBe indices.length
      }
      indices.foreach { idx => idx should be < tx.outputCandidates.length }
      // one proceeds output per surviving burned box; the ones dropped for dust are
      // absent from both the inputs and the outputs, so the two stay aligned
      indices.length shouldBe tx.outputCandidates.length
      indices should contain theSameElementsAs (0 until tx.outputCandidates.length)
    }
  }

  /** Every token id present in any output of `tx`. */
  private def outputTokenIds(tx: ErgoTransaction): Set[ModifierId] =
    tx.outputCandidates.flatMap(_.additionalTokens.toArray.map(_._1.toModifierId)).toSet

  property("a burned box destroys its non-whitelisted tokens") {
    // Full consume spends the box with an empty proof, so nothing carries its tokens
    // over: the tokens leave the UTXO with the box and are burnt with it.
    val token = 11.toByte
    val b = burnCandidateWithTokens(Seq(token))
    b.tokens.keySet should contain(tokenIdOf(token)) // sanity: box carries the token

    val tx = buildAndValidate(Seq(b)).get

    tx.inputs.map(_.boxId) should contain(b.id)
    withClue("no token may survive a full-consume burn: ") {
      outputTokenIds(tx) shouldBe empty
    }
  }

  property("every token of a burned box is burnt, not only the first one") {
    val tokens = Seq(11.toByte, 12.toByte, 13.toByte)
    val b = burnCandidateWithTokens(tokens)
    tokens.foreach(t => b.tokens.keySet should contain(tokenIdOf(t)))

    val tx = buildAndValidate(Seq(b)).get
    outputTokenIds(tx) shouldBe empty
  }

  property("a mixed claim burns burned tokens and preserves recreated ones") {
    val burnToken = 11.toByte
    val recreateToken = 12.toByte
    val burnBox = burnCandidateWithTokens(Seq(burnToken))
    val recreateBox = testBox(10000000000L, Constants.TrueTree,
      H - Constants.StoragePeriod, Seq(tokenEntry(recreateToken)), Map.empty)

    val tx = buildAndValidate(Seq(burnBox, recreateBox)).get

    // the burned box's token is gone; the recreated box keeps its own token
    outputTokenIds(tx) shouldBe Set(tokenIdOf(recreateToken))
  }

  property("a whitelisted token keeps the whole box, and its tokens, out of the claim") {
    // Burning destroys every token of the box, so a box carrying a whitelisted token must
    // be left untouched rather than partially burned.
    val b = burnCandidateWithTokens(Seq(7.toByte, 11.toByte))

    buildAndValidate(Seq(b), whitelist = Set(tokenIdOf(7.toByte))) shouldBe None
  }

  property("only the whitelisted token is spared: other burned boxes are still burnt") {
    val whitelisted = burnCandidateWithTokens(Seq(7.toByte))
    val ordinary = burnCandidateWithTokens(Seq(11.toByte))

    val tx = buildAndValidate(Seq(whitelisted, ordinary),
      whitelist = Set(tokenIdOf(7.toByte))).get

    // the whitelisted box is not an input at all, so it is neither burned nor recreated
    tx.inputs.map(_.boxId) should not contain whitelisted.id
    tx.inputs.map(_.boxId) should contain(ordinary.id)
    outputTokenIds(tx) shouldBe empty // the ordinary box's token is burnt
  }

  property("a recreated box keeps all of its tokens, whitelisted or not") {
    // The recreate branch only charges the storage fee, so tokens are never at risk
    // there; the whitelist must not cause such a box to be skipped.
    val b = tokenizedBox(10000000000L, Constants.StoragePeriod, withToken = true)
    b.tokens.keySet should contain(WhitelistedTokenId)

    val tx = buildAndValidate(Seq(b), whitelist = Set(WhitelistedTokenId)).get
    outputTokenIds(tx) shouldBe Set(WhitelistedTokenId)
  }

  property("a reemission token is never burnt even when it is not whitelisted") {
    val reemission = 9.toByte
    val reemissionId = tokenIdOf(reemission)
    val b = burnCandidateWithTokens(Seq(reemission))

    // empty whitelist: burning is allowed for other tokens, but this box holds a
    // reemission token, which is consensus-unclaimable, so it is skipped not burnt
    buildAndValidate(Seq(b), reemissionTokenId = Some(reemissionId)) shouldBe None
  }

  /** Every token id carried by any output of `tx`, with its total amount. */
  private def outputTokens(tx: ErgoTransaction): Map[ModifierId, Long] =
    tx.outputCandidates.flatMap(_.additionalTokens.toArray
      .map(e => e._1.toModifierId -> e._2)).groupBy(_._1).map { case (id, amts) =>
      id -> amts.map(_._2).sum
    }

  property("a non-expiring box is skipped, so it keeps every token untouched") {
    // Too young to be rent-eligible: the builder must not spend it at all, which leaves
    // its tokens in the UTXO rather than burning or stranding them.
    val tokens = Seq(11.toByte, 12.toByte)
    val b = boxWithTokens(10000000000L, Constants.StoragePeriod - 1, tokens)
    tokens.foreach(t => b.tokens.keySet should contain(tokenIdOf(t)))

    buildAndValidate(Seq(b)) shouldBe None
  }

  property("a recreated box carries the full amount of every token it holds") {
    // The recreate branch only charges the storage fee, so the output must reproduce the
    // input's tokens exactly - same ids and same quantities, nothing dropped or merged.
    val tokens = Seq(11.toByte, 12.toByte, 13.toByte)
    val b = boxWithTokens(10000000000L, Constants.StoragePeriod, tokens)
    val expected = b.tokens

    val tx = buildAndValidate(Seq(b)).get

    val recreated = tx.outputCandidates.head
    recreated.additionalTokens.toArray
      .map(e => e._1.toModifierId -> e._2).toMap shouldBe expected
    outputTokens(tx) shouldBe expected
  }

  property("recreated boxes keep their tokens when burned boxes are present") {
    val recreateTokens = Seq(11.toByte, 12.toByte)
    val burnTokens = Seq(13.toByte)
    val recreateBox = boxWithTokens(10000000000L, Constants.StoragePeriod, recreateTokens)
    val burnBox = burnCandidateWithTokens(burnTokens)
    val expected = recreateBox.tokens

    val tx = buildAndValidate(Seq(recreateBox, burnBox)).get

    // burned box's tokens are gone; the recreated box keeps every token, in full
    outputTokens(tx) shouldBe expected
  }

  property("every recreated box keeps its own tokens across a mixed claim") {
    // A claim over several boxes must not cross-contaminate token sets: each recreated
    // output carries exactly the tokens of the box it recreates.
    val tokenSets = Seq(Seq(11.toByte), Seq(12.toByte), Seq(13.toByte, 14.toByte))
    val boxes = tokenSets.zipWithIndex.map { case (ts, i) =>
      boxWithTokens(10000000000L + i, Constants.StoragePeriod + i, ts)
    }

    val tx = buildAndValidate(boxes).get
    val indices = var127Indices(tx)
    val boxesById = boxes.map(b => b.id -> b).toMap

    withClue("every recreated box must keep all of its tokens: ") {
      boxes.foreach { b =>
        val inputIdx = tx.inputs.indexWhere(_.boxId == b.id)
        inputIdx should be >= 0
        val named = tx.outputCandidates(indices(inputIdx))
        named.additionalTokens.toArray.map(e => e._1.toModifierId -> e._2).toMap shouldBe
          boxesById(b.id).tokens
      }
    }
    // and the union across outputs is the union of the inputs' tokens, nothing else
    outputTokens(tx) shouldBe boxes.flatMap(_.tokens).toMap
  }

  property("tokens are conserved when a whitelisted box is left behind") {
    // The skipped box is not an input, so its tokens stay in the UTXO; the claim must not
    // recreate or burn them either.
    val whitelisted = boxWithTokens(10000000000L, Constants.StoragePeriod, Seq(7.toByte))
    val ordinary = boxWithTokens(10000000000L + 1, Constants.StoragePeriod,
      Seq(11.toByte))

    val tx = buildAndValidate(Seq(whitelisted, ordinary),
      whitelist = Set(tokenIdOf(7.toByte))).get

    // both are recreatable, so the whitelist does not apply and both are recreated
    tx.inputs.length shouldBe 2
    outputTokens(tx) shouldBe (whitelisted.tokens ++ ordinary.tokens)
  }
}
