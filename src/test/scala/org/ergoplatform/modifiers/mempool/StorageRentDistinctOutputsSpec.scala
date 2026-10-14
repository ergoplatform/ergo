package org.ergoplatform.modifiers.mempool

import org.ergoplatform.nodeView.state.{ErgoStateContext, VotingData}
import org.ergoplatform.settings.Constants
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.ergoplatform.settings.Constants.TrueTree
import org.scalatest.Assertion
import scorex.util.bytesToId
import sigma.ast.ShortConstant
import sigma.interpreter.{ContextExtension, ProverResult}
import sigmastate.helpers.TestingHelpers._

/**
  * Pins the `txRentDistinctOutputs` rule: storage-rent recreation inputs of one transaction
  * must name pairwise distinct var-127 outputs, so two identical expired funded boxes can not
  * be claimed against one shared output (which would let the collector keep everything above
  * the larger box's recreation floor). Fully-consumable inputs are unconstrained.
  */
class StorageRentDistinctOutputsSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators._
  import org.ergoplatform.utils.NodeViewTestOps._

  private implicit val verifier: ErgoInterpreter = ErgoInterpreter(parameters)

  // chosen so that spending heights (creationHeight + StoragePeriod) are past the activation
  // height and have the same VLQ serialization length as the creation height, keeping
  // recreated boxes the same size as the expired ones (the dust rule prices the new box's
  // bytes, including its new creation height)
  private val PostActivationCreationHeight: Int = 2200001

  private val PreActivationCreationHeight: Int = 0

  /** Boxes identical in everything but id and value: the attack precondition. */
  private def fundedPair(creationHeight: Int): (ErgoBox, ErgoBox) = {
    val b1 = testBox(10000000000L, Constants.FalseTree, creationHeight, Seq(), Map.empty,
      bytesToId(Array.fill(32)(1.toByte)), 0)
    val b2 = testBox(5000000000L, Constants.FalseTree, creationHeight, Seq(), Map.empty,
      bytesToId(Array.fill(32)(2.toByte)), 1)
    (b1, b2)
  }

  private def storageFeeOf(box: ErgoBox): Long =
    parameters.storageFeeFactor.toLong * box.bytes.length

  private def minValueOf(box: ErgoBox): Long = parameters.minValuePerByte.toLong * box.bytes.length

  private def claimTest(boxes: Seq[ErgoBox],
                        outs: Seq[ErgoBoxCandidate],
                        inputOutputIndices: Seq[Int],
                        expectedValidity: Boolean): Assertion = {
    val h: Int = boxes.head.creationHeight + Constants.StoragePeriod
    whenever(h % votingSettings.votingLength != 0) {
      val ins = boxes.zip(inputOutputIndices).map { case (b, outIdx) =>
        Input(b.id, ProverResult(
          Array.emptyByteArray,
          ContextExtension(Map(Constants.StorageIndexVarId -> ShortConstant(outIdx.toShort)))
        ))
      }
      val oc = outs.map(c => updateHeight(c, h))
      val tx = ErgoTransaction(inputs = ins.toIndexedSeq, dataInputs = IndexedSeq(), outputCandidates = oc.toIndexedSeq)

      val fb0 = invalidErgoFullBlockGen.sample.get
      val fakeHeader = fb0.header.copy(height = h - 1)
      val fb = fb0.copy(fb0.header.copy(height = h, parentId = fakeHeader.id))
      val updContext = {
        val inContext = new ErgoStateContext(Seq(fakeHeader), None, genesisStateDigest, parameters,
          validationSettingsNoIl, VotingData.empty)(settings.chainSettings)
        inContext.appendFullBlock(fb).get
      }

      tx.statelessValidity().isSuccess shouldBe true
      tx.statefulValidity(boxes.toIndexedSeq, emptyDataBoxes, updContext).isSuccess shouldBe expectedValidity
    }
  }

  property("two identical expired boxes claimed against one shared output are invalid after activation") {
    val (b1, b2) = fundedPair(PostActivationCreationHeight)
    val fee = storageFeeOf(b1)
    // both inputs name output 0, which only has to hold max(values) - fee
    val outs = Seq(
      new ErgoBoxCandidate(b1.value - fee, Constants.FalseTree, 0),
      new ErgoBoxCandidate(b2.value + fee, TrueTree, 0)
    )
    claimTest(Seq(b1, b2), outs, Seq(0, 0), expectedValidity = false)
  }

  property("the same claim shape is valid before the activation height") {
    val (b1, b2) = fundedPair(PreActivationCreationHeight)
    val fee = storageFeeOf(b1)
    val outs = Seq(
      new ErgoBoxCandidate(b1.value - fee, Constants.FalseTree, 0),
      new ErgoBoxCandidate(b2.value + fee, TrueTree, 0)
    )
    claimTest(Seq(b1, b2), outs, Seq(0, 0), expectedValidity = true)
  }

  property("identical boxes claimed against distinct recreated outputs are valid") {
    val (b1, b2) = fundedPair(PostActivationCreationHeight)
    val fee = storageFeeOf(b1)
    val outs = Seq(
      new ErgoBoxCandidate(b1.value - fee, Constants.FalseTree, 0),
      new ErgoBoxCandidate(b2.value - fee, Constants.FalseTree, 0),
      new ErgoBoxCandidate(2 * fee, TrueTree, 0)
    )
    claimTest(Seq(b1, b2), outs, Seq(0, 1), expectedValidity = true)
  }

  property("recreation input and fully-consumable input may not share an output index after activation") {
    val (b1, _) = fundedPair(PostActivationCreationHeight)
    val dustBox = testBox(1000L, Constants.FalseTree, PostActivationCreationHeight, Seq(), Map.empty,
      bytesToId(Array.fill(32)(3.toByte)), 2)
    val fee = storageFeeOf(b1)
    val outs = Seq(
      new ErgoBoxCandidate(b1.value - fee, Constants.FalseTree, 0),
      new ErgoBoxCandidate(dustBox.value + fee, TrueTree, 0)
    )
    claimTest(Seq(b1, dustBox), outs, Seq(0, 0), expectedValidity = false)
    // with distinct indices the claim is valid
    claimTest(Seq(b1, dustBox), outs, Seq(0, 1), expectedValidity = true)
  }

  property("multiple fully-consumable inputs may not share the aggregate output after activation") {
    val dust1 = testBox(10000000L, Constants.FalseTree, PostActivationCreationHeight, Seq(), Map.empty,
      bytesToId(Array.fill(32)(4.toByte)), 3)
    val dust2 = testBox(20000000L, Constants.FalseTree, PostActivationCreationHeight, Seq(), Map.empty,
      bytesToId(Array.fill(32)(5.toByte)), 4)
    val shared = Seq(new ErgoBoxCandidate(dust1.value + dust2.value, TrueTree, 0))
    claimTest(Seq(dust1, dust2), shared, Seq(0, 0), expectedValidity = false)

    // one output per fully-consumed box is valid
    val distinct = Seq(
      new ErgoBoxCandidate(dust1.value, TrueTree, 0),
      new ErgoBoxCandidate(dust2.value, TrueTree, 0)
    )
    claimTest(Seq(dust1, dust2), distinct, Seq(0, 1), expectedValidity = true)
  }

  property("a shared aggregate output is valid before the activation height") {
    val dust1 = testBox(10000000L, Constants.FalseTree, PreActivationCreationHeight, Seq(), Map.empty,
      bytesToId(Array.fill(32)(4.toByte)), 3)
    val dust2 = testBox(20000000L, Constants.FalseTree, PreActivationCreationHeight, Seq(), Map.empty,
      bytesToId(Array.fill(32)(5.toByte)), 4)
    val shared = Seq(new ErgoBoxCandidate(dust1.value + dust2.value, TrueTree, 0))
    claimTest(Seq(dust1, dust2), shared, Seq(0, 0), expectedValidity = true)
  }

  property("sponsorship recreate with a topped-up value stays valid") {
    val sponsoredProbe = testBox(1000000L, Constants.FalseTree, PostActivationCreationHeight,
      Seq(), Map.empty, bytesToId(Array.fill(32)(6.toByte)), 5)
    val sponsored = testBox(minValueOf(sponsoredProbe) - 1, Constants.FalseTree, PostActivationCreationHeight,
      Seq(), Map.empty, bytesToId(Array.fill(32)(6.toByte)), 5)
    val topUp = minValueOf(sponsored) - sponsored.value
    val sponsorBox = testBox(10000000000L, TrueTree, PostActivationCreationHeight, Seq(), Map.empty,
      bytesToId(Array.fill(32)(7.toByte)), 6)

    val h = sponsored.creationHeight + Constants.StoragePeriod
    whenever(h % votingSettings.votingLength != 0) {
      val ins = IndexedSeq(
        Input(sponsored.id, ProverResult(
          Array.emptyByteArray,
          ContextExtension(Map(Constants.StorageIndexVarId -> ShortConstant(0))))),
        Input(sponsorBox.id, ProverResult.empty)
      )
      val outs = IndexedSeq(
        new ErgoBoxCandidate(minValueOf(sponsored), Constants.FalseTree, h),
        new ErgoBoxCandidate(sponsorBox.value - topUp, TrueTree, h)
      )
      val tx = ErgoTransaction(ins, IndexedSeq.empty, outs)

      val fb0 = invalidErgoFullBlockGen.sample.get
      val fakeHeader = fb0.header.copy(height = h - 1)
      val fb = fb0.copy(fb0.header.copy(height = h, parentId = fakeHeader.id))
      val updContext = {
        val inContext = new ErgoStateContext(Seq(fakeHeader), None, genesisStateDigest, parameters,
          validationSettingsNoIl, VotingData.empty)(settings.chainSettings)
        inContext.appendFullBlock(fb).get
      }

      // recreated output is worth more than the expired box itself (top-up sponsorship)
      tx.statelessValidity().isSuccess shouldBe true
      tx.statefulValidity(IndexedSeq(sponsored, sponsorBox), emptyDataBoxes, updContext).isSuccess shouldBe true
    }
  }

}
