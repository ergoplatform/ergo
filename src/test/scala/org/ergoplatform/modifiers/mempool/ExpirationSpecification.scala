package org.ergoplatform.modifiers.mempool

import org.ergoplatform.nodeView.state.{ErgoStateContext, VotingData}
import org.ergoplatform.settings.Constants
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.scalatest.Assertion
import scorex.util.encode.Base16
import sigma.Colls
import sigma.ast.{ErgoTree, IntConstant, ShortConstant}
import sigma.interpreter.{ContextExtension, ProverResult}
import sigma.serialization.ErgoTreeSerializer
import sigmastate.helpers.TestingHelpers._
import org.ergoplatform.settings.Constants.TrueTree

class ExpirationSpecification extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators._
  import org.ergoplatform.utils.NodeViewTestOps._

  type Height = Int

  private implicit val verifier: ErgoInterpreter = ErgoInterpreter(parameters)

  /** Creation height for deterministic fixtures: chosen so that the spending heights
    * (`creationHeight + StoragePeriod + delta`) are past the grace-period activation height and
    * have the same VLQ serialization length as the creation height, keeping recreated boxes the
    * same size as the expired ones (the dust rule prices the new box's bytes, including its new
    * creation height). */
  private val FixtureCreationHeight: Int = 2200001

  /** A box carrying an additional register, to which the grace-period rules apply. */
  private def graceProtectedBox(value: Long, creationHeight: Int = FixtureCreationHeight): ErgoBox =
    testBox(value, Constants.FalseTree, creationHeight, Seq(), Map(ErgoBox.R4 -> IntConstant(0)))

  private def minValueOf(box: ErgoBox): Long = parameters.minValuePerByte.toLong * box.bytes.length

  /** A box whose value is exactly the protocol minimum for its size (VLQ fixpoint). */
  private def boxAtMinValue(probe: ErgoBox): ErgoBox = {
    val next = graceProtectedBox(minValueOf(probe), probe.creationHeight)
    if (next.value == minValueOf(next)) next else boxAtMinValue(next)
  }

  def injectScript(box: ErgoBox, script: ErgoTree): ErgoBox = {
    testBox(box.value,
      script,
      box.creationHeight,
      box.additionalTokens.toArray.toSeq,
      box.additionalRegisters,
      transactionId = box.transactionId,
      boxIndex = box.index)
  }

  def constructTest(from: ErgoBox,
                    heightDelta: Int,
                    outsConstructor: Height => IndexedSeq[ErgoBoxCandidate],
                    expectedValidity: Boolean,
                    additionalInputs: Seq[ErgoBox] = Seq.empty): Assertion = {
    // We are filtering out certain heights to avoid problems with improperly generated extension
    // at the beginning of a voting epoch
    whenever((from.creationHeight + Constants.StoragePeriod + heightDelta) % votingSettings.votingLength != 0) {
      val in = Input(from.id,
        ProverResult(Array.emptyByteArray, ContextExtension(Map(Constants.StorageIndexVarId -> ShortConstant(0)))))
      val ins = in +: additionalInputs.map(b => Input(b.id, ProverResult.empty))

      val h: Int = from.creationHeight + Constants.StoragePeriod + heightDelta

      val oc = outsConstructor(h).map(c => updateHeight(c, h))
      val tx = ErgoTransaction(inputs = ins.toIndexedSeq, dataInputs = IndexedSeq(), outputCandidates = oc)

      val fb0 = invalidErgoFullBlockGen.sample.get
      val fakeHeader = fb0.header.copy(height = h - 1)
      val fb = fb0.copy(fb0.header.copy(height = h, parentId = fakeHeader.id))

      val updContext = {
        val inContext = new ErgoStateContext(Seq(fakeHeader), None, genesisStateDigest, parameters, validationSettingsNoIl,
          VotingData.empty)(settings.chainSettings)
        inContext.appendFullBlock(fb).get
      }

      //serialization roundtrip
      val bs = ErgoTransactionSerializer.toBytes(tx)
      ErgoTransactionSerializer.parseBytes(bs) shouldBe tx

      tx.statelessValidity().isSuccess shouldBe true
      tx.statefulValidity(IndexedSeq(from) ++ additionalInputs, emptyDataBoxes, updContext)
        .isSuccess shouldBe expectedValidity
    }
  }

  property("successful spending w. same value") {
    // value is bounded from below so that it always exceeds the protocol minimum for the box
    forAll(unspendableErgoBoxGen(parameters.minValuePerByte.toLong * ErgoBox.MaxBoxSize, Long.MaxValue)) { from0 =>
      val from = testBox(from0.value, from0.ergoTree, FixtureCreationHeight,
        from0.additionalTokens.toArray.toSeq, from0.additionalRegisters)
      constructTest(from, 0, _ => IndexedSeq(from), expectedValidity = true)
    }
  }

  property("successful spending w. invalid ergotree") {
    forAll(unspendableErgoBoxGen(parameters.minValuePerByte.toLong * ErgoBox.MaxBoxSize, Long.MaxValue)) { from0 =>
      // invalid (unparseable) ergo tree
      val etString = "0e1631393039303063646462363930366462363530336665"
      val et = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(Base16.decode(etString).get)
      val from = testBox(from0.value, from0.ergoTree, FixtureCreationHeight,
        from0.additionalTokens.toArray.toSeq, from0.additionalRegisters)
      val modified = injectScript(from, et)
      constructTest(modified, 0, _ => IndexedSeq(modified), expectedValidity = true)
    }
  }

  property("successful spending w. max spending") {
    forAll(unspendableErgoBoxGen(parameters.minValuePerByte.toLong * ErgoBox.MaxBoxSize, Long.MaxValue)) { from0 =>
      val from = testBox(from0.value, from0.ergoTree, FixtureCreationHeight,
        from0.additionalTokens.toArray.toSeq, from0.additionalRegisters)
      constructTest(from, 0, h => {
        // for grace-protected boxes the charge stops at the minimum allowed value
        val maxCharge =
          if (from.additionalTokens.nonEmpty || from.additionalRegisters.nonEmpty) {
            from.value - minValueOf(from)
          } else {
            from.value
          }
        val fee = Math.min(parameters.storageFeeFactor * from.bytes.length, maxCharge)
        val feeBoxCandidate = if (fee > 0) Some(new ErgoBoxCandidate(fee, TrueTree, creationHeight = h)) else None
        IndexedSeq(changeValue(from, -fee), feeBoxCandidate).flatten
      }, expectedValidity = true)
    }
  }

  property("unsuccessful spending due too big storage fee charged") {
    forAll(unspendableErgoBoxGen(parameters.storageFeeFactor * 100 + 1, Long.MaxValue)) { from =>
      constructTest(from, 0, h => {
        val fee = Math.min(parameters.storageFeeFactor * from.bytes.length + 1, from.value)
        val feeBoxCandidate = new ErgoBoxCandidate(fee, TrueTree, creationHeight = h)
        IndexedSeq(changeValue(from, -fee), Some(feeBoxCandidate)).flatten
      }, expectedValidity = false)
    }
  }

  property("unsuccessful spending when more time passed than storage period and charged more than K*storagePeriod") {
    forAll(unspendableErgoBoxGen(parameters.storageFeeFactor * 100 + 1, Long.MaxValue)) { from =>
      constructTest(from, 1, h => {
        val fee = Math.min(parameters.storageFeeFactor * from.bytes.length + 1, from.value)
        val feeBoxCandidate = new ErgoBoxCandidate(fee, TrueTree, creationHeight = h)

        IndexedSeq(changeValue(from, -fee), Some(feeBoxCandidate)).flatten
      }, expectedValidity = false)
    }
  }

  property("too early spending") {
    forAll(unspendableErgoBoxGen()) { from =>
      constructTest(from, -1, h => {
        val fee = Math.min(parameters.storageFeeFactor * from.bytes.length, from.value)
        val feeBoxCandidate = new ErgoBoxCandidate(fee, TrueTree, creationHeight = h)
        IndexedSeq(changeValue(from, -fee), Some(feeBoxCandidate)).flatten
      }, expectedValidity = false)
    }
  }

  property("script changed spending w. same value") {
    forAll(unspendableErgoBoxGen()) { from =>
      val out = new ErgoBoxCandidate(from.value, TrueTree, from.creationHeight + 1, from.additionalTokens)
      constructTest(from, 0, _ => IndexedSeq(out), expectedValidity = false)
    }
  }

  property("script changed tokens w. same value") {
    forAll(unspendableErgoBoxGen()) { from =>
      whenever(from.additionalTokens.nonEmpty) {
        val out = new ErgoBoxCandidate(from.value, from.ergoTree, from.creationHeight + 1, Colls.emptyColl)
        constructTest(from, 0, _ => IndexedSeq(out), expectedValidity = false)
      }
    }
  }

  property("script changed register w. same value") {
    forAll(unspendableErgoBoxGen()) { from =>
      whenever(from.additionalRegisters.get(ErgoBox.R4).nonEmpty) {
        val out = new ErgoBoxCandidate(from.value, from.ergoTree, from.creationHeight + 1, from.additionalTokens)
        constructTest(from, 0, _ => IndexedSeq(out), expectedValidity = false)
      }
    }
  }

  property("original rules: destructing the whole box when its value no more than storage fee") {
    forAll(unspendableErgoBoxGen(maxValue = parameters.storageFeeFactor)) { from0 =>
      // spending height below the activation height: original rules even for boxes with tokens
      // or additional registers
      val from = testBox(from0.value, from0.ergoTree, 0,
        from0.additionalTokens.toArray.toSeq, from0.additionalRegisters)
      val out = new ErgoBoxCandidate(from.value, TrueTree, from.creationHeight + 1)
      constructTest(from, 0, _ => IndexedSeq(out), expectedValidity = true)
    }
  }

  property("plain box without tokens or registers is destructible as before, even after the activation") {
    // generators always attach tokens or registers, so plain boxes are built directly
    Seq(72000L, parameters.storageFeeFactor.toLong / 2, parameters.storageFeeFactor.toLong).foreach { v =>
      val from = testBox(v, Constants.FalseTree, FixtureCreationHeight)
      val out = new ErgoBoxCandidate(from.value, TrueTree, 0)
      constructTest(from, 0, _ => IndexedSeq(out), expectedValidity = true)
    }
  }

  property("grace period: box at the minimum value can be destructed only after the grace period") {
    val from = boxAtMinValue(graceProtectedBox(1000000L))
    // destruction shape: script changed, so the recreation branch does not apply
    val out = new ErgoBoxCandidate(from.value, TrueTree, 0)
    constructTest(from, 0, _ => IndexedSeq(out), expectedValidity = false)
    constructTest(from, verifier.StorageGracePeriod - 1, _ => IndexedSeq(out), expectedValidity = false)
    constructTest(from, verifier.StorageGracePeriod, _ => IndexedSeq(out), expectedValidity = true)
    constructTest(from, verifier.StorageGracePeriod + 1, _ => IndexedSeq(out), expectedValidity = true)
  }

  property("grace period: box above the minimum is charged towards the minimum, destructible only after the grace period") {
    forAll(unspendableErgoBoxGen(1, parameters.storageFeeFactor * 100)) { from0 =>
      // recreate the box at the fixture height, so that recreated outputs at the spending height
      // have the same serialized size (see the comment on FixtureCreationHeight)
      val from = testBox(from0.value, from0.ergoTree, FixtureCreationHeight,
        from0.additionalTokens.toArray.toSeq, from0.additionalRegisters)
      val minV = minValueOf(from)
      val storageFee = parameters.storageFeeFactor.toLong * from.bytes.length
      whenever(from.value > minV && from.value - storageFee < minV &&
        from.value - minV > parameters.minValuePerByte.toLong * 200 &&
        (from.additionalTokens.nonEmpty || from.additionalRegisters.nonEmpty)) {
        // recreation keeping exactly the minimum value: valid, the charge stops at the minimum
        // (outputs must balance the input value, so the charge goes to a second output)
        val recreated = new ErgoBoxCandidate(minV, from.ergoTree, 0, from.additionalTokens, from.additionalRegisters)
        val charge = new ErgoBoxCandidate(from.value - minV, TrueTree, 0)
        constructTest(from, 0, _ => IndexedSeq(recreated, charge), expectedValidity = true)

        // recreation below the minimum value: invalid
        val belowMin = new ErgoBoxCandidate(minV - 1, from.ergoTree, 0, from.additionalTokens, from.additionalRegisters)
        val chargeBelow = new ErgoBoxCandidate(from.value - minV + 1, TrueTree, 0)
        constructTest(from, 0, _ => IndexedSeq(belowMin, chargeBelow), expectedValidity = false)

        // destruction while the box value does not cover the storage fee: valid only after the
        // grace period, not during it
        val destruct = new ErgoBoxCandidate(from.value, TrueTree, 0)
        constructTest(from, 0, _ => IndexedSeq(destruct), expectedValidity = false)
        constructTest(from, verifier.StorageGracePeriod - 1, _ => IndexedSeq(destruct), expectedValidity = false)
        constructTest(from, verifier.StorageGracePeriod, _ => IndexedSeq(destruct), expectedValidity = true)
      }
    }
  }

  property("grace period: box below the minimum value can be sponsored during the grace period") {
    val atMin = boxAtMinValue(graceProtectedBox(1000000L))
    val from = graceProtectedBox(atMin.value - 1)
    whenever(from.value < minValueOf(from)) {
      val minV = minValueOf(from)
      val topUp = minV - from.value
      val sponsorBox = testBox(10000000000L, TrueTree, 0)
      val outs = (_: Height) => IndexedSeq(
        new ErgoBoxCandidate(minV, from.ergoTree, 0, from.additionalTokens, from.additionalRegisters),
        new ErgoBoxCandidate(sponsorBox.value - topUp, TrueTree, 0)
      )
      // sponsorship recreate is valid both during and after the grace period
      constructTest(from, 0, outs, expectedValidity = true, additionalInputs = Seq(sponsorBox))
      constructTest(from, verifier.StorageGracePeriod, outs, expectedValidity = true,
        additionalInputs = Seq(sponsorBox))

      // but the box can not be destructed during the grace period
      val destruct = new ErgoBoxCandidate(from.value, TrueTree, 0)
      constructTest(from, 0, _ => IndexedSeq(destruct), expectedValidity = false)
    }
  }

}
