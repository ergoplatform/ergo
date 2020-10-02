package org.ergoplatform.modifiers.mempool

import org.ergoplatform.nodeView.state.{ErgoStateContext, VotingData}
import org.ergoplatform.settings.{Constants, LaunchParameters}
import org.ergoplatform.utils.ErgoPropertyTest
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.scalatest.Assertion
import sigmastate.Values.ShortConstant
import sigmastate.interpreter.{ContextExtension, ProverResult}
import sigmastate.eval._
import sigmastate.helpers.TestingHelpers._

class ExpirationSpecification extends ErgoPropertyTest {

  type Height = Int

  private implicit val verifier: ErgoInterpreter = ErgoInterpreter(LaunchParameters)

  def falsify(box: ErgoBox): ErgoBox = {
    testBox(box.value,
      Constants.FalseLeaf,
      box.creationHeight,
      box.additionalTokens.toArray.toSeq,
      box.additionalRegisters,
      transactionId = box.transactionId,
      boxIndex = box.index)
  }

  def constructTest(from: ErgoBox,
                    heightDelta: Int,
                    outsConstructor: Height => IndexedSeq[ErgoBoxCandidate],
                    expectedValidity: Boolean): Assertion = {
    // We are filtering out certain heights to avoid problems with improperly generated extension
    // at the beginning of a voting epoch
    whenever((from.creationHeight + Constants.StoragePeriod + heightDelta) % votingSettings.votingLength != 0) {
      val in = Input(from.id,
        ProverResult(Array.emptyByteArray, ContextExtension(Map(Constants.StorageIndexVarId -> ShortConstant(0)))))

      val h: Int = from.creationHeight + Constants.StoragePeriod + heightDelta

      val oc = outsConstructor(h).map(c => updateHeight(c, h))
      val tx = ErgoTransaction(inputs = IndexedSeq(in), dataInputs = IndexedSeq(), outputCandidates = oc)

      val fb0 = invalidErgoFullBlockGen.sample.get
      val fakeHeader = fb0.header.copy(height = h - 1)
      val fb = fb0.copy(fb0.header.copy(height = h, parentId = fakeHeader.id))

      val updContext = {
        val inContext = new ErgoStateContext(Seq(fakeHeader), None, genesisStateDigest, LaunchParameters, validationSettingsNoIl,
          VotingData.empty)(votingSettings)
        inContext.appendFullBlock(fb, votingSettings).get
      }

      tx.statelessValidity.isSuccess shouldBe true
      tx.statefulValidity(IndexedSeq(from), emptyDataBoxes, updContext).isSuccess shouldBe expectedValidity
    }
  }

  property("successful spending w. same value") {
    forAll(unspendableErgoBoxGen()) { from =>
      constructTest(from, 0, _ => IndexedSeq(from), expectedValidity = true)
    }
  }

  property("successful spending w. max spending") {
    forAll(unspendableErgoBoxGen()) { from =>
      constructTest(from, 0, h => {
        val fee = Math.min(parameters.storageFeeFactor * from.bytes.length, from.value)
        val feeBoxCandidate = new ErgoBoxCandidate(fee, Constants.TrueLeaf, creationHeight = h)
        IndexedSeq(changeValue(from, -fee), Some(feeBoxCandidate)).flatten
      }, expectedValidity = true)
    }
  }

  property("unsuccessful spending due too big storage fee charged") {
    forAll(unspendableErgoBoxGen(parameters.storageFeeFactor * 100 + 1, Long.MaxValue)) { from =>
      constructTest(from, 0, h => {
        val fee = Math.min(parameters.storageFeeFactor * from.bytes.length + 1, from.value)
        val feeBoxCandidate = new ErgoBoxCandidate(fee, Constants.TrueLeaf, creationHeight = h)
        IndexedSeq(changeValue(from, -fee), Some(feeBoxCandidate)).flatten
      }, expectedValidity = false)
    }
  }

  property("unsuccessful spending when more time passed than storage period and charged more than K*storagePeriod") {
    forAll(unspendableErgoBoxGen(parameters.storageFeeFactor * 100 + 1, Long.MaxValue)) { from =>
      constructTest(from, 1, h => {
        val fee = Math.min(parameters.storageFeeFactor * from.bytes.length + 1, from.value)
        val feeBoxCandidate = new ErgoBoxCandidate(fee, Constants.TrueLeaf, creationHeight = h)

        IndexedSeq(changeValue(from, -fee), Some(feeBoxCandidate)).flatten
      }, expectedValidity = false)
    }
  }

  property("too early spending") {
    forAll(unspendableErgoBoxGen()) { from =>
      constructTest(from, -1, h => {
        val fee = Math.min(parameters.storageFeeFactor * from.bytes.length, from.value)
        val feeBoxCandidate = new ErgoBoxCandidate(fee, Constants.TrueLeaf, creationHeight = h)
        IndexedSeq(changeValue(from, -fee), Some(feeBoxCandidate)).flatten
      }, expectedValidity = false)
    }
  }

  property("script changed spending w. same value") {
    forAll(unspendableErgoBoxGen()) { from =>
      val out = new ErgoBoxCandidate(from.value, Constants.TrueLeaf, from.creationHeight + 1, from.additionalTokens)
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

  property("spending of whole coin when its value no more than storage fee") {
    val out2 = ergoBoxGenNoProp.sample.get
    val minValue = out2.value + 1

    forAll(unspendableErgoBoxGen(minValue, Long.MaxValue)) { from =>
      val outcome = from.value <= from.bytes.length * parameters.storageFeeFactor
      val out1 = new ErgoBoxCandidate(from.value - minValue, Constants.TrueLeaf, creationHeight = from.creationHeight + 1)
      constructTest(from, 0, _ => IndexedSeq(out1, out2), expectedValidity = outcome)
    }
  }

  property("destructing the whole box when its value no more than storage fee") {
    forAll(unspendableErgoBoxGen(maxValue = parameters.storageFeeFactor)) { from =>
      val out = new ErgoBoxCandidate(from.value, Constants.TrueLeaf, creationHeight = from.creationHeight + 1)
      constructTest(from, 0, _ => IndexedSeq(out), expectedValidity = true)
    }
  }
}
