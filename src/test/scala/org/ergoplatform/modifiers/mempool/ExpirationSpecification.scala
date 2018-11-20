package org.ergoplatform.modifiers.mempool

import org.ergoplatform.nodeView.ErgoInterpreter
import org.ergoplatform.settings.Constants
import org.ergoplatform.utils.ErgoPropertyTest
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.scalatest.Assertion
import sigmastate.Values
import sigmastate.Values.ShortConstant
import sigmastate.interpreter.{ContextExtension, ProverResult}


class ExpirationSpecification extends ErgoPropertyTest {

  type Height = Long

  private implicit val verifier: ErgoInterpreter = ErgoInterpreter()

  def falsify(box: ErgoBox): ErgoBox = {
    ErgoBox(box.value,
      Values.FalseLeaf,
      box.creationHeight,
      box.additionalTokens,
      box.additionalRegisters,
      transactionId = box.transactionId,
      boxId = box.index)
  }

  def constructTest(from: ErgoBox,
                    heightDelta: Int,
                    outsConstructor: Height => IndexedSeq[ErgoBoxCandidate],
                    expectedValidity: Boolean): Assertion = {
    val in = Input(from.id,
      ProverResult(Array.emptyByteArray, ContextExtension(Map(Constants.StorageIndexVarId -> ShortConstant(0)))))

    val h: Int = (from.creationHeight + Constants.StoragePeriod + heightDelta).toInt

    val oc = outsConstructor(h).map(c => updateHeight(c, h))
    val tx = ErgoTransaction(inputs = IndexedSeq(in), outputCandidates = oc)

    val fb0 = invalidErgoFullBlockGen.sample.get
    val fb = fb0.copy(fb0.header.copy(height = h))
    val fakeHeader = fb.header.copy(height = fb.header.height - 1)
    val updContext = emptyStateContext.updateHeaders(Seq(fakeHeader)).appendFullBlock(fb, votingSettings).get

    tx.statelessValidity.isSuccess shouldBe true
    tx.statefulValidity(IndexedSeq(from), updContext).isSuccess shouldBe expectedValidity
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
        val feeBoxCandidate = new ErgoBoxCandidate(fee, Values.TrueLeaf, creationHeight = h)
        IndexedSeq(changeValue(from, -fee), Some(feeBoxCandidate)).flatten
      }, expectedValidity = true)
    }
  }

  property("unsuccessful spending due too big storage fee charged") {
    forAll(unspendableErgoBoxGen(parameters.storageFeeFactor  * 100 + 1, Long.MaxValue)) { from =>
      constructTest(from, 0, h => {
        val fee = Math.min(parameters.storageFeeFactor * from.bytes.length + 1, from.value)
        val feeBoxCandidate = new ErgoBoxCandidate(fee, Values.TrueLeaf, creationHeight = h)
        IndexedSeq(changeValue(from, -fee), Some(feeBoxCandidate)).flatten
      }, expectedValidity = false)
    }
  }

  property("unsuccessful spending when more time passed than storage period and charged more than K*storagePeriod") {
    forAll(unspendableErgoBoxGen(parameters.storageFeeFactor * 100 + 1, Long.MaxValue)) { from =>
      constructTest(from, 1, h => {
        val fee = Math.min(parameters.storageFeeFactor * from.bytes.length + 1, from.value)
        val feeBoxCandidate = new ErgoBoxCandidate(fee, Values.TrueLeaf, creationHeight = h)

        IndexedSeq(changeValue(from, -fee), Some(feeBoxCandidate)).flatten
      }, expectedValidity = false)
    }
  }

  property("too early spending") {
    forAll(unspendableErgoBoxGen()) { from =>
      constructTest(from, -1, h => {
        val fee = Math.min(parameters.storageFeeFactor * from.bytes.length, from.value)
        val feeBoxCandidate = new ErgoBoxCandidate(fee, Values.TrueLeaf, creationHeight = h)
        IndexedSeq(changeValue(from, -fee), Some(feeBoxCandidate)).flatten
      }, expectedValidity = false)
    }
  }

  property("script changed spending w. same value") {
    forAll(unspendableErgoBoxGen()) { from =>
      val out = new ErgoBoxCandidate(from.value, Values.TrueLeaf, from.creationHeight + 1, from.additionalTokens)
      constructTest(from, 0, _ => IndexedSeq(out), expectedValidity = false)
    }
  }

  property("script changed tokens w. same value") {
    forAll(unspendableErgoBoxGen()) { from =>
      whenever(from.additionalTokens.nonEmpty) {
        val out = new ErgoBoxCandidate(from.value, from.proposition, from.creationHeight + 1, Seq())
        constructTest(from, 0, _ => IndexedSeq(out), expectedValidity = false)
      }
    }
  }

  //todo: test register change

  property("spending of whole coin when its value no more than storage fee") {
    val out2 = ergoBoxGenNoProp.sample.get
    val minValue = out2.value + 1

    forAll(unspendableErgoBoxGen(minValue, Long.MaxValue)) { from =>
      val outcome = from.value <= from.bytes.length * parameters.storageFeeFactor
      val out1 = new ErgoBoxCandidate(from.value - minValue, Values.FalseLeaf, creationHeight = from.creationHeight + 1)
      constructTest(from, 0, _ => IndexedSeq(out1, out2), expectedValidity = outcome)
    }
  }
}