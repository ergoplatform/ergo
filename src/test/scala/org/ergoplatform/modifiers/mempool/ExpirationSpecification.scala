package org.ergoplatform.modifiers.mempool

import org.ergoplatform.nodeView.state.ErgoStateContext
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.ergoplatform.settings.{Constants, Parameters}
import org.ergoplatform.utils.ErgoPropertyTest
import org.scalatest.Assertion
import scorex.crypto.authds.ADDigest
import sigmastate.Values
import sigmastate.Values.ShortConstant
import sigmastate.interpreter.{ContextExtension, ProverResult}

class ExpirationSpecification extends ErgoPropertyTest {
  type Height = Long

  private val context = ErgoStateContext(0, ADDigest @@ Array.fill(32)(0: Byte))

  def falsify(box: ErgoBox): ErgoBox =
    ErgoBox(box.value, Values.FalseLeaf, box.additionalTokens, box.additionalRegisters)

  def constructTest(from: ErgoBox,
                    heightDelta: Int,
                    outsConstructor: Height => IndexedSeq[ErgoBoxCandidate],
                    expectedValidity: Boolean): Assertion = {
    val in = Input(from.id,
      ProverResult(Array.emptyByteArray, ContextExtension(Map(Constants.StorageIndexVarId -> ShortConstant(0)))))

    val h = Constants.StoragePeriod + heightDelta

    val oc = outsConstructor(h).map(c => updateHeight(c, h))
    val tx = ErgoTransaction(inputs = IndexedSeq(in), outputCandidates = oc)

    val updContext = context.copy(currentHeight = h)

    tx.statelessValidity.isSuccess shouldBe true
    tx.statefulValidity(IndexedSeq(from), updContext, settings.metadata).isSuccess shouldBe expectedValidity
  }

  property("successful spending w. same value") {
    forAll(unspendableErgoBoxGen()) { from =>
      constructTest(from, 0, _ => IndexedSeq(from), expectedValidity = true)
    }
  }

  property("successful spending w. max spending") {
    forAll(unspendableErgoBoxGen()) { from =>
      constructTest(from, 0, h => {
        val fee = Math.min(Parameters.K * (h - 0) * from.bytes.length, from.value)
        val feeBoxCandidate = new ErgoBoxCandidate(fee, Values.TrueLeaf, creationHeight = h)
        IndexedSeq(changeValue(from, -fee), Some(feeBoxCandidate)).flatten
      }, expectedValidity = true)
    }
  }

  property("unsuccessful spending due too big storage fee charged") {
    forAll(unspendableErgoBoxGen(Parameters.K * (Constants.StoragePeriod - 0) * 100 + 1, Long.MaxValue)) { from =>
      constructTest(from, 0, h => {
        val fee = Math.min(Parameters.K * (h - 0) * from.bytes.length + 1, from.value)
        val feeBoxCandidate = new ErgoBoxCandidate(fee, Values.TrueLeaf, creationHeight = h)
        IndexedSeq(changeValue(from, -fee), Some(feeBoxCandidate)).flatten
      }, expectedValidity = false)
    }
  }

  property("successful spending when more time passed than storage period and charged more than K*storagePeriod") {
    forAll(unspendableErgoBoxGen(Parameters.K * (Constants.StoragePeriod - 0) * 100 + 1, Long.MaxValue)) { from =>
      constructTest(from, 1, h => {
        val fee = Math.min(Parameters.K * Constants.StoragePeriod * from.bytes.length + 1, from.value)
        val feeBoxCandidate = new ErgoBoxCandidate(fee, Values.TrueLeaf, creationHeight = h)

        IndexedSeq(changeValue(from, -fee), Some(feeBoxCandidate)).flatten
      }, expectedValidity = true)
    }
  }

  property("too early spending") {
    forAll(unspendableErgoBoxGen()) { from =>
      constructTest(from, -1, h => {
        val fee = Math.min(Parameters.K * (h - 0) * from.bytes.length, from.value)
        val feeBoxCandidate = new ErgoBoxCandidate(fee, Values.TrueLeaf, creationHeight = h)
        IndexedSeq(changeValue(from, -fee), Some(feeBoxCandidate)).flatten
      }, expectedValidity = false)
    }
  }

  property("script changed spending w. same value") {
    forAll(unspendableErgoBoxGen()) { from =>
      val out = new ErgoBoxCandidate(from.value, Values.TrueLeaf, from.additionalTokens)
      constructTest(from, 0, _ => IndexedSeq(out), expectedValidity = false)
    }
  }

  property("script changed tokens w. same value") {
    forAll(unspendableErgoBoxGen()) { from =>
      whenever(from.additionalTokens.nonEmpty) {
        val out = new ErgoBoxCandidate(from.value, from.proposition, Seq())
        constructTest(from, 0, _ => IndexedSeq(out), expectedValidity = false)
      }
    }
  }

  //todo: test register change

  property("spending of whole coin when its value no more than storage fee") {
    forAll(unspendableErgoBoxGen(2, 200 * Parameters.K * Constants.StoragePeriod)) { from =>
      val outcome = from.value <= from.bytes.length * Parameters.K * Constants.StoragePeriod
      val out1 = new ErgoBoxCandidate(from.value - 1, Values.TrueLeaf)
      val out2 = new ErgoBoxCandidate(1, Values.FalseLeaf)
      constructTest(from, 0, _ => IndexedSeq(out1, out2), expectedValidity = outcome)
    }
  }
}