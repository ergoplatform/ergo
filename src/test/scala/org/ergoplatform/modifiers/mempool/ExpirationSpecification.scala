package org.ergoplatform.modifiers.mempool

import org.ergoplatform.nodeView.state.ErgoStateContext
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.ergoplatform.settings.{Constants, Parameters}
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.crypto.authds.ADDigest
import sigmastate.Values
import sigmastate.Values.ShortConstant
import sigmastate.interpreter.{ContextExtension, ProverResult}

class ExpirationSpecification extends ErgoPropertyTest {

  private val context = ErgoStateContext(0, ADDigest @@ Array.fill(32)(0: Byte))

  def falsify(box: ErgoBox): ErgoBox =
    ErgoBox(box.value, Values.FalseLeaf, box.additionalTokens, box.additionalRegisters)

  property("expiration - successful spending w. same value") {
    forAll(unspendableErgoBoxGen()) { toSpend =>

      val in = Input(toSpend.id,
        ProverResult(Array.emptyByteArray, ContextExtension(Map(Constants.StorageIndexVarId -> ShortConstant(0)))))

      val h = Constants.StoragePeriod

      val tx = ErgoTransaction(inputs = IndexedSeq(in),
        outputCandidates = IndexedSeq(updateHeight(toSpend, h)))

      val updContext = context.copy(height = h)

      tx.statelessValidity.isSuccess shouldBe true
      tx.statefulValidity(IndexedSeq(toSpend), updContext, settings.metadata).isSuccess shouldBe true
    }
  }

  property("expiration - successful spending w. max spending") {
    forAll(unspendableErgoBoxGen()) { from =>

      val in = Input(from.id,
        ProverResult(Array.emptyByteArray, ContextExtension(Map(Constants.StorageIndexVarId -> ShortConstant(0)))))

      val h = Constants.StoragePeriod

      val fee = Math.min(Parameters.K * (h - 0) * from.bytes.length, from.value)

      val feeBoxCondidate = new ErgoBoxCandidate(fee, Values.TrueLeaf, creationHeight = h)
      val tx = ErgoTransaction(inputs = IndexedSeq(in),
        outputCandidates = IndexedSeq(changeValue(updateHeight(from, h), -fee), Some(feeBoxCondidate)).flatten)

      val updContext = context.copy(height = h)

      tx.statelessValidity.isSuccess shouldBe true
      tx.statefulValidity(IndexedSeq(from), updContext, settings.metadata).isSuccess shouldBe true
    }
  }

  property("expiration - unsuccessful spending due too big storage fee charged") {
    forAll(unspendableErgoBoxGen(Parameters.K * (Constants.StoragePeriod - 0) * 100 + 1, Long.MaxValue)) { from =>

      val in = Input(from.id,
        ProverResult(Array.emptyByteArray, ContextExtension(Map(Constants.StorageIndexVarId -> ShortConstant(0)))))

      val h = Constants.StoragePeriod

      val fee = Math.min(Parameters.K * (h - 0) * from.bytes.length + 1, from.value)

      val feeBoxCandidate = new ErgoBoxCandidate(fee, Values.TrueLeaf, creationHeight = h)
      val tx = ErgoTransaction(inputs = IndexedSeq(in),
        outputCandidates = IndexedSeq(changeValue(updateHeight(from, h), -fee), Some(feeBoxCandidate)).flatten)

      val updContext = context.copy(height = h)

      tx.statelessValidity.isSuccess shouldBe true
      tx.statefulValidity(IndexedSeq(from), updContext, settings.metadata).isSuccess shouldBe false
    }
  }
}
