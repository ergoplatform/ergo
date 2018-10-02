package org.ergoplatform.modifiers.mempool

import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.ergoplatform.settings.Constants
import sigmastate.Values
import sigmastate.Values.ShortConstant
import sigmastate.interpreter.{ContextExtension, ProverResult}

class ExpirationSpecification extends ErgoTransactionSpecification {

  property("expiration- successful spending") {

    def updateHeight(box: ErgoBoxCandidate, creationHeight: Long): ErgoBoxCandidate =
      new ErgoBoxCandidate(box.value, box.proposition, box.additionalTokens, box.additionalRegisters, creationHeight)

    def falsify(box: ErgoBox): ErgoBox =
      ErgoBox(box.value, Values.FalseLeaf, box.additionalTokens, box.additionalRegisters)

    forAll(unspendableErgoBoxCandidateGen) { candidate =>
      val from = candidate.toBox(scorex.util.bytesToId(Array.fill(32)(0: Byte)), 0)

      val in = Input(from.id,
        ProverResult(Array.emptyByteArray, ContextExtension(Map(Constants.StorageIndexVarId -> ShortConstant(0)))))

      val h = Constants.StoragePeriod

      val tx = ErgoTransaction(inputs = IndexedSeq(in),
        outputCandidates = IndexedSeq(updateHeight(candidate, h)))

      val updContext = context.copy(height = h)

      tx.statelessValidity.isSuccess shouldBe true
      tx.statefulValidity(IndexedSeq(from), updContext, settings.metadata).isSuccess shouldBe true
    }
  }
}
