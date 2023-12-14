package org.ergoplatform.modifiers.mempool

import org.ergoplatform._


case class UnsignedErgoTransaction(override val inputs: IndexedSeq[UnsignedInput],
                                   override val dataInputs: IndexedSeq[DataInput],
                                   override val outputCandidates: IndexedSeq[ErgoBoxCandidate])
  extends UnsignedErgoLikeTransaction(inputs, dataInputs, outputCandidates)

object UnsignedErgoTransaction {
  def apply(inputs: IndexedSeq[UnsignedInput],
            outputCandidates: IndexedSeq[ErgoBoxCandidate]): UnsignedErgoTransaction = {
    UnsignedErgoTransaction(inputs, IndexedSeq(), outputCandidates)
  }

  def apply(utx: UnsignedErgoLikeTransaction): UnsignedErgoTransaction =
    UnsignedErgoTransaction(utx.inputs, utx.dataInputs, utx.outputCandidates)

}
