package org.ergoplatform.modifiers.mempool

import org.ergoplatform.{ErgoBoxCandidate, ErgoLikeTransactionTemplate, UnsignedInput}
import scorex.crypto.hash.Blake2b256

class UnsignedErgoTransaction(override val inputs: IndexedSeq[UnsignedInput],
                              override val outputCandidates: IndexedSeq[ErgoBoxCandidate])
        extends ErgoLikeTransactionTemplate[UnsignedInput] {

  override lazy val serializedId: Array[Byte] = Blake2b256.hash(messageToSign)
}
