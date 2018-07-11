package org.ergoplatform.modifiers.mempool

import org.ergoplatform.{ErgoBoxCandidate, ErgoLikeTransactionTemplate, UnsignedInput}
import scorex.core.ModifierId
import scorex.crypto.hash.Blake2b256

class UnsignedErgoTransaction(override val inputs: IndexedSeq[UnsignedInput],
                              override val outputCandidates: IndexedSeq[ErgoBoxCandidate])
        extends ErgoLikeTransactionTemplate[UnsignedInput] {

  override type IdType = ModifierId
  override lazy val id: ModifierId = ModifierId @@ Blake2b256.hash(messageToSign)


}
