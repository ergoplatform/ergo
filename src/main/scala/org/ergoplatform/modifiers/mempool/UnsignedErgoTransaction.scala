package org.ergoplatform.modifiers.mempool

import org.ergoplatform.settings.Algos
import org.ergoplatform.{ErgoBoxCandidate, ErgoLikeTransactionTemplate, UnsignedInput}
import scorex.util.{ModifierId, bytesToId}

class UnsignedErgoTransaction(override val inputs: IndexedSeq[UnsignedInput],
                              override val outputCandidates: IndexedSeq[ErgoBoxCandidate])
  extends ErgoLikeTransactionTemplate[UnsignedInput] {

  lazy val serializedId: Array[Byte] = Algos.hash(messageToSign)

  override lazy val id: ModifierId = bytesToId(serializedId)
}
