package org.ergoplatform.mining

import org.ergoplatform.mining.AutolykosPowScheme.derivedHeaderFields
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions}
import org.ergoplatform.modifiers.history.header.HeaderWithoutPow
import scorex.crypto.hash.Digest32

/**
  * Functions related to block candidate generation
  */
object CandidateUtils {
  /**
   * Derives header without pow from [[CandidateBlock]].
   */
  def deriveUnprovenHeader(candidate: CandidateBlock): HeaderWithoutPow = {
    val (parentId, height) = derivedHeaderFields(candidate.parentOpt)
    val transactionsRoot =
      BlockTransactions.transactionsRoot(candidate.transactions, candidate.version)
    val adProofsRoot = ADProofs.proofDigest(candidate.adProofBytes)
    val extensionRoot: Digest32 = candidate.extension.digest

    HeaderWithoutPow(
      candidate.version,
      parentId,
      adProofsRoot,
      candidate.stateRoot,
      transactionsRoot,
      candidate.timestamp,
      candidate.nBits,
      height,
      extensionRoot,
      candidate.votes
    )
  }
}
