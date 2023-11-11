package org.ergoplatform.utils

import org.ergoplatform.ErgoBox.{AdditionalRegisters, TokenId}
import org.ergoplatform.settings.{Algos, Parameters}
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate}
import scorex.util.ModifierId
import sigma.{Coll, Colls}
import sigmastate.Values.ErgoTree
import sigmastate.eval._

object BoxUtils {

  /** Minimal amount for transaction for a box of maximum size*/
  @inline
  def sufficientAmount(parameters: Parameters): Long = ErgoBox.MaxBoxSize * parameters.minValuePerByte

  /** Used when complete instance of ErgoBox is unavailable. */
  @inline
  def minimalErgoAmountSimulated(script: ErgoTree,
                                 tokens: Coll[(TokenId, Long)] = Colls.emptyColl,
                                 additionalRegisters: AdditionalRegisters = Map(),
                                 parameters: Parameters): Long = {
    val candidateMock = new ErgoBoxCandidate(value = Long.MaxValue, script, creationHeight = Int.MaxValue, tokens, additionalRegisters)
    val mockId = ModifierId @@ Algos.encode(scorex.util.Random.randomBytes(32))
    minimalErgoAmount(candidateMock.toBox(mockId, 1), parameters)
  }

  @inline
  def minimalErgoAmountSimulated(script: ErgoTree, parameters: Parameters): Long =
    minimalErgoAmountSimulated(script, Colls.emptyColl, Map(), parameters)

  @inline
  def minimalErgoAmountSimulated(candidate: ErgoBoxCandidate, parameters: Parameters): Long =
    minimalErgoAmountSimulated(
      candidate.ergoTree,
      candidate.additionalTokens,
      candidate.additionalRegisters,
      parameters
    )

  @inline
  def minimalErgoAmount(box: ErgoBox, parameters: Parameters): Long = box.bytes.length * parameters.minValuePerByte
}
