package org.ergoplatform.utils

import org.ergoplatform.ErgoBox.{NonMandatoryRegisterId, TokenId}
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate}
import org.ergoplatform.settings.{Algos, Parameters}
import scorex.util.ModifierId
import sigmastate.SType
import sigmastate.Values.{EvaluatedValue, Value}

object BoxUtils {

  /** Used when completed ErgoBox is unavailable. */
  @inline
  def minimalErgoAmountSimulated(script: Value[sigmastate.SBoolean.type],
                                 tokens: Seq[(TokenId, Long)] = Seq(),
                                 additionalRegisters: Map[NonMandatoryRegisterId, _ <: EvaluatedValue[_ <: SType]] = Map()): Long = {
    val candidateMock = new ErgoBoxCandidate(value = Long.MaxValue, script, creationHeight = Int.MaxValue, tokens, additionalRegisters)
    val mockId = ModifierId @@ Algos.encode(scorex.util.Random.randomBytes(32))
    minimalErgoAmount(candidateMock.toBox(mockId, 1))
  }

  @inline
  def minimalErgoAmountSimulated(candidate: ErgoBoxCandidate): Long = {
    minimalErgoAmountSimulated(candidate.proposition, candidate.additionalTokens, candidate.additionalRegisters)
  }

  @inline
  def minimalErgoAmount(box: ErgoBox): Long = box.bytes.length * Parameters.MinValuePerByte
}
