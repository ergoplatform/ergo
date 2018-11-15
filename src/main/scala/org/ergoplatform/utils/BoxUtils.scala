package org.ergoplatform.utils

import org.ergoplatform.ErgoBox.{NonMandatoryRegisterId, TokenId}
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate}
import org.ergoplatform.settings.{Algos, Parameters}
import scorex.util.ModifierId
import sigmastate.SType
import sigmastate.Values.{EvaluatedValue, Value}

object BoxUtils {

  /** Used when complete instance of ErgoBox is unavailable. */
  @inline
  def minimalErgoAmountSimulated(script: Value[sigmastate.SBoolean.type],
                                 tokens: Seq[(TokenId, Long)] = Seq(),
                                 additionalRegisters: Map[NonMandatoryRegisterId, _ <: EvaluatedValue[_ <: SType]] = Map(),
                                 parameters: Parameters): Long = {
    val candidateMock = new ErgoBoxCandidate(Long.MaxValue, script, tokens, additionalRegisters, creationHeight = Int.MaxValue)
    val mockId = ModifierId @@ Algos.encode(scorex.util.Random.randomBytes(32))
    minimalErgoAmount(candidateMock.toBox(mockId, 1), parameters)
  }

  @inline
  def minimalErgoAmountSimulated(script: Value[sigmastate.SBoolean.type], parameters: Parameters): Long =
    minimalErgoAmountSimulated(script, Seq(), Map(), parameters)

  @inline
  def minimalErgoAmountSimulated(candidate: ErgoBoxCandidate, parameters: Parameters): Long =
    minimalErgoAmountSimulated(
      candidate.proposition,
      candidate.additionalTokens,
      candidate.additionalRegisters,
      parameters
    )

  @inline
  def minimalErgoAmount(box: ErgoBox, parameters: Parameters): Long = box.bytes.length * parameters.minValuePerByte
}
