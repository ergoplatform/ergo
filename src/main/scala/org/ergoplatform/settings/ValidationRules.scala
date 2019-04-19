package org.ergoplatform.settings

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.core.validation.ModifierValidator
import scorex.core.validation.ValidationResult.Invalid

object ValidationRules {

  /**
    * TODO check that we really need:
    *
    * txBoxToSpend - should always work for correct client implementation
    * hdrGenesisNoneEmpty - should lead to troubles on launch
    *
    */

  lazy val map: Map[Short, (String => Invalid, Boolean)] = Map(
    // stateless transaction validation
    txNoInputs -> (s => fatal(s"No inputs in transaction: $s"), true),
    txNoOutputs -> (s => fatal(s"No outputs in transaction: $s"), true),
    txManyInputs -> (s => fatal(s"Too many inputs in transaction: $s"), true),
    txManyOutputs -> (s => fatal(s"Too many outputs in transaction: $s"), true),
    txNegativeOutput -> (s => fatal(s"Transaction has an output with negative amount: $s"), true),
    txOutputsOverflow -> (s => fatal(s"Overflow in outputs: $s"), true),
    txAssetRules -> (s => fatal(s"Asset rules violated: $s"), true),

    // stateful transaction validation
    txCost -> (s => fatal(s"Transaction cost exceeds limit: $s"), true),
    txDust -> (s => fatal(s"Transaction is trying to create dust: $s"), true),
    txFuture -> (s => fatal(s"Box created in future: $s"), true),
    txBoxesToSpend -> (s => fatal(s"boxesToSpend.size != inputs.size: $s"), true),
    txDataBoxes -> (s => fatal(s"dataBoxes.size != dataInputs.size: $s"), true),
    txInputsSum -> (s => fatal(s"Overflow in inputs: $s"), true),
    txOutputSum -> (s => fatal(s"Overflow in outputs: $s"), true),
    txErgPreservation -> (s => fatal(s"Ergo token preservation is broken: $s"), true),
    txAssetsPreservation -> (s => fatal(s"Assets preservation rule is broken: $s"), true),
    txBoxToSpend -> (s => recoverable(s"Box id doesn't match input: $s"), true),
    txScriptValidation -> (s => fatal(s"Transaction validation failed on input: $s"), true),

    // header validation
    hdrGenesisParent -> (s => fatal(s"Genesis block should have genesis parent id: $s"), true),
    hdrGenesisFromConfig -> (s => fatal(s"Genesis header id does not correspond to id from the config: $s"), true),
    hdrGenesisNonEmpty -> (s => fatal(s"Trying to append genesis block to non-empty history: $s"), true),
    hdrGenesisHeight -> (s => fatal(s"Incorrect genesis height: $s"), true),
    hdrParent -> (s => recoverable(s"Parent header with id $s is not defined"), true),
    hdrVotes -> (s => fatal(s"Incorrect votes: $s"), true),
    hdrNonIncreasingTimestamp -> (s => fatal(s"Header timestamp is not greater than parents: $s"), true),
    hdrHeight -> (s => fatal(s"Header height is not greater by 1 than parents: $s"), true),
    hdrPoW -> (s => fatal(s"Incorrect PoW solution: $s"), true),
    hdrRequiredDifficulty -> (s => fatal(s"Incorrect required difficulty: $s"), true),
    hdrTooOld -> (s => fatal(s"Too old header: $s"), true),
    hdrParentSemantics -> (s => fatal(s"Parent header is marked as semantically invalid: $s"), true),
    hdrFutureTimestamp -> (s => fatal(s"Header timestamp is too far in future: $s"), true),
    hdrAlreadyApplied -> (s => fatal(s"Modifier $s is already in history"), true),



    Short.MaxValue -> (_ => recoverable("Deactivated check"), false)
  )


  // stateless transaction validation
  val txNoInputs: Short = 100
  val txNoOutputs: Short = 101
  val txManyInputs: Short = 102
  val txManyOutputs: Short = 103
  val txNegativeOutput: Short = 104
  val txOutputsOverflow: Short = 105
  val txAssetRules: Short = 106

  // stateful transaction validation
  val txCost: Short = 120
  val txDust: Short = 121
  val txFuture: Short = 122
  val txBoxesToSpend: Short = 123
  val txDataBoxes: Short = 124
  val txInputsSum: Short = 125
  val txOutputSum: Short = 126
  val txErgPreservation: Short = 127
  val txAssetsPreservation: Short = 128
  val txBoxToSpend: Short = 129
  val txScriptValidation: Short = 130

  // header validation
  val hdrGenesisParent: Short = 200
  val hdrGenesisFromConfig: Short = 201
  val hdrGenesisNonEmpty: Short = 202
  val hdrGenesisHeight: Short = 203
  val hdrVotes: Short = 204
  val hdrParent: Short = 205
  val hdrNonIncreasingTimestamp: Short = 206
  val hdrHeight: Short = 207
  val hdrPoW: Short = 208
  val hdrRequiredDifficulty: Short = 209
  val hdrTooOld: Short = 210
  val hdrParentSemantics: Short = 211
  val hdrFutureTimestamp: Short = 212
  val hdrAlreadyApplied: Short = 213


  private def recoverable(errorMessage: String): Invalid = ModifierValidator.error(errorMessage)

  private def fatal(errorMessage: String): Invalid = ModifierValidator.fatal(errorMessage)

}
