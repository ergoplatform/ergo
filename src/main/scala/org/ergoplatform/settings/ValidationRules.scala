package org.ergoplatform.settings

import scorex.core.validation.ModifierValidator
import scorex.core.validation.ValidationResult.Invalid

object ValidationRules {

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


    txId -> (s => fatal(s"Incorrect transaction id: $s"), true),


    10.toShort -> (_ => recoverable("Deactivated check"), false)
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
  val txCost: Short = 121
  val txDust: Short = 122
  val txFuture: Short = 123
  val txBoxesToSpend: Short = 124
  val txDataBoxes: Short = 125
  val txInputsSum: Short = 126
  val txOutputSum: Short = 127
  val txErgPreservation: Short = 128
  val txAssetsPreservation: Short = 129
  val txBoxToSpend: Short = 130
  val txScriptValidation: Short = 131


  val txId: Short = 199

  private def recoverable(errorMessage: String): Invalid = ModifierValidator.error(errorMessage)

  private def fatal(errorMessage: String): Invalid = ModifierValidator.fatal(errorMessage)

}
