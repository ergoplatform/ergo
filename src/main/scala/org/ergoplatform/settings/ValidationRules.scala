package org.ergoplatform.settings

import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Extension, Header}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.core.validation.{MapValidationSettings, ModifierValidator, ValidationSettings}
import scorex.core.validation.ValidationResult.Invalid

object ValidationRules {

  // TODO update via soft-forks
  lazy val initialSettings: ValidationSettings = new MapValidationSettings(true, rulesSpec.map(r => r._1 -> (r._2._1, r._2._2)))

  /**
    * TODO
    *
    * check that we really need:
    * txBoxToSpend - should always work for correct client implementation
    * hdrGenesisNoneEmpty - should lead to troubles on launch
    * bsTooOld - check
    *
    * add Parameters.maxBlockSize rule
    *
    * looks like recoverable errors are implementation-specific
    *
    */
  lazy val rulesSpec: Map[Short, (String => Invalid, Boolean, Seq[Class[_]])] = Map(

    alreadyApplied -> (s => fatal(s"Modifier $s is already in history"), true, Seq(classOf[Header], classOf[ADProofs], classOf[Extension], classOf[BlockTransactions])),


    // stateless transaction validation
    txNoInputs -> (s => fatal(s"No inputs in transaction: $s"), true, Seq(classOf[ErgoTransaction])),
    txNoOutputs -> (s => fatal(s"No outputs in transaction: $s"), true, Seq(classOf[ErgoTransaction])),
    txManyInputs -> (s => fatal(s"Too many inputs in transaction: $s"), true, Seq(classOf[ErgoTransaction])),
    txManyOutputs -> (s => fatal(s"Too many outputs in transaction: $s"), true, Seq(classOf[ErgoTransaction])),
    txNegativeOutput -> (s => fatal(s"Transaction has an output with negative amount: $s"), true, Seq(classOf[ErgoTransaction])),
    txOutputsOverflow -> (s => fatal(s"Overflow in outputs: $s"), true, Seq(classOf[ErgoTransaction])),
    txAssetRules -> (s => fatal(s"Asset rules violated: $s"), true, Seq(classOf[ErgoTransaction])),

    // stateful transaction validation
    txCost -> (s => fatal(s"Transaction cost exceeds limit: $s"), true, Seq(classOf[ErgoTransaction])),
    txDust -> (s => fatal(s"Transaction is trying to create dust: $s"), true, Seq(classOf[ErgoTransaction])),
    txFuture -> (s => fatal(s"Box created in future: $s"), true, Seq(classOf[ErgoTransaction])),
    txBoxesToSpend -> (s => fatal(s"boxesToSpend.size != inputs.size: $s"), true, Seq(classOf[ErgoTransaction])),
    txDataBoxes -> (s => fatal(s"dataBoxes.size != dataInputs.size: $s"), true, Seq(classOf[ErgoTransaction])),
    txInputsSum -> (s => fatal(s"Overflow in inputs: $s"), true, Seq(classOf[ErgoTransaction])),
    txOutputSum -> (s => fatal(s"Overflow in outputs: $s"), true, Seq(classOf[ErgoTransaction])),
    txErgPreservation -> (s => fatal(s"Ergo token preservation is broken: $s"), true, Seq(classOf[ErgoTransaction])),
    txAssetsPreservation -> (s => fatal(s"Assets preservation rule is broken: $s"), true, Seq(classOf[ErgoTransaction])),
    txBoxToSpend -> (s => recoverable(s"Box id doesn't match input: $s"), true, Seq(classOf[ErgoTransaction])),
    txScriptValidation -> (s => fatal(s"Transaction validation failed on input: $s"), true, Seq(classOf[ErgoTransaction])),

    // header validation
    hdrGenesisParent -> (s => fatal(s"Genesis block should have genesis parent id: $s"), true, Seq(classOf[Header])),
    hdrGenesisFromConfig -> (s => fatal(s"Genesis header id does not correspond to id from the config: $s"), true, Seq(classOf[Header])),
    hdrGenesisNonEmpty -> (s => fatal(s"Trying to append genesis block to non-empty history: $s"), true, Seq(classOf[Header])),
    hdrGenesisHeight -> (s => fatal(s"Incorrect genesis height: $s"), true, Seq(classOf[Header])),
    hdrParent -> (s => recoverable(s"Parent header with id $s is not defined"), true, Seq(classOf[Header])),
    hdrVotes -> (s => fatal(s"Incorrect votes: $s"), true, Seq(classOf[Header])),
    hdrNonIncreasingTimestamp -> (s => fatal(s"Header timestamp is not greater than parents: $s"), true, Seq(classOf[Header])),
    hdrHeight -> (s => fatal(s"Header height is not greater by 1 than parents: $s"), true, Seq(classOf[Header])),
    hdrPoW -> (s => fatal(s"Incorrect PoW solution: $s"), true, Seq(classOf[Header])),
    hdrRequiredDifficulty -> (s => fatal(s"Incorrect required difficulty: $s"), true, Seq(classOf[Header])),
    hdrTooOld -> (s => fatal(s"Too old header: $s"), true, Seq(classOf[Header])),
    hdrParentSemantics -> (s => fatal(s"Parent header is marked as semantically invalid: $s"), true, Seq(classOf[Header])),
    hdrFutureTimestamp -> (s => fatal(s"Header timestamp is too far in future: $s"), true, Seq(classOf[Header])),

    // block sections validation
    bsNoHeader -> (s => recoverable(s"Header for modifier $s is not defined"), true, Seq(classOf[ADProofs], classOf[Extension], classOf[BlockTransactions])),
    bsCorrespondsToHeader -> (s => fatal(s"Modifier does not corresponds to header: $s"), true, Seq(classOf[ADProofs], classOf[Extension], classOf[BlockTransactions])),
    bsHeaderValid -> (s => fatal(s"Header $s is semantically invalid: "), true, Seq(classOf[ADProofs], classOf[Extension], classOf[BlockTransactions])),
    bsHeadersChainSynced -> (s => recoverable(s"Headers chain is not synchronized yet"), true, Seq(classOf[ADProofs], classOf[Extension], classOf[BlockTransactions])),
    bsTooOld -> (s => fatal(s"Too old block section: $s"), true, Seq(classOf[ADProofs], classOf[Extension], classOf[BlockTransactions])),

    // extension validation
    // interlinks validation
    exIlUnableToValidate -> (s => recoverable(s"Unable to validate interlinks: $s"), true, Seq(classOf[Extension])),
    exIlEncoding -> (s => fatal(s"Interlinks improperly packed: $s"), true, Seq(classOf[Extension])),
    exIlStructure -> (s => fatal(s"Interlinks are incorrect: $s"), true, Seq(classOf[Extension])),

    exKeyLength -> (s => fatal(s"Extension $s field key length is not ${Extension.FieldKeySize}"), true, Seq(classOf[Extension])),
    exValueLength -> (s => fatal(s"Extension $s field value length > ${Extension.FieldValueMaxSize}"), true, Seq(classOf[Extension])),
    exDuplicateKeys -> (s => fatal(s"Extension $s contains duplicate keys"), true, Seq(classOf[Extension])),
    exEmpty -> (s => fatal(s"Empty fields in non-genesis block: $s"), true, Seq(classOf[Extension])),


    Short.MaxValue -> (_ => recoverable("Deactivated check"), false, Seq())
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

  // block sections validation
  val alreadyApplied: Short = 300
  val bsNoHeader: Short = 301
  val bsCorrespondsToHeader: Short = 302
  val bsHeaderValid: Short = 303
  val bsHeadersChainSynced: Short = 304
  val bsTooOld: Short = 305

  // extension validation
  // validate interlinks
  val exIlUnableToValidate: Short = 310
  val exIlEncoding: Short = 311
  val exIlStructure: Short = 312
  // rest extension validation
  val exKeyLength: Short = 313
  val exValueLength: Short = 314
  val exDuplicateKeys: Short = 315
  val exEmpty: Short = 316

  private def recoverable(errorMessage: String): Invalid = ModifierValidator.error(errorMessage)

  private def fatal(errorMessage: String): Invalid = ModifierValidator.fatal(errorMessage)

}
