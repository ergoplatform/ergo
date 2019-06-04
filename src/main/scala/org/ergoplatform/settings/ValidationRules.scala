package org.ergoplatform.settings

import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Extension, Header}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory
import scorex.core.validation.ModifierValidator
import scorex.core.validation.ValidationResult.Invalid

object ValidationRules {

  // TODO update via soft-forks
  lazy val initialSettings: ErgoValidationSettings = ErgoValidationSettings(rulesSpec)

  /**
    * Contains description of all the validation rules.
    * Fatal errors are consensus-critical, while recoverable errors are implementation-specific.
    */
  lazy val rulesSpec: Map[Short, RuleStatus] = Map(

    alreadyApplied -> RuleStatus(s => fatal(s"A modifier should not be applied yet. $s"),
      Seq(classOf[Header], classOf[ADProofs], classOf[Extension], classOf[BlockTransactions])),


    // stateless transaction validation
    txNoInputs -> RuleStatus(s => fatal(s"A transaction should have at least one input. $s"),
      Seq(classOf[ErgoTransaction])),
    txNoOutputs -> RuleStatus(s => fatal(s"A transaction should have at least one output. $s"),
      Seq(classOf[ErgoTransaction])),
    txManyInputs -> RuleStatus(s => fatal(s"A number of transaction inputs should not exceed ${Short.MaxValue}. $s"),
      Seq(classOf[ErgoTransaction])),
    txManyDataInputs -> RuleStatus(s => fatal(s"A number transaction data inputs should not exceed ${Short.MaxValue}. $s"),
      Seq(classOf[ErgoTransaction])),
    txManyOutputs -> RuleStatus(s => fatal(s"A number of transaction outputs should not exceed ${Short.MaxValue}. $s"),
      Seq(classOf[ErgoTransaction])),
    txNegativeOutput -> RuleStatus(s => fatal(s"Erg amount for a transaction output should not be negative. $s"),
      Seq(classOf[ErgoTransaction])),
    txOutputSum -> RuleStatus(s => fatal(s"Sum of transaction output values should not exceed ${Long.MaxValue}. $s"),
      Seq(classOf[ErgoTransaction])),
    txInputsUnique -> RuleStatus(s => fatal(s"There should be no duplicate inputs. $s"),
      Seq(classOf[ErgoTransaction])),
    txAssetsInOneBox -> RuleStatus(s => fatal(s"A number of tokens within a box should not exceed ${ErgoTransaction.MaxAssetsPerBox}" +
      s" and sum of assets of one type should not exceed ${Long.MaxValue}. $s"),
      Seq(classOf[ErgoTransaction])),
    txPositiveAssets -> RuleStatus(s => fatal(s"All token amounts of transaction outputs should be positive. $s"),
      Seq(classOf[ErgoTransaction])),

    // stateful transaction validation
    txCost -> RuleStatus(s => fatal(s"The total cost of transaction input scripts should not exceed <maxBlockCost>. $s"),
      Seq(classOf[ErgoTransaction])),
    txDust -> RuleStatus(s => fatal(s"Every output of the transaction should contain at least <minValuePerByte * outputSize> nanoErgs. $s"),
      Seq(classOf[ErgoTransaction])),
    txFuture -> RuleStatus(s => fatal(s"Transaction outputs should have creationHeight the does not exceed the block height. $s"),
      Seq(classOf[ErgoTransaction])),
    txBoxesToSpend -> RuleStatus(s => fatal(s"Every input of the transaction should be in UTXO. $s"),
      Seq(classOf[ErgoTransaction])),
    txDataBoxes -> RuleStatus(s => fatal(s"Every data input of the transaction should be in UTXO. $s"),
      Seq(classOf[ErgoTransaction])),
    txInputsSum -> RuleStatus(s => fatal(s"Sum of transaction inputs should not exceed ${Long.MaxValue}. $s"),
      Seq(classOf[ErgoTransaction])),
    txErgPreservation -> RuleStatus(s => fatal(s"Amount of Ergs in inputs should be equal to amount of Erg in outputs. $s"),
      Seq(classOf[ErgoTransaction])),
    txAssetsPreservation -> RuleStatus(s => fatal(s"For every token, its amount in inputs should not exceed its amount in outputs. $s"),
      Seq(classOf[ErgoTransaction])),
    txBoxToSpend -> RuleStatus(s => recoverable(s"Box id doesn't match the input. $s"),
      Seq(classOf[ErgoTransaction])),
    txScriptValidation -> RuleStatus(s => fatal(s"Scripts of all transaction inputs should pass verification. $s"),
      Seq(classOf[ErgoTransaction])),

    // header validation
    hdrGenesisParent -> RuleStatus(s => fatal(s"Genesis header should have genesis parent id. $s"),
      Seq(classOf[Header])),
    hdrGenesisFromConfig -> RuleStatus(s => fatal(s"Genesis header id should be equal to id from the config. $s"),
      Seq(classOf[Header])),
    hdrGenesisHeight -> RuleStatus(s => fatal(s"Genesis height should be ${ErgoHistory.GenesisHeight}. $s"),
      Seq(classOf[Header])),
    hdrParent -> RuleStatus(s => recoverable(s"Parent header with id $s is not defined"),
      Seq(classOf[Header])),
    hdrVotes -> RuleStatus(s => fatal(s"A header should contain three votes, with no duplicates and contradictory votes. $s"),
      Seq(classOf[Header])),
    hdrNonIncreasingTimestamp -> RuleStatus(s => fatal(s"Header timestamp should be greater than the parent's. $s"),
      Seq(classOf[Header])),
    hdrHeight -> RuleStatus(s => fatal(s"A header height should be greater by one than the parent's. $s"),
      Seq(classOf[Header])),
    hdrPoW -> RuleStatus(s => fatal(s"A header should contain correct PoW solution. $s"),
      Seq(classOf[Header])),
    hdrRequiredDifficulty -> RuleStatus(s => fatal(s"A header should contain correct required difficulty. $s"),
      Seq(classOf[Header])),
    hdrTooOld -> RuleStatus(s => fatal(s"A header height should not be older than current height minus <config.keepVersions>. $s"),
      Seq(classOf[Header])),
    hdrParentSemantics -> RuleStatus(s => fatal(s"Parent header should not be marked as invalid. $s"),
      Seq(classOf[Header])),
    hdrFutureTimestamp -> RuleStatus(s => recoverable(s"Header timestamp should not be more than 20 minutes in the future. $s"),
      Seq(classOf[Header])),

    // block sections validation
    bsNoHeader -> RuleStatus(s => recoverable(s"A header for a modifier $s is not defined"),
      Seq(classOf[ADProofs], classOf[Extension], classOf[BlockTransactions])),
    bsCorrespondsToHeader -> RuleStatus(s => fatal(s"Block sections should correspond to the declared header. $s"),
      Seq(classOf[ADProofs], classOf[Extension], classOf[BlockTransactions])),
    bsHeaderValid -> RuleStatus(s => fatal(s"A header for the block section should not be marked as invalid. $s"),
      Seq(classOf[ADProofs], classOf[Extension], classOf[BlockTransactions])),
    bsHeadersChainSynced -> RuleStatus(s => recoverable(s"Headers-chain is not synchronized yet"),
      Seq(classOf[ADProofs], classOf[Extension], classOf[BlockTransactions])),
    bsTooOld -> RuleStatus(s => fatal(s"Block section should correspond to a block header that is not pruned yet. $s"),
      Seq(classOf[ADProofs], classOf[Extension], classOf[BlockTransactions])),
    fbOperationFailed -> RuleStatus(s => fatal(s"Operations against the state AVL+ tree should be successful. $s"),
      Seq(classOf[ErgoFullBlock])),
    fbDigestIncorrect -> RuleStatus(s => fatal(s"Calculated AVL+ digest should be equal to one written in the block header. $s"),
      Seq(classOf[ErgoFullBlock])),

    // extension validation
    // interlinks validation
    exIlUnableToValidate -> RuleStatus(s => recoverable(s"Unable to validate interlinks. $s"),
      Seq(classOf[Extension])),
    exIlEncoding -> RuleStatus(s => fatal(s"Interlinks should be packed properly. $s"),
      Seq(classOf[Extension])),
    exIlStructure -> RuleStatus(s => fatal(s"Interlinks should have the correct structure. $s"),
      Seq(classOf[Extension])),

    exKeyLength -> RuleStatus(s => fatal(s"Extension fields key length should be ${Extension.FieldKeySize}. $s"),
      Seq(classOf[Extension])),
    exValueLength -> RuleStatus(s => fatal(s"Extension field value length should be <= ${Extension.FieldValueMaxSize}. $s"),
      Seq(classOf[Extension])),
    exDuplicateKeys -> RuleStatus(s => fatal(s"An extension should not contain duplicate keys. $s"),
      Seq(classOf[Extension])),
    exEmpty -> RuleStatus(s => fatal(s"Extension of non-genesis block should not be empty. $s"),
      Seq(classOf[Extension]))
  )


  // stateless transaction validation
  val txNoInputs: Short = 100
  val txNoOutputs: Short = 101
  val txManyInputs: Short = 102
  val txManyDataInputs: Short = 103
  val txManyOutputs: Short = 104
  val txNegativeOutput: Short = 105
  val txOutputSum: Short = 106
  val txInputsUnique: Short = 107
  val txPositiveAssets: Short = 108
  val txAssetsInOneBox: Short = 109
  val txRegistersCount: Short = 120
  val txMaxBoxSize: Short = 121
  val txRegisterData: Short = 122

  // stateful transaction validation
  val txCost: Short = 110
  val txDust: Short = 111
  val txFuture: Short = 112
  val txBoxesToSpend: Short = 113
  val txDataBoxes: Short = 114
  val txInputsSum: Short = 115
  val txErgPreservation: Short = 116
  val txAssetsPreservation: Short = 117
  val txBoxToSpend: Short = 118
  val txScriptValidation: Short = 119

  // header validation
  val hdrGenesisParent: Short = 200
  val hdrGenesisFromConfig: Short = 201
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

  // full block application
  val fbOperationFailed: Short = 400
  val fbDigestIncorrect: Short = 401


  def errorMessage(id: Short, details: String): String = {
    ValidationRules.rulesSpec(id)
      .error(details)
      .errors
      .last
      .message
  }

  private def recoverable(errorMessage: String): Invalid = ModifierValidator.error(errorMessage)
  private def fatal(errorMessage: String): Invalid = ModifierValidator.fatal(errorMessage)
}

/**
  * Status of validation rule.
  * The only mutable parameter is `isActive`
  *
  * @param error - function that construct validation error from details string
  * @param affectedClasses - modifiers, that are validated via this rule
  * @param isActive - whether rule is active or not
  */
case class RuleStatus(error: String => Invalid, affectedClasses: Seq[Class[_]], isActive: Boolean)

object RuleStatus {
  def apply(error: String => Invalid, affectedClasses: Seq[Class[_]]): RuleStatus = {
    RuleStatus(error, affectedClasses, isActive = true)
  }
}