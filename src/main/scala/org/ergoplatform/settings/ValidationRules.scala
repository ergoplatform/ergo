package org.ergoplatform.settings

import org.ergoplatform.SigmaConstants.{MaxBoxSize, MaxPropositionBytes}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.wallet.boxes.ErgoBoxAssetExtractor
import scorex.core.ModifierTypeId
import scorex.core.validation.{InvalidModifierDetails, ModifierValidator}
import scorex.core.validation.ValidationResult.Invalid
import scorex.util.ModifierId

object ValidationRules {

  /**
    * Contains description of all the validation rules.
    * Fatal errors are consensus-critical, while recoverable errors are implementation-specific.
    */
  lazy val rulesSpec: Map[Short, RuleStatus] = Map(

    alreadyApplied -> RuleStatus(d => fatal(s"Double application of a modifier is prohibited. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[Header], classOf[ADProofs], classOf[Extension], classOf[BlockTransactions]),
      mayBeDisabled = false),


    // transaction validation
    txNoInputs -> RuleStatus(d => fatal(s"A transaction should have at least one input. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txNoOutputs -> RuleStatus(d => fatal(s"A transaction should have at least one output. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txManyInputs -> RuleStatus(d => fatal(s"A number of transaction inputs should not exceed ${Short.MaxValue}. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txManyDataInputs -> RuleStatus(d => fatal(s"A number transaction data inputs should not exceed ${Short.MaxValue}. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txManyOutputs -> RuleStatus(d => fatal(s"A number of transaction outputs should not exceed ${Short.MaxValue}. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txNegativeOutput -> RuleStatus(d => fatal(s"Erg amount for a transaction output should not be negative. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txOutputSum -> RuleStatus(d => fatal(s"Sum of transaction output values should not exceed ${Long.MaxValue}. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txInputsUnique -> RuleStatus(d => fatal(s"There should be no duplicate inputs. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txAssetsInOneBox -> RuleStatus(d => fatal(s"A number of tokens within a box should not exceed ${ErgoBoxAssetExtractor.MaxAssetsPerBox}" +
      s" and sum of assets of one type should not exceed ${Long.MaxValue}. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txPositiveAssets -> RuleStatus(d => fatal(s"All token amounts of transaction outputs should be positive. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txDust -> RuleStatus(d => fatal(s"Every output of the transaction should contain at least <minValuePerByte * outputSize> nanoErgs. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = true),
    txFuture -> RuleStatus(d => fatal(s"Transaction outputs should have creationHeight not exceeding block height. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txBoxesToSpend -> RuleStatus(d => fatal(s"Every input of the transaction should be in UTXO. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txDataBoxes -> RuleStatus(d => fatal(s"Every data input of the transaction should be in UTXO. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txInputsSum -> RuleStatus(d => fatal(s"Sum of transaction inputs should not exceed ${Long.MaxValue}. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txErgPreservation -> RuleStatus(d => fatal(s"Amount of Ergs in inputs should be equal to amount of Erg in outputs. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txAssetsPreservation -> RuleStatus(d => fatal(s"For every token, its amount in outputs should not exceed its amount in inputs. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txBoxToSpend -> RuleStatus(d => fatal(s"Box id doesn't match the input. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = true),
    txScriptValidation -> RuleStatus(d => fatal(s"Scripts of all transaction inputs should pass verification. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txBoxSize -> RuleStatus(d => fatal(s"Box size should not exceed ${MaxBoxSize.value}. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = true),
    txBoxPropositionSize -> RuleStatus(d => fatal(s"Box proposition size should not exceed ${MaxPropositionBytes.value}. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = true),
    txNegHeight -> RuleStatus(d => fatal(s"Transaction outputs should have non-negative creationHeight. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txReemission -> RuleStatus(d => fatal(s"Transaction should conform EIP-27 rules ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = true),

    // header validation
    hdrGenesisParent -> RuleStatus(d => fatal(s"Genesis header should have genesis parent id. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[Header]),
      mayBeDisabled = false),
    hdrGenesisFromConfig -> RuleStatus(d => fatal(s"Genesis header id should be equal to id from the config. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[Header]),
      mayBeDisabled = false),
    hdrGenesisHeight -> RuleStatus(d => fatal(s"Genesis height should be ${ErgoHistory.GenesisHeight}. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[Header]),
      mayBeDisabled = false),
    hdrParent -> RuleStatus(d => recoverable(s"Parent header with id ${d.error} is not defined", d.modifierId, d.modifierTypeId),
      Seq(classOf[Header]),
      mayBeDisabled = false),
    hdrNonIncreasingTimestamp -> RuleStatus(d => fatal(s"Header timestamp should be greater than the parent's. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[Header]),
      mayBeDisabled = false),
    hdrHeight -> RuleStatus(d => fatal(s"A header height should be greater by one than the parent's. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[Header]),
      mayBeDisabled = false),
    hdrPoW -> RuleStatus(d => fatal(s"A header should contain correct PoW solution. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[Header]),
      mayBeDisabled = false),
    hdrRequiredDifficulty -> RuleStatus(d => fatal(s"A header should contain correct required difficulty. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[Header]),
      mayBeDisabled = false),
    hdrTooOld -> RuleStatus(d => fatal(s"A header height should not be older than current height minus <config.keepVersions>. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[Header]),
      mayBeDisabled = false),
    hdrParentSemantics -> RuleStatus(d => fatal(s"Parent header should not be marked as invalid. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[Header]),
      mayBeDisabled = false),
    hdrFutureTimestamp -> RuleStatus(d => recoverable(s"Header timestamp should not be more than 20 minutes in the future. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[Header]),
      mayBeDisabled = false),

    hdrVotesNumber -> RuleStatus(d => fatal(s"Number of non-zero votes should be <= ${Parameters.ParamVotesCount}. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[Header]),
      mayBeDisabled = true),
    hdrVotesDuplicates -> RuleStatus(d => fatal(s"A header votes should contain no duplicates. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[Header]),
      mayBeDisabled = false),
    hdrVotesContradictory -> RuleStatus(d => fatal(s"A header votes should contain no contradictory votes. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[Header]),
      mayBeDisabled = false),
    hdrVotesUnknown -> RuleStatus(d => fatal(s"First header of an epoch should not contain a vote for unknown parameter. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[Header]),
      mayBeDisabled = true),
    hdrCheckpoint -> RuleStatus(d => fatal(s"Chain is failing checkpoint validation. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[Header]),
      mayBeDisabled = false),

    // block sections validation
    bsNoHeader -> RuleStatus(d => recoverable(s"A header for a modifier ${d.error} is not defined", d.modifierId, d.modifierTypeId),
      Seq(classOf[ADProofs], classOf[Extension], classOf[BlockTransactions]),
      mayBeDisabled = false),
    bsCorrespondsToHeader -> RuleStatus(d => fatal(s"Block sections should correspond to the declared header. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[ADProofs], classOf[Extension], classOf[BlockTransactions]),
      mayBeDisabled = false),
    bsHeaderValid -> RuleStatus(d => fatal(s"A header for the block section should not be marked as invalid. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[ADProofs], classOf[Extension], classOf[BlockTransactions]),
      mayBeDisabled = false),
    bsHeadersChainSynced -> RuleStatus(d => recoverable(s"Headers-chain is not synchronized yet. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[ADProofs], classOf[Extension], classOf[BlockTransactions]),
      mayBeDisabled = false),
    bsTooOld -> RuleStatus(d => fatal(s"Block section should correspond to a block header that is not pruned yet. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[ADProofs], classOf[Extension], classOf[BlockTransactions]),
      mayBeDisabled = false),
    bsBlockTransactionsSize -> RuleStatus(d => fatal(s"Size of block transactions section should not exceed <maxBlockSize>. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[BlockTransactions]),
      mayBeDisabled = true),
    bsBlockTransactionsCost -> RuleStatus(d => fatal(s"Accumulated cost of block transactions should not exceed <maxBlockCost>. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[ErgoTransaction], classOf[BlockTransactions]),
      mayBeDisabled = false),

    // full block processing validation
    fbOperationFailed -> RuleStatus(d => fatal(s"Operations against the state AVL+ tree should be successful. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[ErgoFullBlock]),
      mayBeDisabled = false),
    fbDigestIncorrect -> RuleStatus(d => fatal(s"Calculated AVL+ digest should be equal to one written in the block header. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[ErgoFullBlock]),
      mayBeDisabled = false),

    // extension validation
    // interlinks validation
    exIlUnableToValidate -> RuleStatus(d => recoverable(s"Unable to validate interlinks. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[Extension]),
      mayBeDisabled = true),
    exIlEncoding -> RuleStatus(d => fatal(s"Interlinks should be packed properly. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[Extension]),
      mayBeDisabled = true),
    exIlStructure -> RuleStatus(d => fatal(s"Interlinks should have the correct structure. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[Extension]),
      mayBeDisabled = true),
    exKeyLength -> RuleStatus(d => fatal(s"Extension fields key length should be ${Extension.FieldKeySize}. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[Extension]),
      mayBeDisabled = false),
    exValueLength -> RuleStatus(d => fatal(s"Extension field value length should be <= ${Extension.FieldValueMaxSize}. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[Extension]),
      mayBeDisabled = true),
    exDuplicateKeys -> RuleStatus(d => fatal(s"An extension should not contain duplicate keys. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[Extension]),
      mayBeDisabled = true),
    exCheckForkVote -> RuleStatus(d => fatal(s"Voting for fork could be started only after activation period of a previous soft-fork. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[Extension]),
      mayBeDisabled = true),
    exParseParameters -> RuleStatus(d => fatal(s"At the beginning of the epoch, the extension should contain correctly packed parameters. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[Extension]),
      mayBeDisabled = true),
    exMatchParameters -> RuleStatus(d => fatal(s"At the beginning of the epoch, the extension should contain all the system parameters. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[Extension]),
      mayBeDisabled = true),
    exBlockVersion -> RuleStatus(d => fatal(s"Versions in header and parameters section should be equal. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[Extension]),
      mayBeDisabled = true),
    exParseValidationSettings -> RuleStatus(d => fatal(s"At the beginning of the epoch, the extension should contain correctly packed validation settings. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[Extension]),
      mayBeDisabled = true),
    exMatchValidationSettings -> RuleStatus(d => fatal(s"At the beginning of the epoch, the extension should contain all the validation settings. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[Extension]),
      mayBeDisabled = true),
    exSize -> RuleStatus(d => fatal(s"Size of extension section should not exceed ${Constants.MaxExtensionSize}. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[Extension]),
      mayBeDisabled = true),

    exEmpty -> RuleStatus(d => fatal(s"Extension of non-genesis block should not be empty. ${d.error}", d.modifierId, d.modifierTypeId),
      Seq(classOf[Extension]),
      mayBeDisabled = true)
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
  // stateful transaction validation
  val txDust: Short = 111
  val txFuture: Short = 112
  val txBoxesToSpend: Short = 113
  val txDataBoxes: Short = 114
  val txInputsSum: Short = 115
  val txErgPreservation: Short = 116
  val txAssetsPreservation: Short = 117
  val txBoxToSpend: Short = 118
  val txScriptValidation: Short = 119
  val txBoxSize: Short = 120
  val txBoxPropositionSize: Short = 121
  val txNegHeight: Short = 122
  val txReemission: Short = 123

  // header validation
  val hdrGenesisParent: Short = 200
  val hdrGenesisFromConfig: Short = 201
  val hdrGenesisHeight: Short = 203
  val hdrParent: Short = 204
  val hdrNonIncreasingTimestamp: Short = 205
  val hdrHeight: Short = 206
  val hdrPoW: Short = 207
  val hdrRequiredDifficulty: Short = 208
  val hdrTooOld: Short = 209
  val hdrParentSemantics: Short = 210
  val hdrFutureTimestamp: Short = 211
  val hdrVotesNumber: Short = 212
  val hdrVotesDuplicates: Short = 213
  val hdrVotesContradictory: Short = 214
  val hdrVotesUnknown: Short = 215
  val hdrCheckpoint: Short = 216

  // block sections validation
  val alreadyApplied: Short = 300
  val bsNoHeader: Short = 301
  val bsCorrespondsToHeader: Short = 302
  val bsHeaderValid: Short = 303
  val bsHeadersChainSynced: Short = 304
  val bsTooOld: Short = 305
  val bsBlockTransactionsSize: Short = 306
  val bsBlockTransactionsCost: Short = 307

  // extension validation
  val exSize: Short = 400
  val exIlEncoding: Short = 401
  val exIlStructure: Short = 402
  val exKeyLength: Short = 403
  val exValueLength: Short = 404
  val exDuplicateKeys: Short = 405
  val exEmpty: Short = 406
  val exCheckForkVote: Short = 407
  val exParseParameters: Short = 408
  val exMatchParameters: Short = 409
  val exBlockVersion: Short = 410
  val exParseValidationSettings: Short = 411
  val exMatchValidationSettings: Short = 412
  val exIlUnableToValidate: Short = 413

  // full block application
  val fbOperationFailed: Short = 500
  val fbDigestIncorrect: Short = 501


  def errorMessage(id: Short, details: String, modifierId: ModifierId, modifierTypeId: ModifierTypeId): String = {
    ValidationRules.rulesSpec(id)
      .invalidMod(InvalidModifierDetails(details, modifierId, modifierTypeId))
      .errors
      .last
      .message
  }

  private def recoverable(errorMessage: String, modifierId: ModifierId, modifierTypeId: ModifierTypeId): Invalid =
    ModifierValidator.error(errorMessage, modifierId, modifierTypeId)

  private def fatal(error: String, modifierId: ModifierId, modifierTypeId: ModifierTypeId): Invalid =
    ModifierValidator.fatal(error, modifierId, modifierTypeId)
}

/**
  * Status of validation rule.
  * The only mutable parameter is `isActive`
  *
  * @param invalidMod      - function that construct validation error from details
  * @param affectedClasses - modifiers, that are validated via this rule
  * @param mayBeDisabled   - whether rule may be disabled via soft-fork
  * @param isActive        - whether rule is active right now
  */
case class RuleStatus(invalidMod: InvalidModifierDetails => Invalid,
                      affectedClasses: Seq[Class[_]],
                      mayBeDisabled: Boolean,
                      isActive: Boolean)

object RuleStatus {
  def apply(invalidMod: InvalidModifierDetails => Invalid, affectedClasses: Seq[Class[_]], mayBeDisabled: Boolean): RuleStatus = {
    RuleStatus(invalidMod, affectedClasses, mayBeDisabled, isActive = true)
  }

}
