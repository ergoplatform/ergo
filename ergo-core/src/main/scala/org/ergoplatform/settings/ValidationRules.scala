package org.ergoplatform.settings

import org.ergoplatform.SigmaConstants.{MaxBoxSize, MaxPropositionBytes}
import org.ergoplatform.modifiers.{ErgoFullBlock, NetworkObjectTypeId}
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistoryConstants._
import org.ergoplatform.wallet.boxes.ErgoBoxAssetExtractor
import org.ergoplatform.validation.{InvalidModifier, ModifierValidator}
import org.ergoplatform.validation.ValidationResult.Invalid
import scorex.util.ModifierId

object ValidationRules {

  /**
    * Contains description of all the validation rules.
    * Fatal errors are consensus-critical, while recoverable errors are implementation-specific.
    */
  lazy val rulesSpec: Map[Short, RuleStatus] = Map(

    alreadyApplied -> RuleStatus(im => fatal(s"Double application of a modifier is prohibited. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[Header], classOf[ADProofs], classOf[Extension], classOf[BlockTransactions]),
      mayBeDisabled = false),


    // transaction validation
    txNoInputs -> RuleStatus(im => fatal(s"A transaction should have at least one input. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txNoOutputs -> RuleStatus(im => fatal(s"A transaction should have at least one output. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txManyInputs -> RuleStatus(im => fatal(s"A number of transaction inputs should not exceed ${Short.MaxValue}. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txManyDataInputs -> RuleStatus(im => fatal(s"A number transaction data inputs should not exceed ${Short.MaxValue}. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txManyOutputs -> RuleStatus(im => fatal(s"A number of transaction outputs should not exceed ${Short.MaxValue}. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txNegativeOutput -> RuleStatus(im => fatal(s"Erg amount for a transaction output should not be negative. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txOutputSum -> RuleStatus(im => fatal(s"Sum of transaction output values should not exceed ${Long.MaxValue}. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txInputsUnique -> RuleStatus(im => fatal(s"There should be no duplicate inputs. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txAssetsInOneBox -> RuleStatus(im => fatal(s"A number of tokens within a box should not exceed ${ErgoBoxAssetExtractor.MaxAssetsPerBox}" +
      s" and sum of assets of one type should not exceed ${Long.MaxValue}. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txPositiveAssets -> RuleStatus(im => fatal(s"All token amounts of transaction outputs should be positive. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txDust -> RuleStatus(im => fatal(s"Every output of the transaction should contain at least <minValuePerByte * outputSize> nanoErgs. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = true),
    txFuture -> RuleStatus(im => fatal(s"Transaction outputs should have creationHeight not exceeding block height. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txBoxesToSpend -> RuleStatus(im => fatal(s"Every input of the transaction should be in UTXO. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txDataBoxes -> RuleStatus(im => fatal(s"Every data input of the transaction should be in UTXO. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txInputsSum -> RuleStatus(im => fatal(s"Sum of transaction inputs should not exceed ${Long.MaxValue}. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txErgPreservation -> RuleStatus(im => fatal(s"Amount of Ergs in inputs should be equal to amount of Erg in outputs. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txAssetsPreservation -> RuleStatus(im => fatal(s"For every token, its amount in outputs should not exceed its amount in inputs. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txBoxToSpend -> RuleStatus(im => fatal(s"Box id doesn't match the input. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = true),
    txScriptValidation -> RuleStatus(im => fatal(s"Scripts of all transaction inputs should pass verification. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txBoxSize -> RuleStatus(im => fatal(s"Box size should not exceed ${MaxBoxSize.value}. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = true),
    txBoxPropositionSize -> RuleStatus(im => fatal(s"Box proposition size should not exceed ${MaxPropositionBytes.value}. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = true),
    txNegHeight -> RuleStatus(im => fatal(s"Transaction outputs should have non-negative creationHeight. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = false),
    txReemission -> RuleStatus(im => fatal(s"Transaction should conform EIP-27 rules ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = true),
    txMonotonicHeight -> RuleStatus(im => fatal(s"Creation height of any output should be not less than  ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[ErgoTransaction]),
      mayBeDisabled = true),

    // header validation
    hdrGenesisParent -> RuleStatus(im => fatal(s"Genesis header should have genesis parent id. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[Header]),
      mayBeDisabled = false),
    hdrGenesisFromConfig -> RuleStatus(im => fatal(s"Genesis header id should be equal to id from the config. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[Header]),
      mayBeDisabled = false),
    hdrGenesisHeight -> RuleStatus(im => fatal(s"Genesis height should be ${GenesisHeight}. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[Header]),
      mayBeDisabled = false),
    hdrParent -> RuleStatus(im => recoverable(s"Parent header with id ${im.error} is not defined", im.modifierId, im.modifierTypeId),
      Seq(classOf[Header]),
      mayBeDisabled = false),
    hdrNonIncreasingTimestamp -> RuleStatus(im => fatal(s"Header timestamp should be greater than the parent's. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[Header]),
      mayBeDisabled = false),
    hdrHeight -> RuleStatus(im => fatal(s"A header height should be greater by one than the parent's. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[Header]),
      mayBeDisabled = false),
    hdrPoW -> RuleStatus(im => fatal(s"A header should contain correct PoW solution. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[Header]),
      mayBeDisabled = false),
    hdrRequiredDifficulty -> RuleStatus(im => fatal(s"A header should contain correct required difficulty. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[Header]),
      mayBeDisabled = false),
    hdrTooOld -> RuleStatus(im => fatal(s"A header height should not be older than current height minus <config.keepVersions>. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[Header]),
      mayBeDisabled = false),
    hdrParentSemantics -> RuleStatus(im => fatal(s"Parent header should not be marked as invalid. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[Header]),
      mayBeDisabled = false),
    hdrFutureTimestamp -> RuleStatus(im => recoverable(s"Header timestamp should not be more than 20 minutes in the future. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[Header]),
      mayBeDisabled = false),

    hdrVotesNumber -> RuleStatus(im => fatal(s"Number of non-zero votes should be <= ${Parameters.ParamVotesCount}. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[Header]),
      mayBeDisabled = true),
    hdrVotesDuplicates -> RuleStatus(im => fatal(s"A header votes should contain no duplicates. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[Header]),
      mayBeDisabled = false),
    hdrVotesContradictory -> RuleStatus(im => fatal(s"A header votes should contain no contradictory votes. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[Header]),
      mayBeDisabled = false),
    hdrVotesUnknown -> RuleStatus(im => fatal(s"First header of an epoch should not contain a vote for unknown parameter. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[Header]),
      mayBeDisabled = true),
    hdrCheckpoint -> RuleStatus(im => fatal(s"Chain is failing checkpoint validation. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[Header]),
      mayBeDisabled = false),

    // block sections validation
    bsNoHeader -> RuleStatus(im => recoverable(s"A header for a modifier ${im.error} is not defined", im.modifierId, im.modifierTypeId),
      Seq(classOf[ADProofs], classOf[Extension], classOf[BlockTransactions]),
      mayBeDisabled = false),
    bsCorrespondsToHeader -> RuleStatus(im => fatal(s"Block sections should correspond to the declared header. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[ADProofs], classOf[Extension], classOf[BlockTransactions]),
      mayBeDisabled = false),
    bsHeaderValid -> RuleStatus(im => fatal(s"A header for the block section should not be marked as invalid. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[ADProofs], classOf[Extension], classOf[BlockTransactions]),
      mayBeDisabled = false),
    bsHeadersChainSynced -> RuleStatus(im => recoverable(s"Headers-chain is not synchronized yet. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[ADProofs], classOf[Extension], classOf[BlockTransactions]),
      mayBeDisabled = false),
    bsTooOld -> RuleStatus(im => fatal(s"Block section should correspond to a block header that is not pruned yet. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[ADProofs], classOf[Extension], classOf[BlockTransactions]),
      mayBeDisabled = false),
    bsBlockTransactionsSize -> RuleStatus(im => fatal(s"Size of block transactions section should not exceed <maxBlockSize>. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[BlockTransactions]),
      mayBeDisabled = true),
    bsBlockTransactionsCost -> RuleStatus(im => fatal(s"Accumulated cost of block transactions should not exceed <maxBlockCost>. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[ErgoTransaction], classOf[BlockTransactions]),
      mayBeDisabled = false),

    // full block processing validation
    fbOperationFailed -> RuleStatus(im => fatal(s"Operations against the state AVL+ tree should be successful. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[ErgoFullBlock]),
      mayBeDisabled = false),
    fbDigestIncorrect -> RuleStatus(im => fatal(s"Calculated AVL+ digest should be equal to one written in the block header. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[ErgoFullBlock]),
      mayBeDisabled = false),

    // extension validation
    // interlinks validation
    exIlUnableToValidate -> RuleStatus(im => recoverable(s"Unable to validate interlinks. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[Extension]),
      mayBeDisabled = true),
    exIlEncoding -> RuleStatus(im => fatal(s"Interlinks should be packed properly. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[Extension]),
      mayBeDisabled = true),
    exIlStructure -> RuleStatus(im => fatal(s"Interlinks should have the correct structure. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[Extension]),
      mayBeDisabled = true),
    exKeyLength -> RuleStatus(im => fatal(s"Extension fields key length should be ${Extension.FieldKeySize}. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[Extension]),
      mayBeDisabled = false),
    exValueLength -> RuleStatus(im => fatal(s"Extension field value length should be <= ${Extension.FieldValueMaxSize}. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[Extension]),
      mayBeDisabled = true),
    exDuplicateKeys -> RuleStatus(im => fatal(s"An extension should not contain duplicate keys. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[Extension]),
      mayBeDisabled = true),
    exCheckForkVote -> RuleStatus(im => fatal(s"Voting for fork could be started only after activation period of a previous soft-fork. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[Extension]),
      mayBeDisabled = true),
    exParseParameters -> RuleStatus(im => fatal(s"At the beginning of the epoch, the extension should contain correctly packed parameters. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[Extension]),
      mayBeDisabled = true),
    exMatchParameters -> RuleStatus(im => fatal(s"At the beginning of the epoch, the extension should contain all the system parameters. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[Extension]),
      mayBeDisabled = true),
    exBlockVersion -> RuleStatus(im => fatal(s"Versions in header and parameters section should be equal. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[Extension]),
      mayBeDisabled = true),
    exParseValidationSettings -> RuleStatus(im => fatal(s"At the beginning of the epoch, the extension should contain correctly packed validation settings. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[Extension]),
      mayBeDisabled = true),
    exMatchValidationSettings -> RuleStatus(im => fatal(s"At the beginning of the epoch, the extension should contain all the validation settings. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[Extension]),
      mayBeDisabled = true),
    exSize -> RuleStatus(im => fatal(s"Size of extension section should not exceed ${Constants.MaxExtensionSize}. ${im.error}", im.modifierId, im.modifierTypeId),
      Seq(classOf[Extension]),
      mayBeDisabled = true),

    exEmpty -> RuleStatus(im => fatal(s"Extension of non-genesis block should not be empty. ${im.error}", im.modifierId, im.modifierTypeId),
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
  val txNegHeight: Short = 122 // introduced in v2 blocks
  val txReemission: Short = 123 // introduced in EIP-27 (soft-fork)
  val txMonotonicHeight: Short = 124 // introduced in v3 blocks

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


  def errorMessage(id: Short, details: String, modifierId: ModifierId, modifierTypeId: NetworkObjectTypeId.Value): String = {
    ValidationRules.rulesSpec(id)
      .invalidMod(InvalidModifier(details, modifierId, modifierTypeId))
      .errors
      .last
      .message
  }

  private def recoverable(errorMessage: String, modifierId: ModifierId, modifierTypeId: NetworkObjectTypeId.Value): Invalid =
    ModifierValidator.error(errorMessage, modifierId, modifierTypeId)

  private def fatal(error: String, modifierId: ModifierId, modifierTypeId: NetworkObjectTypeId.Value): Invalid =
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
case class RuleStatus(invalidMod: InvalidModifier => Invalid,
                      affectedClasses: Seq[Class[_]],
                      mayBeDisabled: Boolean,
                      isActive: Boolean)

object RuleStatus {
  def apply(invalidMod: InvalidModifier => Invalid, affectedClasses: Seq[Class[_]], mayBeDisabled: Boolean): RuleStatus = {
    RuleStatus(invalidMod, affectedClasses, mayBeDisabled, isActive = true)
  }

}
