package org.ergoplatform.settings

import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Extension, Header}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory
import scorex.core.validation.{MapValidationSettings, ModifierValidator, ValidationSettings}
import scorex.core.validation.ValidationResult.Invalid

object ValidationRules {

  // TODO update via soft-forks
  lazy val initialSettings: ValidationSettings = new MapValidationSettings(true, rulesSpec.map(r => r._1 -> (r._2._1, r._2._2)))

  /**
    * TODO
    *
    * txBoxToSpend - should always work for correct client implementation
    * hdrGenesisNoneEmpty - should lead to troubles on launch
    * bsTooOld - check
    * hdrFutureTimestamp - is it fatal?
    *
    * add Parameters.maxBlockSize rule
    *
    * split hdrVotes into multiple checks?
    * split hdrPoW into multiple checks?
    *
    * looks like recoverable errors are implementation-specific
    * exEmpty - do we need it?
    *
    */
  lazy val rulesSpec: Map[Short, (String => Invalid, Boolean, Seq[Class[_]])] = Map(

    alreadyApplied -> (s => fatal(s"Modifier should not be applied yet. $s"), true, Seq(classOf[Header], classOf[ADProofs], classOf[Extension], classOf[BlockTransactions])),


    // stateless transaction validation
    txNoInputs -> (s => fatal(s"Transaction should have at least one input. $s"), true, Seq(classOf[ErgoTransaction])),
    txNoOutputs -> (s => fatal(s"Transaction should have at least one output. $s"), true, Seq(classOf[ErgoTransaction])),
    txManyInputs -> (s => fatal(s"Number of transaction inputs should not exceed ${Short.MaxValue}. $s"), true, Seq(classOf[ErgoTransaction])),
    txManyDataInputs -> (s => fatal(s"Number of transaction data inputs should not exceed ${Short.MaxValue}. $s"), true, Seq(classOf[ErgoTransaction])),
    txManyOutputs -> (s => fatal(s"Number of transaction outputs should not exceed ${Short.MaxValue}. $s"), true, Seq(classOf[ErgoTransaction])),
    txNegativeOutput -> (s => fatal(s"Amounts of transaction outputs should not be negative. $s"), true, Seq(classOf[ErgoTransaction])),
    txOutputSum -> (s => fatal(s"Sum of transaction outputs should not exceed ${Long.MaxValue}. $s"), true, Seq(classOf[ErgoTransaction])),
    txInputsUnique -> (s => fatal(s"There should be no duplicate inputs. $s"), true, Seq(classOf[ErgoTransaction])),
    txAssetRules -> (s => fatal(s"Number of assets in a single output should not exceed ${ErgoTransaction.MaxAssetsPerBox}, " +
      s"amount of every individual asset should be positive and sum of assets of one type should " +
      s"not exceed ${Long.MaxValue}. $s"), true, Seq(classOf[ErgoTransaction])),

    // stateful transaction validation
    txCost -> (s => fatal(s"Total cost of transaction scripts should not exceed <maxBlockCost>. $s"), true, Seq(classOf[ErgoTransaction])),
    txDust -> (s => fatal(s"Every output of the transaction should contain at least <minValuePerByte * outputSize> nanoErg. $s"), true, Seq(classOf[ErgoTransaction])),
    txFuture -> (s => fatal(s"Every output of the transaction should have creationHeight less or equals than height of block with this transaction. $s"), true, Seq(classOf[ErgoTransaction])),
    txBoxesToSpend -> (s => fatal(s"Every input of the transaction should be present in UTXO. $s"), true, Seq(classOf[ErgoTransaction])),
    txDataBoxes -> (s => fatal(s"Every data input of the transaction should be present in UTXO. $s"), true, Seq(classOf[ErgoTransaction])),
    txInputsSum -> (s => fatal(s"Sum of transaction inputs should not exceed ${Long.MaxValue}. $s"), true, Seq(classOf[ErgoTransaction])),
    txErgPreservation -> (s => fatal(s"Amount of Erg in inputs should equal to amount of Erg in outputs. $s"), true, Seq(classOf[ErgoTransaction])),
    txAssetsPreservation -> (s => fatal(s"For every token, its amount in inputs should not exceed its amount of Erg in outputs. $s"), true, Seq(classOf[ErgoTransaction])),
    txBoxToSpend -> (s => recoverable(s"Box id doesn't match input. $s"), true, Seq(classOf[ErgoTransaction])),
    txScriptValidation -> (s => fatal(s"Scripts of all transaction inputs should pass verification. $s"), true, Seq(classOf[ErgoTransaction])),

    // header validation
    hdrGenesisParent -> (s => fatal(s"Genesis header should have genesis parent id. $s"), true, Seq(classOf[Header])),
    hdrGenesisFromConfig -> (s => fatal(s"Genesis header id should equals to to id from the config. $s"), true, Seq(classOf[Header])),
    hdrGenesisHeight -> (s => fatal(s"Genesis height should be ${ErgoHistory.GenesisHeight}. $s"), true, Seq(classOf[Header])),
    hdrParent -> (s => recoverable(s"Parent header with id $s is not defined"), true, Seq(classOf[Header])),
    hdrVotes -> (s => fatal(s"Header should contain 3 votes, with no duplicates and contradictory votes, that should be known by parametersDescs. $s"), true, Seq(classOf[Header])),
    hdrNonIncreasingTimestamp -> (s => fatal(s"Block timestamp should be greater than parents. $s"), true, Seq(classOf[Header])),
    hdrHeight -> (s => fatal(s"Header height should be greater by 1 than parents. $s"), true, Seq(classOf[Header])),
    hdrPoW -> (s => fatal(s"Header should contain correct PoW solution. $s"), true, Seq(classOf[Header])),
    hdrRequiredDifficulty -> (s => fatal(s"Header should contain correct required difficulty. $s"), true, Seq(classOf[Header])),
    hdrTooOld -> (s => fatal(s"Header height should not be older, than current height minus <config.keepVersions>. $s"), true, Seq(classOf[Header])),
    hdrParentSemantics -> (s => fatal(s"Parent header should not be marked as invalid. $s"), true, Seq(classOf[Header])),
    hdrFutureTimestamp -> (s => fatal(s"Header timestamp should not be more than 20 minutes in future. $s"), true, Seq(classOf[Header])),

    // block sections validation
    bsNoHeader -> (s => recoverable(s"Header for modifier $s is not defined"), true, Seq(classOf[ADProofs], classOf[Extension], classOf[BlockTransactions])),
    bsCorrespondsToHeader -> (s => fatal(s"Block sections should correspond to declared header. $s"), true, Seq(classOf[ADProofs], classOf[Extension], classOf[BlockTransactions])),
    bsHeaderValid -> (s => fatal(s"Header for block section should not be marked as invalid. $s"), true, Seq(classOf[ADProofs], classOf[Extension], classOf[BlockTransactions])),
    bsHeadersChainSynced -> (s => recoverable(s"Headers chain is not synchronized yet"), true, Seq(classOf[ADProofs], classOf[Extension], classOf[BlockTransactions])),
    bsTooOld -> (s => fatal(s"Too old block section. $s"), true, Seq(classOf[ADProofs], classOf[Extension], classOf[BlockTransactions])),
    fbOperationFailed -> (s => fatal(s"Operation applied to AVL+ tree should pass. $s"), true, Seq(classOf[ErgoFullBlock])),
    fbDigestIncorrect -> (s => fatal(s"Calucated AVL+ digest should be eqaul to the declared one. $s"), true, Seq(classOf[ErgoFullBlock])),

    // extension validation
    // interlinks validation
    exIlUnableToValidate -> (s => recoverable(s"Unable to validate interlinks. $s"), true, Seq(classOf[Extension])),
    exIlEncoding -> (s => fatal(s"Interlinks should be packed properly. $s"), true, Seq(classOf[Extension])),
    exIlStructure -> (s => fatal(s"Interlinks should have correct structure. $s"), true, Seq(classOf[Extension])),

    exKeyLength -> (s => fatal(s"Extension fields key length should be ${Extension.FieldKeySize}. $s"), true, Seq(classOf[Extension])),
    exValueLength -> (s => fatal(s"Extension field value length > ${Extension.FieldValueMaxSize}. $s"), true, Seq(classOf[Extension])),
    exDuplicateKeys -> (s => fatal(s"Extension should not contain duplicate keys. $s"), true, Seq(classOf[Extension])),
    exEmpty -> (s => fatal(s"Extension of non-genesis block should not be empty. $s"), true, Seq(classOf[Extension]))
  )


  // stateless transaction validation
  val txNoInputs: Short = 100
  val txNoOutputs: Short = 101
  val txManyInputs: Short = 102
  val txManyDataInputs: Short = 103
  val txManyOutputs: Short = 104
  val txNegativeOutput: Short = 105
  val txOutputSum: Short = 106
  val txAssetRules: Short = 107
  val txInputsUnique: Short = 108

  // stateful transaction validation
  val txCost: Short = 120
  val txDust: Short = 121
  val txFuture: Short = 122
  val txBoxesToSpend: Short = 123
  val txDataBoxes: Short = 124
  val txInputsSum: Short = 125
  val txErgPreservation: Short = 126
  val txAssetsPreservation: Short = 127
  val txBoxToSpend: Short = 128
  val txScriptValidation: Short = 129

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


  private def recoverable(errorMessage: String): Invalid = ModifierValidator.error(errorMessage)

  private def fatal(errorMessage: String): Invalid = ModifierValidator.fatal(errorMessage)

}
