package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Extension, Header}
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.settings.Algos
import scorex.util.{ModifierId, bytesToId}
import scorex.core.consensus.History.ProgressInfo
import scorex.core.utils.ScorexEncoding
import scorex.core.validation.{ModifierValidator, RecoverableModifierError, ValidationResult, ValidationState}

import scala.reflect.ClassTag
import scala.util.{Failure, Try}

/**
  * Trait that implements BlockSectionProcessor interfaces for regimes where the node
  * downloads and process full blocks.
  */
trait FullBlockSectionProcessor extends BlockSectionProcessor with FullBlockProcessor {

  /**
    * Process block section.
    * If modifier is ADProofs in UTXO mode - just put to storage, we should already process this full block.
    * Otherwise - try to construct full block with this block section, if possible - process this new full block,
    * if not - just put new block section to storage.
    */
  override protected def process(m: BlockSection): ProgressInfo[ErgoPersistentModifier] = {
    m match {
      case _: ADProofs if !requireProofs =>
        // got proofs in UTXO mode. Don't need to try to update better chain
        justPutToHistory(m)
      case _ =>
        getFullBlockByBlockSection(m) match {
          case Some(fb: ErgoFullBlock) =>
            processFullBlock(fb, m)
          case _ =>
            justPutToHistory(m)
        }
    }
  }

  override protected def validate(m: BlockSection): Try[Unit] = {
    typedModifierById[Header](m.headerId).map { header =>
      PayloadValidator.validate(m, header).toTry
    }.getOrElse(Failure(new RecoverableModifierError(s"Header for modifier $m is not defined")))
  }

  /**
    * Trying to construct full block with modifier `m` and data, kept in history
    *
    * @param m - new modifier
    * @return Some(ErgoFullBlock) if block construction is possible, None otherwise
    */
  private def getFullBlockByBlockSection(m: BlockSection): Option[ErgoFullBlock] = {
    def getOrRead[T <: ErgoPersistentModifier : ClassTag](id: ModifierId): Option[T] = m match {
      case mod: T if m.id == id => Some(mod)
      case _ => typedModifierById[T](id)
    }

    typedModifierById[Header](m.headerId).flatMap { h =>
      getOrRead[BlockTransactions](h.transactionsId).flatMap { txs =>
        getOrRead[Extension](h.extensionId).flatMap { e =>
          if (!requireProofs) {
            Some(ErgoFullBlock(h, txs, e, None))
          } else {
            getOrRead[ADProofs](h.ADProofsId).flatMap { p =>
              Some(ErgoFullBlock(h, txs, e, Some(p)))
            }
          }
        }
      }
    }
  }


  private def justPutToHistory(m: BlockSection): ProgressInfo[ErgoPersistentModifier] = {
    historyStorage.insert(Algos.idToBAW(m.id), Seq.empty, Seq(m))
    ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
  }

  /**
    * Validator for BlockTransactions and ADProofs
    */
  object PayloadValidator extends ModifierValidator with ScorexEncoding {

    def validate(m: BlockSection, header: Header): ValidationResult[Unit] = {
      modifierSpecificValidation(m, header)
        .validate(header.isCorrespondingModifier(m)) {
          fatal(s"Modifier ${m.encodedId} does not corresponds to header ${header.encodedId}")
        }
        .validateSemantics(isSemanticallyValid(header.id)) {
          fatal(s"Header ${header.encodedId} for modifier ${m.encodedId} is semantically invalid")
        }
        .validate(isHeadersChainSynced) {
          error("Headers chain is not synced yet")
        }
        .validate(isHistoryADProof(m, header) || pruningProcessor.shouldDownloadBlockAtHeight(header.height)) {
          fatal(s"Too old modifier ${m.encodedId}: " +
                s"${header.height} < ${pruningProcessor.minimalFullBlockHeight}")
        }
        .validate(!historyStorage.contains(m.id)) {
          error(s"Modifier ${m.encodedId} is already in history")
        }
        .result
    }

    private def isHistoryADProof(m: BlockSection, header: Header): Boolean = m match {
      // ADProofs for block transactions that are already in history. Do not validate whether ADProofs are too old
      case _: ADProofs if contains(header.transactionsId) => true
      case _ => false
    }

    /**
      * Validation specific to concrete type of block payload
      */
    private def modifierSpecificValidation(m: BlockSection, header: Header): ValidationState[Unit] = {
      m match {
        case e: Extension =>
          // todo checks that all required mandatory fields are set and non additional mandatory fields
          failFast
            .validate(e.optionalFields.lengthCompare(Extension.MaxOptionalFields) <= 0) {
              fatal(s"Extension ${m.encodedId} have too many optional fields")
            }
            .validate(e.mandatoryFields.forall(_._1.lengthCompare(Extension.MandatoryFieldKeySize) == 0)) {
              fatal(s"Extension ${m.encodedId} mandatory field key length is not ${Extension.MandatoryFieldKeySize}")
            }
            .validate(e.optionalFields.forall(_._1.lengthCompare(Extension.OptionalFieldKeySize) == 0)) {
              fatal(s"Extension ${m.encodedId} optional field key length is not ${Extension.OptionalFieldKeySize}")
            }
            .validate(e.mandatoryFields.forall(_._2.lengthCompare(Extension.MaxMandatoryFieldValueSize) <= 0)) {
              fatal(s"Extension ${m.encodedId} mandatory field value length > ${Extension.MaxMandatoryFieldValueSize}")
            }
            .validate(e.optionalFields.forall(_._2.lengthCompare(Extension.MaxOptionalFieldValueSize) <= 0)) {
              fatal(s"Extension ${m.encodedId} optional field value length > ${Extension.MaxOptionalFieldValueSize}")
            }
            .validate(e.mandatoryFields.map(kv => bytesToId(kv._1)).distinct.length == e.mandatoryFields.length) {
              //todo this check may be done in general mandatory fields check
              fatal(s"Extension ${m.encodedId} contains duplicate mandatory keys")
            }
            .validate(e.optionalFields.map(kv => bytesToId(kv._1)).distinct.length == e.optionalFields.length) {
              fatal(s"Extension ${m.encodedId} contains duplicate optionalFields keys")
            }
        case _ =>
          // todo some validations of block transactions, including size limit, should go there.
          failFast
      }
    }
  }

}
