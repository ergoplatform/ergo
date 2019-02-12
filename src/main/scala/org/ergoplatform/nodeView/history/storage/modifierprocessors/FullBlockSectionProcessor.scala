package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.settings.Algos
import scorex.core.consensus.History.ProgressInfo
import scorex.core.utils.ScorexEncoding
import scorex.core.validation.{ModifierValidator, RecoverableModifierError, ValidationResult, ValidationState}
import scorex.util.{ModifierId, bytesToId}

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

    private def validateExtension(extension: Extension, header: Header): ValidationState[Unit] = {
      validateInterlinks(extension, header) {
        failFast
          .validate(extension.fields.forall(_._1.lengthCompare(Extension.FieldKeySize) == 0)) {
            fatal(s"Extension ${extension.encodedId} field key length is not ${Extension.FieldKeySize}")
          }
          .validate(extension.fields.forall(_._2.lengthCompare(Extension.FieldValueMaxSize) <= 0)) {
            fatal(s"Extension ${extension.encodedId} field value length > ${Extension.FieldValueMaxSize}")
          }
          .validate(extension.fields.map(kv => bytesToId(kv._1)).distinct.length == extension.fields.length) {
            fatal(s"Extension ${extension.encodedId} contains duplicate keys")
          }
          .validate(header.isGenesis || extension.fields.nonEmpty) {
            fatal("Empty fields in non-genesis block")
          }
      }
    }

    private def validateInterlinks(extension: Extension, header: Header)
                                  (vs: ValidationState[Unit]): ValidationState[Unit] = {
      import PoPowAlgos._
      val prevHeaderOpt = typedModifierById[Header](header.parentId)
      val prevExtensionOpt = prevHeaderOpt.flatMap(parent => typedModifierById[Extension](parent.extensionId))
      (prevHeaderOpt, prevExtensionOpt) match {
        case (Some(parent), Some(parentExt)) =>
          vs.validate(
            updateInterlinks(parent, unpackInterlinks(parentExt.fields)) == unpackInterlinks(extension.fields)) {
            fatal("Invalid interlinks")
          }
        case _ =>
          vs.validate(header.isGenesis || header.height == pruningProcessor.minimalFullBlockHeight) {
            error("Unable to validate interlinks")
          }
      }
    }

    /**
      * Validation specific to concrete type of block payload
      */
    private def modifierSpecificValidation(m: BlockSection, header: Header): ValidationState[Unit] = {
      m match {
        case extension: Extension =>
          validateExtension(extension, header)
        case _ =>
          failFast
      }
    }

  }

}
