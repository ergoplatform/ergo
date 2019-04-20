package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.settings.Algos
import org.ergoplatform.settings.ValidationRules._
import scorex.core.consensus.History.ProgressInfo
import scorex.core.utils.ScorexEncoding
import scorex.core.validation.{ModifierValidator, _}
import scorex.util.{ModifierId, bytesToId}

import scala.reflect.ClassTag
import scala.util.{Failure, Try}

/**
  * Trait that implements BlockSectionProcessor interfaces for regimes where the node
  * downloads and process full blocks.
  */
trait FullBlockSectionProcessor extends BlockSectionProcessor with FullBlockProcessor {

  protected def vs: ValidationSettings

  private def validationState: ValidationState[Unit] = ModifierValidator(vs)

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
    typedModifierById[Header](m.headerId) map { header =>
      new PayloadValidator(validationState).validate(m, header).toTry
    } getOrElse {
      validationState.validate(bsNoHeader, condition = false, Algos.encode(m.id)).result.toTry
    }
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
    * Validator for BlockTransactions, ADProofs and Extension
    */
  class PayloadValidator(validator: ValidationState[Unit]) extends ScorexEncoding {

    def validate(m: BlockSection, header: Header): ValidationResult[Unit] = {
      modifierSpecificValidation(m, header)
        .validate(alreadyApplied,!historyStorage.contains(m.id), s"${m.encodedId}")
        .validate(bsCorrespondsToHeader, header.isCorrespondingModifier(m), s"header=${header.encodedId}, id=${m.encodedId}")
        .validateSemantics(bsHeaderValid, isSemanticallyValid(header.id), s"header=${header.encodedId}, id=${m.encodedId}")
        .validate(bsHeadersChainSynced, isHeadersChainSynced)
        .validate(bsTooOld, isHistoryADProof(m, header) || pruningProcessor.shouldDownloadBlockAtHeight(header.height), s"header=${header.encodedId}, id=${m.encodedId}")
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
          val parentLinksTry = unpackInterlinks(parentExt.fields)
          val currentLinksTry = unpackInterlinks(extension.fields)
          vs
            .validate(currentLinksTry.isSuccess)(fatal("Interlinks improperly packed"))
            .validate(parentLinksTry.flatMap(parLinks => currentLinksTry.map(parLinks -> _))
              .toOption.exists { case (prev, cur) => updateInterlinks(parent, prev) == cur }) {
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
        case p: ADProofs =>
        case _ =>
          failFast
      }
    }

  }

}
