package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock, NonHeaderBlockSection}
import org.ergoplatform.settings.ValidationRules._
import org.ergoplatform.settings.{Algos, ErgoValidationSettings}
import scorex.core.consensus.ProgressInfo
import scorex.core.utils.ScorexEncoding
import scorex.core.validation.{ModifierValidator, _}
import scorex.db.ByteArrayWrapper
import scorex.util.ModifierId

import scala.reflect.ClassTag
import scala.util.Try

/**
  * Trait that implements BlockSectionProcessor interfaces for regimes where the node
  * downloads and process full blocks.
  */
trait FullBlockSectionProcessor extends BlockSectionProcessor with FullBlockProcessor {

  private def initialValidationState: ValidationState[Unit] = ModifierValidator(ErgoValidationSettings.initial)

  /**
    * Process block section.
    * If modifier is ADProofs in UTXO mode - just put to storage, we should already process this full block.
    * Otherwise - try to construct full block with this block section, if possible - process this new full block,
    * if not - just put new block section to storage.
    */
  override protected def process(m: NonHeaderBlockSection): Try[ProgressInfo[BlockSection]] = {
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

  override protected def validate(m: NonHeaderBlockSection): Try[Unit] = {
    typedModifierById[Header](m.headerId).map(header =>
      new PayloadValidator().validate(m, header)
    ).getOrElse(
      // Block section can not be validated without a corresponding header
      initialValidationState
        .validate(bsNoHeader, condition = false, InvalidModifier(s"Block section id: ${Algos.encode(m.id)}", m.id, m.modifierTypeId))
        .result
    ).toTry
  }

  /**
    * Trying to construct full block with modifier `m` and data, kept in history
    *
    * @param m - new modifier
    * @return Some(ErgoFullBlock) if block construction is possible, None otherwise
    */
  private def getFullBlockByBlockSection(m: NonHeaderBlockSection): Option[ErgoFullBlock] = {
    def getOrRead[T <: BlockSection : ClassTag](id: ModifierId): Option[T] = m match {
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

  private def justPutToHistory(m: NonHeaderBlockSection): Try[ProgressInfo[BlockSection]] = {
    historyStorage.insert(Array.empty[(ByteArrayWrapper, Array[Byte])], Array[BlockSection](m)).map { _ =>
      ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
    }
  }

  /**
    * Validator for BlockTransactions, ADProofs and Extension
    */
  class PayloadValidator extends ScorexEncoding {

    def validate(m: NonHeaderBlockSection, header: Header): ValidationResult[Unit] = {
      initialValidationState
        .validate(alreadyApplied, !historyStorage.contains(m.id), InvalidModifier(m.encodedId, m.id, m.modifierTypeId))
        .validate(bsCorrespondsToHeader, header.isCorrespondingModifier(m), InvalidModifier(s"header=${header.encodedId}, id=${m.encodedId}", m.id, m.modifierTypeId))
        .validateSemantics(bsHeaderValid, isSemanticallyValid(header.id), InvalidModifier(s"header=${header.encodedId}, id=${m.encodedId}", m.id, m.modifierTypeId))
        .validate(bsHeadersChainSynced, isHeadersChainSynced, InvalidModifier(header.id, m.id, m.modifierTypeId))
        .validate(bsTooOld, isHistoryADProof(m, header) || shouldDownloadBlockAtHeight(header.height),
          InvalidModifier(s"header=${header.encodedId}, id=${m.encodedId}", m.id, m.modifierTypeId))
        .result
    }

    private def isHistoryADProof(m: NonHeaderBlockSection, header: Header): Boolean = m match {
      // ADProofs for block transactions that are already in history. Do not validate whether ADProofs are too old
      case _: ADProofs if contains(header.transactionsId) => true
      case _ => false
    }

  }

}
