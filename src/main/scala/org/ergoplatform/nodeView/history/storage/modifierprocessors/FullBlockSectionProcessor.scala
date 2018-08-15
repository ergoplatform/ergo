package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Extension, Header}
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.settings.Algos
import scorex.core.ModifierId
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
      val minimalHeight = m match {
        case proofs: ADProofs if contains(header.transactionsId) =>
          // ADProofs for block transactions that are already in history. Do not validate whether ADProofs are too old
          -1
        case _ => pruningProcessor.minimalFullBlockHeight
      }

      PayloadValidator.validate(m, header, minimalHeight).toTry
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
      case mod: T if m.id sameElements id => Some(mod)
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

    def validate(m: BlockSection, header: Header, minimalHeight: Int): ValidationResult[Unit] = {
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
        .validate(header.height >= minimalHeight) {
          fatal(s"Too old modifier ${m.encodedId}: ${header.height} < $minimalHeight")
        }
        .validate(!historyStorage.contains(m.id)) {
          error(s"Modifier ${m.encodedId} is already in history")
        }
        .result
    }

    /**
      * Validation specific to concrete type of block payload
      */
    private def modifierSpecificValidation(m: BlockSection, header: Header): ValidationState[Unit] = {
      m match {
        case e: Extension =>
          // todo checks that all required mandatory fields are set
          // todo checks number of mandatory fields
          // todo checks number of optional fields
          failFast
            .validate(e.mandatoryFields.forall(_._1.lengthCompare(Extension.MandatoryFieldKeySize) == 0)) {
              fatal(s"Extension ${m.encodedId} mandatory field key is not ${Extension.MandatoryFieldKeySize}")
            }
            .validate(e.optionalFields.forall(_._1.lengthCompare(Extension.OptionalFieldKeySize) == 0)) {
              fatal(s"Extension ${m.encodedId} mandatory field key is not ${Extension.OptionalFieldKeySize}")
            }
        case _ => failFast
      }
    }
  }

}
