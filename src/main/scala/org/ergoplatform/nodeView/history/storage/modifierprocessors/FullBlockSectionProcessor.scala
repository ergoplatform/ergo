package org.ergoplatform.nodeView.history.storage.modifierprocessors

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header}
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock, ErgoPersistentModifier}
import scorex.core.consensus.History.ProgressInfo
import scorex.core.consensus.ModifierSemanticValidity.Invalid
import scorex.core.utils.ScorexEncoding
import scorex.core.validation.{ModifierValidator, RecoverableModifierError, ValidationResult}

import scala.util.{Failure, Try}

/**
  * Trait that implements BlockSectionProcessor interfaces for regimes where the node
  * downloads and process full blocks.
  */
trait FullBlockSectionProcessor extends BlockSectionProcessor with FullBlockProcessor {

  override protected def process(m: BlockSection): ProgressInfo[ErgoPersistentModifier] = {
    getFullBlockByBlockSection(m) match {
      case Some(fb: ErgoFullBlock) =>
        processFullBlock(fb, m)
      case _ =>
        justPutToHistory(m)
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
    }.getOrElse(Failure(RecoverableModifierError(s"Header for modifier $m is not defined")))
  }

  /**
    * Trying to construct full block with modifier `m` and data, kept in history
    *
    * @param m - new modifier
    * @return Some(ErgoFullBlock) if block construction is possible, None otherwise
    */
  private def getFullBlockByBlockSection(m: BlockSection): Option[ErgoFullBlock] = {
    typedModifierById[Header](m.headerId).flatMap { h =>
      m match {
        case txs: BlockTransactions if !requireProofs =>
          Some(ErgoFullBlock(h, txs, None))
        case txs: BlockTransactions =>
          typedModifierById[ADProofs](h.ADProofsId).map(proofs => ErgoFullBlock(h, txs, Some(proofs)))
        case proofs: ADProofs =>
          typedModifierById[BlockTransactions](h.transactionsId).map(txs => ErgoFullBlock(h, txs, Some(proofs)))
      }
    }
  }

  private def justPutToHistory(m: BlockSection): ProgressInfo[ErgoPersistentModifier] = {
    historyStorage.insert(ByteArrayWrapper(m.id), Seq.empty, Seq(m))
    ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
  }

  /**
    * Validator for BlockTransactions and ADProofs
    */
  object PayloadValidator extends ModifierValidator with ScorexEncoding {

    def validate(m: ErgoPersistentModifier, header: Header, minimalHeight: Int): ValidationResult = {
      failFast
        .validate(!historyStorage.contains(m.id)) {
          fatal(s"Modifier ${m.encodedId} is already in history")
        }
        .validate(header.isCorrespondingModifier(m)) {
          fatal(s"Modifier ${m.encodedId} does not corresponds to header ${header.encodedId}")
        }
        .validateSemantics(isSemanticallyValid(header.id)) {
          fatal(s"Header ${header.encodedId} for modifier ${m.encodedId} is semantically invalid")
        }
        .validate(header.height >= minimalHeight) {
          fatal(s"Too old modifier ${m.encodedId}: ${header.height} < $minimalHeight")
        }
        .result
    }
  }

}
