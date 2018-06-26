package org.ergoplatform.nodeView.history.storage.modifierprocessors

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header}
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock, ErgoPersistentModifier}
import scorex.core.consensus.History.ProgressInfo
import scorex.core.consensus.ModifierSemanticValidity.Invalid
import scorex.core.utils.ScorexEncoding
import scorex.core.validation.{ModifierValidator, RecoverableModifierError, ValidationResult}

import scala.util.{Failure, Try}

trait FullBlockSectionProcessor extends BlockSectionProcessor with FullBlockProcessor {

  protected def adState: Boolean

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
      val minimalHeight = if (contains(header.transactionsId) && !(header.transactionsId sameElements m.id)) {
        // ADProofs for already block transactions that are already in history. Do not validate whether it is too old
        -1
      } else {
        pruningProcessor.minimalFullBlockHeight
      }
      PayloadValidator.validate(m, header, minimalHeight).toTry
    }.getOrElse(Failure(RecoverableModifierError(s"Header for modifier $m is not defined")))
  }

  /**
    * Trying to construct full block with modifier `m` and data, kept in history
    * @param m - new modifier
    * @return Some(ErgoFullBlock) if block construction is possible, None otherwise
    */
  private def getFullBlockByBlockSection(m: BlockSection): Option[ErgoFullBlock] = {
    typedModifierById[Header](m.headerId).flatMap { h =>
      m match {
        case txs: BlockTransactions if !adState => Some(ErgoFullBlock(h, txs, None))
        case txs: BlockTransactions =>
          typedModifierById[ADProofs](h.ADProofsId) match {
            case Some(proofs) => Some(ErgoFullBlock(h, txs, Some(proofs)))
            case _ => None
          }
        case proofs: ADProofs =>
          typedModifierById[BlockTransactions](h.transactionsId) match {
            case Some(txs) => Some(ErgoFullBlock(h, txs, Some(proofs)))
            case _ => None
          }
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
        .validate(isSemanticallyValid(header.id) != Invalid) {
          fatal(s"Header ${header.encodedId} for modifier ${m.encodedId} is semantically invalid")
        }
        .validate(header.height >= minimalHeight) {
          fatal(s"Too old modifier ${m.encodedId}: ${header.height} < $minimalHeight")
        }
        .result
    }
  }

}
