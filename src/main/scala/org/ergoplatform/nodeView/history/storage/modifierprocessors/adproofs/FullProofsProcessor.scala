package org.ergoplatform.nodeView.history.storage.modifierprocessors.adproofs

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header}
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.storage.modifierprocessors.FullBlockProcessor
import scorex.core.consensus.History.ProgressInfo

import scala.util.Try

/**
  * ADProof processor for node regime with DigestState
  */
trait FullProofsProcessor extends ADProofsProcessor with FullBlockProcessor {

  protected val adState: Boolean

  override protected def process(m: ADProofs): ProgressInfo[ErgoPersistentModifier] = {
    historyStorage.modifierById(m.headerId) match {
      case Some(header: Header) =>
        historyStorage.modifierById(header.transactionsId) match {
          case Some(txs: BlockTransactions) if adState =>
            processFullBlock(ErgoFullBlock(header, txs, Some(m)), txsAreNew = false)
          case _ =>
            historyStorage.insert(ByteArrayWrapper(m.id), Seq.empty, Seq(m))
            ProgressInfo(None, Seq.empty, None, Seq.empty)
        }
      case _ =>
        throw new Error(s"Header for modifier $m is no defined")
    }
  }

  override protected def validate(m: ADProofs): Try[Unit] = modifierValidation(m, typedModifierById[Header](m.headerId))

}
