package org.ergoplatform.nodeView.history.storage.modifierprocessors.blocktransactions

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header}
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.history.storage.modifierprocessors.FullBlockProcessor
import scorex.core.consensus.History.ProgressInfo

import scala.util.Try

/**
  * BlockTransactions processor for settings with verifyTransactions=true
  */
trait FullBlockTransactionsProcessor extends BlockTransactionsProcessor with FullBlockProcessor {
  protected val historyStorage: HistoryStorage

  protected val adState: Boolean

  override protected def process(txs: BlockTransactions): ProgressInfo[ErgoPersistentModifier] = {
    historyStorage.modifierById(txs.headerId) match {
      case Some(header: Header) =>
        historyStorage.modifierById(header.ADProofsId) match {
          case _ if bestFullBlockIdOpt.isEmpty && !isValidFirstFullBlock(header) =>
            justPutToHistory(txs)
          case Some(adProof: ADProofs) =>
            processFullBlock(ErgoFullBlock(header, txs, Some(adProof)), txsAreNew = true)
          case None if !adState =>
            processFullBlock(ErgoFullBlock(header, txs, None), txsAreNew = true)
          case _ =>
            justPutToHistory(txs)
        }
      case _ =>
        throw new Error(s"Header for modifier $txs is no defined")
    }
  }

  private def justPutToHistory(txs: BlockTransactions): ProgressInfo[ErgoPersistentModifier] = {
    historyStorage.insert(ByteArrayWrapper(txs.id), Seq.empty, Seq(txs))
    ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
  }

  override protected def validate(m: BlockTransactions): Try[Unit] =
    modifierValidation(m, typedModifierById[Header](m.headerId))

}
