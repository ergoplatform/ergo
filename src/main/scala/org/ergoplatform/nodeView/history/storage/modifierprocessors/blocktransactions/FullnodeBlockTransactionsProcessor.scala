package org.ergoplatform.nodeView.history.storage.modifierprocessors.blocktransactions

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.{ADProof, BlockTransactions, Header, HistoryModifierSerializer}
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.history.storage.modifierprocessors.FullBlockProcessor
import scorex.core.consensus.History.ProgressInfo
import scorex.crypto.encode.Base58

import scala.util.Try

/**
  * BlockTransactions processor for fullnode regime
  */
trait FullnodeBlockTransactionsProcessor extends BlockTransactionsProcessor with FullBlockProcessor {
  protected val historyStorage: HistoryStorage

  protected val adState: Boolean

  override def process(txs: BlockTransactions): ProgressInfo[ErgoPersistentModifier] = {
    historyStorage.modifierById(txs.headerId) match {
      case Some(header: Header) =>
        historyStorage.modifierById(header.ADProofsId) match {
          case Some(adProof: ADProof) =>
            processFullBlock(ErgoFullBlock(header, txs, Some(adProof), None), txsAreNew = true)
          case None if !adState =>
            processFullBlock(ErgoFullBlock(header, txs, None, None), txsAreNew = true)
          case _ =>
            val modifierRow = Seq((ByteArrayWrapper(txs.id), ByteArrayWrapper(HistoryModifierSerializer.toBytes(txs))))
            historyStorage.insert(txs.id, modifierRow)
            ProgressInfo(None, Seq(), Seq())
        }
      case _ =>
        throw new Error(s"Header for modifier $txs is no defined")
    }
  }

  override def toDrop(m: BlockTransactions): Seq[ByteArrayWrapper] = Seq(ByteArrayWrapper(m.id))

  override def validate(m: BlockTransactions): Try[Unit] = Try {
    require(!historyStorage.contains(m.id), s"Modifier $m is already in history")
    historyStorage.modifierById(m.headerId) match {
      case Some(h: Header) =>
        require(h.transactionsRoot sameElements m.digest,
          s"Header transactions root ${Base58.encode(h.transactionsRoot)} differs from block transactions $m digest")
      case _ =>
        throw new Error(s"Header for modifier $m is no defined")
    }
  }
}
