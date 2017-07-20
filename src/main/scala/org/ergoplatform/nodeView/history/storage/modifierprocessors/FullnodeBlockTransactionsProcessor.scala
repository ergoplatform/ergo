package org.ergoplatform.nodeView.history.storage.modifierprocessors

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header, HistoryModifierSerializer}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import scorex.core.consensus.History.ProgressInfo
import scorex.crypto.encode.Base58

import scala.util.Try

/**
  * BlockTransactions processor for fullnode regime
  */
trait FullnodeBlockTransactionsProcessor extends BlockTransactionsProcessor with FullBlockProcessor {
  protected val historyStorage: HistoryStorage

  override def process(txs: BlockTransactions): ProgressInfo[ErgoPersistentModifier] = {
    historyStorage.modifierById(txs.headerId) match {
      case Some(header: Header) =>
        historyStorage.modifierById(header.ADProofsRoot) match {
          case Some(adProof: ADProofs) =>
            processFullBlock(header, txs, adProof, Map.empty, txsAreNew = true)
          case _ =>
            //TODO what if we do not need ADProofs (e.g. we can generate them by ourselves)
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
        require(h.transactionsRoot sameElements m.id,
          s"Header transactions root ${Base58.encode(h.transactionsRoot)} differs from block transactions $m id")
      case _ =>
        throw new Error(s"Header for modifier $m is no defined")
    }
  }
}
