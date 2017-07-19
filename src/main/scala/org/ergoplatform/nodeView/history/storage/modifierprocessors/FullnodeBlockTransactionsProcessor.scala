package org.ergoplatform.nodeView.history.storage.modifierprocessors

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.{BlockTransactions, Header}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import scorex.crypto.encode.Base58

import scala.util.Try

/**
  * BlockTransactions processor for fullnode regime
  */
trait FullnodeBlockTransactionsProcessor extends BlockTransactionsProcessor {
  protected val historyStorage: HistoryStorage

  override def toInsert(m: BlockTransactions, env: ModifierProcessorEnvironment): Seq[(ByteArrayWrapper, ByteArrayWrapper)] = ???

  override def toDrop(modifier: BlockTransactions): Seq[ByteArrayWrapper] = ???

  override def validate(m: BlockTransactions): Try[Unit] = Try {
    require(!historyStorage.contains(m.id), s"Modifier $m is already in history")
    historyStorage.modifierById(m.headerId) match {
      case Some(h: Header) =>
        require(h.transactionsRoot sameElements m.id,
          s"Header transactions root ${Base58.encode(h.transactionsRoot)} differs from block transactions $m id")
      case None =>
        throw new Error(s"Header for modifier $m is no defined")
    }
  }
}
