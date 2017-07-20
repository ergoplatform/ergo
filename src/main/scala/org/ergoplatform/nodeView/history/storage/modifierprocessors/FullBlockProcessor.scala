package org.ergoplatform.nodeView.history.storage.modifierprocessors

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header, HistoryModifierSerializer}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import scorex.core.consensus.History.ProgressInfo

trait FullBlockProcessor {

  protected val historyStorage: HistoryStorage

  protected def processFullBlock(header: Header,
                                 txs: BlockTransactions,
                                 adProofs: ADProofs,
                                 txsAreNew: Boolean): ProgressInfo[ErgoPersistentModifier] = {
    val newModRow = if (txsAreNew) {
      Seq((ByteArrayWrapper(txs.id), ByteArrayWrapper(HistoryModifierSerializer.toBytes(txs))))
    } else {
      Seq((ByteArrayWrapper(adProofs.id), ByteArrayWrapper(HistoryModifierSerializer.toBytes(adProofs))))
    }
    val storageVersion = if (txsAreNew) txs.id else adProofs.id
???

  }

}
