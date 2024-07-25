package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.extra.ExtraIndexer._

/**
 * An immutable state for extra indexer
 * @param indexedHeight - Indexed block height
 * @param globalTxIndex - Indexed transaction count
 * @param globalBoxIndex - Indexed box count
 * @param rollbackTo - blockheight to rollback to, 0 if no rollback is in progress
 * @param caughtUp - flag to indicate if the indexer is caught up with the chain and is listening for updates
 */
case class IndexerState(indexedHeight: Int,
                        globalTxIndex: Long,
                        globalBoxIndex: Long,
                        rollbackTo: Int,
                        caughtUp: Boolean) {

  def rollbackInProgress: Boolean = rollbackTo > 0

  def incrementIndexedHeight: IndexerState = copy(indexedHeight = indexedHeight + 1)

  def incrementTxIndex: IndexerState = copy(globalTxIndex = globalTxIndex + 1)
  def incrementBoxIndex: IndexerState = copy(globalBoxIndex = globalBoxIndex + 1)

  def decrementTxIndex: IndexerState = copy(globalTxIndex = globalTxIndex - 1)
  def decrementBoxIndex: IndexerState = copy(globalBoxIndex = globalBoxIndex - 1)

}

object IndexerState {

  def fromHistory(history: ErgoHistory): IndexerState = {
    val indexedHeight = getIndex(IndexedHeightKey, history).getInt
    val globalTxIndex = getIndex(GlobalTxIndexKey, history).getLong
    val globalBoxIndex = getIndex(GlobalBoxIndexKey, history).getLong
    val rollbackTo = getIndex(RollbackToKey, history).getInt
    IndexerState(
      indexedHeight,
      globalTxIndex,
      globalBoxIndex,
      rollbackTo,
      caughtUp = indexedHeight == history.fullBlockHeight
    )
  }

}
