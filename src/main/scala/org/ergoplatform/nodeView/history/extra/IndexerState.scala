package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.extra.ExtraIndexer._
import org.ergoplatform.modifiers.ErgoNodeViewModifier
import org.ergoplatform.modifiers.history.header.Header
import scorex.util.{ModifierId, bytesToId}

/**
 * An immutable state for extra indexer
 * @param indexedHeight - Indexed block height
 * @param globalTxIndex - Indexed transaction count
 * @param globalBoxIndex - Indexed box count
 * @param rollbackTo - blockheight to rollback to, 0 if no rollback is in progress
 * @param caughtUp - flag to indicate if the indexer is caught up with the chain and is listening for updates
 * @param indexedHeaderId - id of the last block represented by the extra index
 */
case class IndexerState(indexedHeight: Int,
                        globalTxIndex: Long,
                        globalBoxIndex: Long,
                        rollbackTo: Int,
                        caughtUp: Boolean,
                        indexedHeaderId: Option[ModifierId] = None) {

  def rollbackInProgress: Boolean = rollbackTo > 0

  def incrementIndexedHeight: IndexerState = copy(indexedHeight = indexedHeight + 1)
  def decrementIndexedHeight: IndexerState = copy(indexedHeight = indexedHeight - 1)

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
    val indexedHeaderId = history.historyStorage
      .modifierBytesById(bytesToId(IndexedHeaderIdKey))
      .filter(_.length == ErgoNodeViewModifier.ModifierIdSize)
      .map(bytesToId)
      .filter(id => history.typedModifierById[Header](id).exists(_.height == indexedHeight))
    IndexerState(
      indexedHeight,
      globalTxIndex,
      globalBoxIndex,
      rollbackTo,
      caughtUp = indexedHeight == history.fullBlockHeight &&
        (indexedHeight == 0 || indexedHeaderId.isDefined),
      indexedHeaderId = indexedHeaderId
    )
  }

}
