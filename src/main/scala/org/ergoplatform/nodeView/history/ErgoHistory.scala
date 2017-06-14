package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.block.ErgoBlock
import org.ergoplatform.modifiers.transaction.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.transaction.proposition.AnyoneCanSpendProposition
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.consensus.History
import scorex.core.consensus.History.{ModifierIds, ProgressInfo}
import scorex.core.utils.ScorexLogging

import scala.util.Try

class ErgoHistory extends History[AnyoneCanSpendProposition,
  AnyoneCanSpendTransaction,
  ErgoBlock,
  ErgoSyncInfo,
  ErgoHistory] with ScorexLogging {

  override def isEmpty: Boolean = ???

  override def modifierById(modifierId: ModifierId): Option[ErgoBlock] = ???

  override def append(modifier: ErgoBlock): Try[(ErgoHistory, ProgressInfo[ErgoBlock])] = ???

  override def drop(modifierId: ModifierId): ErgoHistory = ???

  override def openSurfaceIds(): Seq[ModifierId] = ???

  override def continuationIds(from: ModifierIds, size: Int): Option[ModifierIds] = ???

  override def syncInfo(answer: Boolean): ErgoSyncInfo = ???

  override def compare(other: ErgoSyncInfo): _root_.scorex.core.consensus.History.HistoryComparisonResult.Value = ???

  override type NVCT = ErgoHistory
}