package org.ergoplatform.nodeView.history

import io.iohk.iodb.LSMStore
import org.ergoplatform.modifiers.block.ErgoBlock
import org.ergoplatform.settings.ErgoSettings
import scorex.core.NodeViewModifier._
import scorex.core.utils.ScorexLogging

class HistoryStorage(storage: LSMStore, settings: ErgoSettings) extends ScorexLogging {

  def update(b: ErgoBlock, isBest: Boolean): Unit = ???

  def height: Int = ???

  def modifierById(id: ModifierId): Option[ErgoBlock] = ???

  def bestBlock: ErgoBlock = ???

  def heightOf(blockId: ModifierId): Option[Int] = ???
}
