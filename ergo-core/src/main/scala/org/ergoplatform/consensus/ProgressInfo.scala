package org.ergoplatform.consensus

import org.ergoplatform.modifiers.{BlockSection, NetworkObjectTypeId}
import org.ergoplatform.utils.ScorexEncoder
import scorex.util.ModifierId

/**
  * Info returned by history to nodeViewHolder after modifier application
  *
  * @param branchPoint - branch point in case of rollback
  * @param toRemove    - modifiers to remove from current node view
  * @param toApply     - modifiers to apply to current node view
  * @param toDownload  - modifiers to download from other nodes
  * @tparam PM - type of used modifier
  */
case class ProgressInfo[PM <: BlockSection](branchPoint: Option[ModifierId],
                                            toRemove: Seq[PM],
                                            toApply: Seq[PM],
                                            toDownload: Seq[(NetworkObjectTypeId.Value, ModifierId)]) {

  if (toRemove.nonEmpty)
    require(branchPoint.isDefined, s"Branch point should be defined for non-empty `toRemove`")

  lazy val chainSwitchingNeeded: Boolean = toRemove.nonEmpty

  override def toString: String = {
    s"ProgressInfo(BranchPoint: ${branchPoint.map(ScorexEncoder.encodeId)}, " +
      s" to remove: ${toRemove.map(_.encodedId)}, to apply: ${toApply.map(_.encodedId)})"
  }
}

object ProgressInfo {
  val empty = ProgressInfo[BlockSection](None, Seq.empty, Seq.empty, Seq.empty)
}
