package scorex.core.transaction

import scorex.core.consensus.ContainsModifiers
import scorex.core.NodeViewComponent
import scorex.util.ModifierId

/**
  * Unconfirmed transactions pool
  *
  * @tparam TX -type of transaction the pool contains
  */
trait MempoolReader[TX <: Transaction] extends NodeViewComponent with ContainsModifiers[TX] {

  //getters
  override def modifierById(modifierId: ModifierId): Option[TX]

  def contains(id: ModifierId): Boolean

  def getAll(ids: Seq[ModifierId]): Seq[TX]

  def size: Int

}
