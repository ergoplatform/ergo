package scorex.core.transaction

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.core.consensus.ContainsModifiers
import scorex.core.NodeViewComponent
import scorex.util.ModifierId

/**
  * Unconfirmed transactions pool
  *
  */
trait MempoolReader extends NodeViewComponent with ContainsModifiers[ErgoTransaction] {

  //getters
  override def modifierById(modifierId: ModifierId): Option[ErgoTransaction]

  def contains(id: ModifierId): Boolean

  def getAll(ids: Seq[ModifierId]): Seq[ErgoTransaction]

  def size: Int

  def take(limit: Int): Iterable[ErgoTransaction]

}
