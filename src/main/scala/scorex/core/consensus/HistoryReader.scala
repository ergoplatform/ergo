package scorex.core.consensus

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.nodeView.history.ErgoSyncInfo
import scorex.core.NodeViewComponent
import scorex.util.ModifierId

import scala.util.Try


trait HistoryReader extends NodeViewComponent with ContainsModifiers[ErgoPersistentModifier] {

  import History._

  /**
    * Is there's no history, even genesis block
    */
  def isEmpty: Boolean

   /**
    * Whether a modifier could be applied to the history
    *
    * @param modifier  - modifier to apply
    * @return `Success` if modifier can be applied, `Failure(ModifierError)` if can not
    */
  def applicableTry(modifier: ErgoPersistentModifier): Try[Unit]

  /**
    * Return semantic validity status of modifier with id == modifierId
    *
    * @param modifierId - modifier id to check
    * @return
    */
  def isSemanticallyValid(modifierId: ModifierId): ModifierSemanticValidity

  /**
    * Ids of modifiers, that node with info should download and apply to synchronize
    */
  def continuationIds(info: ErgoSyncInfo, size: Int): ModifierIds

  /**
    * Whether another's node syncinfo shows that another node is ahead or behind ours
    *
    * @param other other's node sync info
    * @return Equal if nodes have the same history, Younger if another node is behind, Older if a new node is ahead
    */
  def compare(other: ErgoSyncInfo): HistoryComparisonResult
}
