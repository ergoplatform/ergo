package org.ergoplatform.nodeView

import org.ergoplatform.network.ErgoDeliveryTracker
import org.ergoplatform.nodeView.ModifiersStatusChecker._
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import scorex.core.ModifierId

import scala.collection.mutable

/**
  * Class that calculate modifiers status based on different components data
  */
class ModifiersStatusChecker(history: ErgoHistoryReader,
                             modifiersCache: ErgoModifiersCache,
                             deliveryTracker: ErgoDeliveryTracker) {

  type K = mutable.WrappedArray[Byte]

  def status(id: ModifierId): ModifiersStatus = {
    if (deliveryTracker.isKnown(key(id))) {
      Requested
    } else if (modifiersCache.contains(key(id))) {
      InCache
    } else if (history.contains(id)) {
      Applied
    } else {
      Unknown
    }
  }

  def isUnknown(id: ModifierId): Boolean = status(id) == Unknown

  protected def key(id: ModifierId) = new mutable.WrappedArray.ofByte(id)
}


object ModifiersStatusChecker {

  sealed trait ModifiersStatus

  /**
    * This modifier was never known to our peer
    */
  case object Unknown extends ModifiersStatus

  /**
    * Id of this modifier was declared somehow, e.g. we received a header that contains id of this modifier
    */
  case object IdKnown extends ModifiersStatus

  /**
    * Our node have requested this modifier from other peers
    */
  case object Requested extends ModifiersStatus

  /**
    * Our node have received this modifierm but did not applied yet
    */
  case object InCache extends ModifiersStatus

  /**
    * This modifier was already applied to history
    */
  case object Applied extends ModifiersStatus

}
