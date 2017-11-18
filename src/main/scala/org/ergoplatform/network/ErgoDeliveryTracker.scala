package org.ergoplatform.network

import scorex.core.network.{ConnectedPeer, DeliveryTracker}
import scorex.core.utils.NetworkTime
import scorex.core.{ModifierId, ModifierTypeId}

import scala.collection.mutable

class ErgoDeliveryTracker extends DeliveryTracker {

  val toDownload = mutable.Map[MapKey, (ModifierTypeId, Long)]()

  def downloadRequested(modifierTypeId: ModifierTypeId, modifierId: ModifierId): Unit = {
    toDownload.put(key(modifierId), (modifierTypeId, NetworkTime.time()))
  }

  override def receive(mtid: ModifierTypeId, mid: ModifierId, cp: ConnectedPeer): Unit = {
    toDownload.remove(key(mid))
    super.receive(mtid, mid, cp)
  }

}
