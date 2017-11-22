package org.ergoplatform.network

import scorex.core.network.{ConnectedPeer, DeliveryTracker}
import scorex.core.utils.NetworkTime
import scorex.core.{ModifierId, ModifierTypeId}

import scala.collection.mutable

class ErgoDeliveryTracker extends DeliveryTracker {

  val toDownload:mutable.Map[MapKey, (ModifierTypeId, Long)] = mutable.Map[MapKey, (ModifierTypeId, Long)]()

  def downloadRequested(modifierTypeId: ModifierTypeId, modifierId: ModifierId): Unit = {
    toDownload.put(key(modifierId), (modifierTypeId, NetworkTime.time()))
  }

  override def receive(mtid: ModifierTypeId, mid: ModifierId, cp: ConnectedPeer): Unit = {
    if (isExpecting(mtid, mid, cp) || toDownload.contains(key(mid))) {
      toDownload.remove(key(mid))
      val eo = expecting.find(e => (mtid == e._1) && (mid sameElements e._2) && cp == e._3)
      for (e <- eo) expecting -= e
      delivered(key(mid)) = cp
    } else {
      deliveredSpam(key(mid)) = cp
    }
  }


}
