package org.ergoplatform.network

import akka.actor.{ActorContext, ActorRef}
import scorex.core.network.{ConnectedPeer, DeliveryTracker}
import scorex.core.utils.NetworkTime
import scorex.core.{ModifierId, ModifierTypeId}

import scala.collection.mutable
import scala.concurrent.duration._

class ErgoDeliveryTracker(context: ActorContext,
                          deliveryTimeout: FiniteDuration,
                          maxDeliveryChecks: Int,
                          nvsRef: ActorRef)
  extends DeliveryTracker(context, deliveryTimeout, maxDeliveryChecks, nvsRef) {

  val toDownload: mutable.Map[ModifierIdAsKey, ToDownloadStatus] = mutable.Map[ModifierIdAsKey, ToDownloadStatus]()
  private val toDownloadRetryInterval = 5.seconds
  private val toDownloadLifetime = 5.minutes

  def downloadRequested(modifierTypeId: ModifierTypeId, modifierId: ModifierId): Unit = {
    val time = NetworkTime.time()
    val prevValue = toDownload.get(key(modifierId))
    val newValue = prevValue.map(p => p.copy(lastTry = time)).getOrElse(ToDownloadStatus(modifierTypeId, time, time))
    toDownload.put(key(modifierId), newValue)
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

  def removeOutdatedToDownload(): Unit = {
    val currentTime = NetworkTime.time()
    toDownload.filter(_._2.firstViewed < currentTime - toDownloadLifetime.toMillis)
      .foreach(i => toDownload.remove(i._1))
  }

  def downloadRetry(): Seq[(ModifierId, ToDownloadStatus)] = {
    val currentTime = NetworkTime.time()
    toDownload.filter(_._2.lastTry < currentTime - toDownloadRetryInterval.toMillis)
      .map(i => (ModifierId @@ i._1.array, i._2)).toSeq
  }


}
