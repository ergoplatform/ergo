package org.ergoplatform.network

import akka.actor.{ActorContext, ActorRef}
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import scorex.core.network.{ConnectedPeer, DeliveryTracker}
import scorex.core.utils.NetworkTimeProvider
import scorex.core.{ModifierId, ModifierTypeId}

import scala.collection.mutable
import scala.concurrent.duration._

class ErgoDeliveryTracker(context: ActorContext,
                          deliveryTimeout: FiniteDuration,
                          maxDeliveryChecks: Int,
                          nvsRef: ActorRef,
                          timeProvider: NetworkTimeProvider)
  extends DeliveryTracker(context, deliveryTimeout, maxDeliveryChecks, nvsRef) {

  val toDownload: mutable.Map[ModifierIdAsKey, ToDownloadStatus] = mutable.Map[ModifierIdAsKey, ToDownloadStatus]()

  //TODO move to config
  private val ToDownloadRetryInterval = 30.seconds
  private val ToDownloadLifetime = 24.hours
  private val MaxModifiersToDownload = 100
  private var lastTime = timeProvider.time()

  def downloadRequested(modifierTypeId: ModifierTypeId, modifierId: ModifierId): Unit = {
    lastTime = Math.max(timeProvider.time(), lastTime + 1)
    val newValue = toDownload.get(key(modifierId))
      .map(_.copy(lastTry = lastTime))
      .getOrElse(ToDownloadStatus(modifierTypeId, lastTime, lastTime))
    toDownload.put(key(modifierId), newValue)
  }

  def removeOutdatedToDownload(historyReaderOpt: Option[ErgoHistoryReader]): Unit = {
    val currentTime = timeProvider.time()
    toDownload.filter(td => (td._2.firstViewed < currentTime - ToDownloadLifetime.toMillis)
      || historyReaderOpt.exists(hr => hr.contains(ModifierId @@ td._1.array)))
      .foreach(i => toDownload.remove(i._1))
  }

  def downloadRetry(historyReaderOpt: Option[ErgoHistoryReader]): Seq[(ModifierId, ToDownloadStatus)] = {
    removeOutdatedToDownload(historyReaderOpt)
    val currentTime = timeProvider.time()
    toDownload.filter(_._2.lastTry < currentTime - ToDownloadRetryInterval.toMillis).toSeq
      .sortBy(_._2.lastTry)
      .take(MaxModifiersToDownload)
      .map(i => (ModifierId @@ i._1.array, i._2))
  }

  @SuppressWarnings(Array("ComparingUnrelatedTypes"))
  override def receive(mtid: ModifierTypeId, mid: ModifierId, cp: ConnectedPeer): Unit = {
    if (isExpecting(mtid, mid, cp) || toDownload.contains(key(mid))) {
      toDownload.remove(key(mid))
      expecting.find(e => (mtid == e._1) && (mid sameElements e._2) && cp == e._3).foreach(e => expecting-= e)
      delivered(key(mid)) = cp
    } else {
      deliveredSpam(key(mid)) = cp
    }
  }
}
