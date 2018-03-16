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

  private val ToDownloadRetryInterval = 10.seconds
  private val ToDownloadLifetime = 1.hour
  private val toDownload: mutable.Map[ModifierIdAsKey, ToDownloadStatus] = mutable.Map[ModifierIdAsKey, ToDownloadStatus]()

  def toDownloadQueue: Iterable[ModifierId] = ModifierId @@ toDownload.keys.map(_.array)

  def downloadRequested(modifierTypeId: ModifierTypeId, modifierId: ModifierId): Unit = {
    val downloadRequestTime = timeProvider.time()
    val newValue = toDownload.get(key(modifierId))
      .map(_.copy(lastTry = downloadRequestTime))
      .getOrElse(ToDownloadStatus(modifierTypeId, downloadRequestTime, downloadRequestTime))
    toDownload.put(key(modifierId), newValue)
  }

  def removeOutdatedToDownload(historyReaderOpt: Option[ErgoHistoryReader]): Unit = {
    val currentTime = timeProvider.time()
    toDownload.filter(td => (td._2.firstViewed < currentTime - ToDownloadLifetime.toMillis)
      || historyReaderOpt.exists(hr => hr.contains(ModifierId @@ td._1.array)))
      .foreach(i => toDownload.remove(i._1))
  }

  def idsToRetry(): Seq[(ModifierTypeId, ModifierId)] = {
    val currentTime = timeProvider.time()
    toDownload.filter(_._2.lastTry < currentTime - ToDownloadRetryInterval.toMillis).toSeq
      .sortBy(_._2.lastTry)
      .map(i => (i._2.tp, ModifierId @@ i._1.array))
  }

  override def receive(mtid: ModifierTypeId, mid: ModifierId, cp: ConnectedPeer): Unit = {
    toDownload.remove(key(mid))
    super.receive(mtid, mid, cp)
  }

}