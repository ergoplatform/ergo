package org.ergoplatform.network


import akka.actor.{ActorRef, ActorSystem}
import scorex.core.network.{ConnectedPeer, DeliveryTracker}
import scorex.core.utils.NetworkTimeProvider
import scorex.core.{ModifierId, ModifierTypeId}

import scala.collection.mutable
import scala.concurrent.duration._

class ErgoDeliveryTracker(system: ActorSystem,
                          deliveryTimeout: FiniteDuration,
                          maxDeliveryChecks: Int,
                          nvsRef: ActorRef,
                          timeProvider: NetworkTimeProvider)
  extends DeliveryTracker(system, deliveryTimeout, maxDeliveryChecks, nvsRef) {

  private val ToDownloadRetryInterval = 10.seconds
  private val ToDownloadLifetime = 1.hour
  // Modifiers we need to download, but do not know peer that have this modifier
  // TODO we may try to guess this peers using delivered map
  private val expectingFromRandom: mutable.Map[ModifierIdAsKey, ToDownloadStatus] = mutable.Map[ModifierIdAsKey, ToDownloadStatus]()

  def isExpectingFromRandom: Boolean = expectingFromRandom.nonEmpty

  def isExpecting: Boolean = expecting.nonEmpty

  /**
    * TODO temporary fix until https://github.com/ergoplatform/ergo/issues/342 will be implemented
    * @return ids we're going to download or already have downloaded
    */
  def expectingAndDelivered: (Iterable[ModifierId], Iterable[ModifierId]) =
    (ModifierId @@ delivered.keys.map(_.array), ModifierId @@ expectingFromRandom.keys.map(_.array))

  /**
    * Process download request of modifier of type modifierTypeId with id modifierId
    */
  def expectFromRandom(modifierTypeId: ModifierTypeId, modifierId: ModifierId): Unit = {
    val downloadRequestTime = timeProvider.time()
    val newValue = expectingFromRandom.get(key(modifierId))
      .map(_.copy(lastTry = downloadRequestTime))
      .getOrElse(ToDownloadStatus(modifierTypeId, downloadRequestTime, downloadRequestTime))
    expectingFromRandom.put(key(modifierId), newValue)
  }

  /**
    * Remove old modifiers from download queue
    */
  def removeOutdatedExpectingFromRandom(): Unit = {
    val currentTime = timeProvider.time()
    expectingFromRandom
      .filter { case (_, status) => status.firstViewed < currentTime - ToDownloadLifetime.toMillis }
      .foreach { case (key, _) => expectingFromRandom.remove(key) }
  }

  /**
    * Id's that are already in queue to download but are not downloaded yet and were not requested recently
    */
  def idsExpectingFromRandomToRetry(): Seq[(ModifierTypeId, ModifierId)] = {
    val currentTime = timeProvider.time()
    expectingFromRandom.filter(_._2.lastTry < currentTime - ToDownloadRetryInterval.toMillis).toSeq
      .sortBy(_._2.lastTry)
      .map(i => (i._2.tp, ModifierId @@ i._1.array))
  }

  /**
    * Modifier downloaded
    */
  override def onReceive(mtid: ModifierTypeId, mid: ModifierId, cp: ConnectedPeer): Unit = {
    if (expectingFromRandom.contains(key(mid))) {
      expectingFromRandom.remove(key(mid))
      delivered(key(mid)) = cp
    } else {
      super.onReceive(mtid, mid, cp)
    }
  }

}