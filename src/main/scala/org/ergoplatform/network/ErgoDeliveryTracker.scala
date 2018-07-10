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

  def isExpecting: Boolean = expecting.nonEmpty

  def expectingSize: Int = expecting.size

  override def isExpecting(mid: ModifierId): Boolean = expecting.contains(key(mid))

}