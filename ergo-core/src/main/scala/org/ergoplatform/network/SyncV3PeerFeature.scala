package org.ergoplatform.network

import org.ergoplatform.serialization.ErgoSerializer
import org.ergoplatform.settings.PeerFeatureDescriptors
import scorex.util.serialization.{Reader, Writer}

/**
  * Handshake peer feature advertising support for SyncInfo V3 (`ErgoSyncInfoV3`),
  * which includes stored header and full-block height ranges.
  */
case class SyncV3PeerFeature() extends PeerFeature {
  override type M = SyncV3PeerFeature

  override val featureId: PeerFeature.Id = PeerFeatureDescriptors.SyncV3FeatureId

  override def serializer: ErgoSerializer[SyncV3PeerFeature] = SyncV3FeatureSerializer
}

object SyncV3FeatureSerializer extends ErgoSerializer[SyncV3PeerFeature] {

  val MaxSize = 64

  override def serialize(obj: SyncV3PeerFeature, w: Writer): Unit = {}

  override def parse(r: Reader): SyncV3PeerFeature = {
    require(r.remaining <= MaxSize, s"Too big SyncV3PeerFeature payload: ${r.remaining}")
    SyncV3PeerFeature()
  }

}
