package org.ergoplatform.network.peer

import org.ergoplatform.network.PeerFeature
import org.ergoplatform.serialization.ErgoSerializer
import org.ergoplatform.settings.PeerFeatureDescriptors
import scorex.util.serialization._

/**
  * Capability flag: the peer accepts a Modifiers message carrying a single AD proofs section bigger than
  * `ModifiersSpec.maxMsgSizeWithReserve` (up to `ModifiersSpec.maxSingleAdProofsMsgSize`).
  * Older nodes ignore unknown features in the handshake, and would reject such a message as malformed,
  * so a node sends a large AD proofs section only to peers announcing this feature.
  */
case class LargeAdProofsPeerFeature() extends PeerFeature {
  override type M = LargeAdProofsPeerFeature
  override val featureId: PeerFeature.Id = PeerFeatureDescriptors.LargeAdProofsFeatureId

  override def serializer: LargeAdProofsPeerFeatureSerializer.type = LargeAdProofsPeerFeatureSerializer
}

object LargeAdProofsPeerFeatureSerializer extends ErgoSerializer[LargeAdProofsPeerFeature] {

  override def serialize(obj: LargeAdProofsPeerFeature, w: Writer): Unit = {}

  override def parse(r: Reader): LargeAdProofsPeerFeature = {
    // the feature carries no data; a non-empty payload is not this feature (PeerSpec parsing then drops it)
    require(r.remaining == 0, s"LargeAdProofsPeerFeature must have an empty payload, got ${r.remaining} bytes")
    LargeAdProofsPeerFeature()
  }

}
