package org.ergoplatform.network

import org.ergoplatform.core.BytesSerializable
import org.ergoplatform.serialization.ErgoSerializer

/**
  * An abstract trait to describe peer capabilities.
  * During a handshake peers are sending list of their "features" to each other.
  * It is assumed that features are not changing when the node runs.
  * Maximum theoretical size of a serialized feature is 32,767 bytes.
  * However, handshake size limit is also to be considered
  * (for all the features to be sent during the handshake).
  */
trait PeerFeature extends BytesSerializable {
  override type M >: this.type <: PeerFeature
  val featureId: PeerFeature.Id
}

object PeerFeature {
  type Id = Byte
  type Serializers = Map[Id, ErgoSerializer[_ <: PeerFeature]]
}
