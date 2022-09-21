package org.ergoplatform.settings

import org.ergoplatform.network.ModeFeatureSerializer
import scorex.core.network.PeerFeature
import scorex.core.network.peer.{LocalAddressPeerFeatureSerializer, RestApiUrlPeerFeatureSerializer, SessionIdPeerFeatureSerializer}

/**
  * Repository of existing peer feature identifiers
  */
object PeerFeatureConstants {
  /**
    * See `LocalAddressPeerFeature`
    */
  val LocalAddressPeerFeatureId: Byte = 2: Byte

  /**
    * See `SessionIdPeerFeature`
    */
  val SessionIdPeerFeatureId: Byte = 3: Byte

  /**
    * See `RestApiUrlPeerFeature`
    */
  val RestApiUrlFeatureId: Byte = 4: Byte

  /**
    * See `ModePeerFeature`
    */
  val ModeFeatureId: Byte = 16: Byte

  val FeatureSerializers: PeerFeature.Serializers = Map(
    LocalAddressPeerFeatureId -> LocalAddressPeerFeatureSerializer,
    SessionIdPeerFeatureId -> SessionIdPeerFeatureSerializer,
    RestApiUrlFeatureId -> RestApiUrlPeerFeatureSerializer,
    ModeFeatureId -> ModeFeatureSerializer
  )

}
