package org.ergoplatform.settings

import org.ergoplatform.network.{ModeFeatureSerializer, PeerFeature}
import org.ergoplatform.network.peer.{LocalAddressPeerFeatureSerializer, RestApiUrlPeerFeatureSerializer, SessionIdPeerFeatureSerializer}

/**
  * Repository of existing peer feature identifiers, stores their ids along with serializers
  */
object PeerFeatureDescriptors {
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

  /**
    * All the peer feature serializers should be here
    */
  val FeatureSerializers: PeerFeature.Serializers = Map(
    LocalAddressPeerFeatureId -> LocalAddressPeerFeatureSerializer,
    SessionIdPeerFeatureId -> SessionIdPeerFeatureSerializer,
    RestApiUrlFeatureId -> RestApiUrlPeerFeatureSerializer,
    ModeFeatureId -> ModeFeatureSerializer
  )

}
