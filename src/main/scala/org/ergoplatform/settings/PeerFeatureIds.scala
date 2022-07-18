package org.ergoplatform.settings

/**
  * Repository of existing peer feature identifiers
  */
object PeerFeatureIds {
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
}
