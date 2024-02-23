package org.ergoplatform.network.peer

import org.ergoplatform.network.PeerFeature
import org.ergoplatform.settings.PeerFeatureDescriptors
import org.ergoplatform.network.message.MessageConstants
import org.ergoplatform.serialization.ErgoSerializer
import scorex.util.serialization._

/**
  * This peer feature allows to more reliably detect connections to self node and connections from other networks
  *
  * @param networkMagic network magic bytes (taken from settings)
  * @param sessionId    randomly generated 64-bit session identifier
  */
case class SessionIdPeerFeature(networkMagic: Array[Byte],
                                sessionId: Long = scala.util.Random.nextLong()) extends PeerFeature {

  override type M = SessionIdPeerFeature
  override val featureId: PeerFeature.Id = PeerFeatureDescriptors.SessionIdPeerFeatureId

  override def serializer: SessionIdPeerFeatureSerializer.type = SessionIdPeerFeatureSerializer

}


object SessionIdPeerFeatureSerializer extends ErgoSerializer[SessionIdPeerFeature] {

  override def serialize(obj: SessionIdPeerFeature, w: Writer): Unit = {
    w.putBytes(obj.networkMagic)
    w.putLong(obj.sessionId)
  }

  override def parse(r: Reader): SessionIdPeerFeature = {
    val networkMagic = r.getBytes(MessageConstants.MagicLength)
    val sessionId = r.getLong()
    SessionIdPeerFeature(networkMagic, sessionId)
  }

}
