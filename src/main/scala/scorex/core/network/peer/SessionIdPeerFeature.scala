package scorex.core.network.peer

import org.ergoplatform.settings.PeerFeatureDescriptors
import scorex.core.network.PeerFeature
import scorex.core.network.PeerFeature.Id
import scorex.core.network.message.Message
import scorex.util.serialization._
import scorex.core.serialization.ErgoSerializer

/**
  * This peer feature allows to more reliably detect connections to self node and connections from other networks
  *
  * @param networkMagic network magic bytes (taken from settings)
  * @param sessionId    randomly generated 64-bit session identifier
  */
case class SessionIdPeerFeature(networkMagic: Array[Byte],
                                sessionId: Long = scala.util.Random.nextLong()) extends PeerFeature {

  override type M = SessionIdPeerFeature
  override val featureId: Id = PeerFeatureDescriptors.SessionIdPeerFeatureId

  override def serializer: SessionIdPeerFeatureSerializer.type = SessionIdPeerFeatureSerializer

}


object SessionIdPeerFeatureSerializer extends ErgoSerializer[SessionIdPeerFeature] {

  override def serialize(obj: SessionIdPeerFeature, w: Writer): Unit = {
    w.putBytes(obj.networkMagic)
    w.putLong(obj.sessionId)
  }

  override def parse(r: Reader): SessionIdPeerFeature = {
    val networkMagic = r.getBytes(Message.MagicLength)
    val sessionId = r.getLong()
    SessionIdPeerFeature(networkMagic, sessionId)
  }

}
