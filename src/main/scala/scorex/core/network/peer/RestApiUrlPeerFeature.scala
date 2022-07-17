package scorex.core.network.peer

import scorex.core.network.PeerFeature
import scorex.core.network.PeerFeature.Id
import scorex.core.serialization.ScorexSerializer
import scorex.util.Extensions._
import scorex.util.serialization._
import java.net.URL

import org.ergoplatform.settings.PeerFeatureIds

/**
  * Peer may have rest-api URL enabled in which case it needs to be passed to/from other peers
  * @param restApiUrl publicly accessible url of node which exposes restApi in firewall
  */
case class RestApiUrlPeerFeature(restApiUrl: URL) extends PeerFeature {
  override type M = RestApiUrlPeerFeature
  override val featureId: Id = PeerFeatureIds.RestApiUrlFeatureId

  override def serializer: RestApiUrlPeerFeatureSerializer.type = RestApiUrlPeerFeatureSerializer
}

object RestApiUrlPeerFeatureSerializer extends ScorexSerializer[RestApiUrlPeerFeature] {

  override def serialize(obj: RestApiUrlPeerFeature, w: Writer): Unit = {
    val addr = obj.restApiUrl.toString.getBytes("UTF-8")
    w.put(addr.size.toByteExact)
    w.putBytes(addr)
  }

  override def parse(r: Reader): RestApiUrlPeerFeature = {
    val fas = r.getUByte()
    val fa = r.getBytes(fas)
    val url = new URL(new String(fa, "UTF-8"))
    RestApiUrlPeerFeature(url)
  }

}
