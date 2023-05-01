package scorex.core.network.peer

import scorex.core.network.PeerFeature
import scorex.core.network.PeerFeature.Id
import scorex.core.serialization.ErgoSerializer
import scorex.util.serialization._

import java.net.URL
import org.ergoplatform.settings.PeerFeatureDescriptors
import scorex.core.serialization.ErgoSerializer

/**
  * Peer may have rest-api URL enabled in which case it needs to be passed to/from other peers
  * @param restApiUrl publicly accessible url of node which exposes restApi in firewall
  */
case class RestApiUrlPeerFeature(restApiUrl: URL) extends PeerFeature {
  override type M = RestApiUrlPeerFeature
  override val featureId: Id = PeerFeatureDescriptors.RestApiUrlFeatureId

  override def serializer: RestApiUrlPeerFeatureSerializer.type = RestApiUrlPeerFeatureSerializer
}

object RestApiUrlPeerFeatureSerializer extends ErgoSerializer[RestApiUrlPeerFeature] {

  override def serialize(obj: RestApiUrlPeerFeature, w: Writer): Unit = {
    val restApiUrl = obj.restApiUrl.toString
    val restApiUrlBytes = restApiUrl.getBytes("UTF-8")
    require(restApiUrlBytes.size <= 0xFF, s"$restApiUrl size is out of unsigned byte range")
    w.putUByte(restApiUrlBytes.size)
    w.putBytes(restApiUrlBytes)
  }

  override def parse(r: Reader): RestApiUrlPeerFeature = {
    val fas = r.getUByte()
    val fa = r.getBytes(fas)
    val url = new URL(new String(fa, "UTF-8"))
    RestApiUrlPeerFeature(url)
  }

}
