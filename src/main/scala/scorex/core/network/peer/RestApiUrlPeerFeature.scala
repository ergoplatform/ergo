package scorex.core.network.peer

import scorex.core.network.PeerFeature
import scorex.core.network.PeerFeature.Id
import scorex.core.serialization.ScorexSerializer
import scorex.util.Extensions._
import scorex.util.serialization._

import java.net.URL

case class RestApiUrlPeerFeature(restApiUrl: Option[URL]) extends PeerFeature {
  override type M = RestApiUrlPeerFeature
  override val featureId: Id = RestApiUrlPeerFeature.featureId

  override def serializer: RestApiUrlPeerFeatureSerializer.type = RestApiUrlPeerFeatureSerializer
}

object RestApiUrlPeerFeature {
  val featureId: Id = 4: Byte // ???
}

object RestApiUrlPeerFeatureSerializer extends ScorexSerializer[RestApiUrlPeerFeature] {

  override def serialize(obj: RestApiUrlPeerFeature, w: Writer): Unit = {
    w.putOption(obj.restApiUrl: Option[URL]) { (writer, url) =>
      val addr = url.toString.getBytes("UTF-8")
      writer.put(addr.size.toByteExact)
      writer.putBytes(addr)
    }
  }

  override def parse(r: Reader): RestApiUrlPeerFeature =
    RestApiUrlPeerFeature (
      r.getOption {
        val fas = r.getUByte()
        val fa = r.getBytes(fas)
        new URL(new String(fa))
      }
    )
}
