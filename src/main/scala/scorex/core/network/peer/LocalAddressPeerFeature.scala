package scorex.core.network.peer

import java.net.{InetAddress, InetSocketAddress}

import org.ergoplatform.settings.PeerFeatureIds
import scorex.core.network.PeerFeature
import scorex.core.network.PeerFeature.Id
import scorex.util.serialization._
import scorex.core.serialization.ScorexSerializer
import scorex.util.Extensions._

case class LocalAddressPeerFeature(address: InetSocketAddress) extends PeerFeature {
  override type M = LocalAddressPeerFeature
  override val featureId: Id = PeerFeatureIds.LocalAddressPeerFeatureId

  override def serializer: LocalAddressPeerFeatureSerializer.type = LocalAddressPeerFeatureSerializer
}

object LocalAddressPeerFeatureSerializer extends ScorexSerializer[LocalAddressPeerFeature] {

  private val AddressLength = 4

  override def serialize(obj: LocalAddressPeerFeature, w: Writer): Unit = {
    w.putBytes(obj.address.getAddress.getAddress)
    w.putUInt(obj.address.getPort)
  }

  override def parse(r: Reader): LocalAddressPeerFeature = {
    val fa = r.getBytes(AddressLength)
    val port = r.getUInt().toIntExact
    LocalAddressPeerFeature(new InetSocketAddress(InetAddress.getByAddress(fa), port))
  }
}
