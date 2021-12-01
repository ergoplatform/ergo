package scorex.core.network.peer

import java.net.InetSocketAddress

import org.ergoplatform.network.ModeFeature
import org.ergoplatform.settings.ErgoSettings
import scorex.core.app.Version
import scorex.core.network.{ConnectionDirection, Incoming, Outgoing, PeerFeature, PeerSpec, PeerSpecSerializer}
import scorex.core.serialization.ScorexSerializer
import scorex.util.serialization.{Reader, Writer}

/**
  * Information about peer to be stored in PeerDatabase
  *
  * In case of updating this class check database backwards compatibility also!
  *
  * @param peerSpec       - general information about the peer
  * @param lastHandshake  - timestamp when last handshake was done
  * @param connectionType - type of connection (Incoming/Outgoing) established to this peer if any
  */
case class PeerInfo(peerSpec: PeerSpec,
                    lastHandshake: Long,
                    connectionType: Option[ConnectionDirection] = None)

/**
  * Information about P2P layer status
  *
  * @param lastIncomingMessage - timestamp of last received message from any peer
  * @param currentNetworkTime  - current network time
  */
case class PeersStatus(lastIncomingMessage: Long, currentNetworkTime: Long)

object PeerInfo {

  /**
    * Create peer info from address only, when we don't know other fields
    * (e.g. we got this information from config or from API)
    */
  def fromAddress(address: InetSocketAddress): PeerInfo = {
    val peerSpec = PeerSpec("unknown", Version.initial, s"unknown-$address", Some(address), Seq())
    PeerInfo(peerSpec, 0L, None)
  }

}

object PeerInfoSerializer extends ScorexSerializer[PeerInfo] {
  // TODO: This code is cut&pasted from ErgoApp.
  // It should be better extracted to some other place.
  private val ergoSettings = ErgoSettings.read()
  private val features: Seq[PeerFeature] = Seq(ModeFeature(ergoSettings.nodeSettings))
  private val featureSerializers: PeerFeature.Serializers = features.map(f => f.featureId -> f.serializer).toMap
  private val peerSpecSerializer =  new PeerSpecSerializer(featureSerializers)

  override def serialize(obj: PeerInfo, w: Writer): Unit = {
    w.putLong(obj.lastHandshake)
    w.putOption(obj.connectionType)((w,d) => w.putBoolean(d.isIncoming))
    peerSpecSerializer.serialize(obj.peerSpec, w)
  }

   override def parse(r: Reader): PeerInfo = {
     val lastHandshake = r.getLong()
     val connectionType = r.getOption(if (r.getUByte() != 0) Incoming else Outgoing)
     val peerSpec = peerSpecSerializer.parse(r)
     PeerInfo(peerSpec, lastHandshake, connectionType)
   }
}
