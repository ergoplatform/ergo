package org.ergoplatform.network.message

import org.ergoplatform.network.{PeerSpec, PeerSpecSerializer}
import org.ergoplatform.nodeView.state.SnapshotsInfo
import org.ergoplatform.nodeView.state.UtxoState.{ManifestId, SubtreeId}
import org.ergoplatform.network.message.MessageConstants.MessageCode
import scorex.crypto.hash.Digest32
import scorex.util.Extensions._
import scorex.util.serialization.{Reader, Writer}
import org.ergoplatform.sdk.wallet.Constants.ModifierIdLength

/**
  * The `GetPeer` message requests an `Peers` message from the receiving node,
  * preferably one with lots of `PeerSpec` of other receiving nodes.
  * The transmitting node can use those `PeerSpec` addresses to quickly update
  * its database of available nodes rather than waiting for unsolicited `Peers`
  * messages to arrive over time.
  */
object GetPeersSpec extends MessageSpecV1[Unit] {
  override val messageCode: MessageCode = 1: Byte

  override val messageName: String = "GetPeers message"

  override def serialize(obj: Unit, w: Writer): Unit = {
  }

  override def parse(r: Reader): Unit = {
    require(r.remaining == 0, "Non-empty data for GetPeers")
  }
}

object PeersSpec {

  val messageCode: MessageCode = 2: Byte

  val messageName: String = "Peers message"

}

/**
  * The `Peers` message is a reply to a `GetPeer` message and relays connection information about peers
  * on the network.
  */
class PeersSpec(peersLimit: Int) extends MessageSpecV1[Seq[PeerSpec]] {

  override val messageCode: MessageCode = PeersSpec.messageCode

  override val messageName: String = PeersSpec.messageName

  override def serialize(peers: Seq[PeerSpec], w: Writer): Unit = {
    w.putUInt(peers.size)
    peers.foreach(p => PeerSpecSerializer.serialize(p, w))
  }

  override def parse(r: Reader): Seq[PeerSpec] = {
    val length = r.getUInt().toIntExact
    require(length <= peersLimit, s"Too many peers. $length exceeds limit $peersLimit")
    (0 until length).map { _ =>
      PeerSpecSerializer.parse(r)
    }
  }
}

/**
  * The `GetSnapshotsInfo` message requests an `SnapshotsInfo` message from the receiving node
  */
object GetSnapshotsInfoSpec extends MessageSpecV1[Unit] {
  private val SizeLimit = 100

  override val messageCode: MessageCode = 76: Byte

  override val messageName: String = "GetSnapshotsInfo"

  override def serialize(obj: Unit, w: Writer): Unit = {
  }

  override def parse(r: Reader): Unit = {
    require(r.remaining < SizeLimit, "Too big GetSnapshotsInfo message")
  }
}

/**
  * The `SnapshotsInfo` message is a reply to a `GetSnapshotsInfo` message.
  * It contains information about UTXO set snapshots stored locally.
  */
object SnapshotsInfoSpec extends MessageSpecV1[SnapshotsInfo] {
  private val SizeLimit = 20000

  override val messageCode: MessageCode = 77: Byte

  override val messageName: String = "SnapshotsInfo"

  override def serialize(si: SnapshotsInfo, w: Writer): Unit = {
    w.putUInt(si.availableManifests.size)
    for ((height, manifest) <- si.availableManifests) {
      w.putInt(height)
      w.putBytes(manifest)
    }
  }

  override def parse(r: Reader): SnapshotsInfo = {
    require(r.remaining <= SizeLimit, s"Too big SnapshotsInfo message: ${r.remaining} bytes found, $SizeLimit max expected.")

    val length = r.getUInt().toIntExact
    val manifests = (0 until length).map { _ =>
      val height = r.getInt()
      val manifest = Digest32 @@ r.getBytes(ModifierIdLength)
      height -> manifest
    }.toMap
    new SnapshotsInfo(manifests)
  }

}

/**
  * The `GetManifest` sends manifest (BatchAVLProverManifest) identifier
  */
object GetManifestSpec extends MessageSpecV1[ManifestId] {
  private val SizeLimit = 100

  override val messageCode: MessageCode = 78: Byte
  override val messageName: String = "GetManifest"

  override def serialize(id: ManifestId, w: Writer): Unit = {
    w.putBytes(id)
  }

  override def parse(r: Reader): ManifestId = {
    require(r.remaining < SizeLimit, "Too big GetManifest message")
    Digest32 @@ r.getBytes(ModifierIdLength)
  }

}

/**
  * The `Manifest` message is a reply to a `GetManifest` message.
  * It contains serialized manifest, top subtree of a tree authenticating UTXO set snapshot
  */
object ManifestSpec extends MessageSpecV1[Array[Byte]] {
  private val SizeLimit = 4000000

  override val messageCode: MessageCode = 79: Byte

  override val messageName: String = "Manifest"

  override def serialize(manifestBytes: Array[Byte], w: Writer): Unit = {
    w.putUInt(manifestBytes.length)
    w.putBytes(manifestBytes)
  }

  override def parse(r: Reader): Array[Byte] = {
    require(r.remaining <= SizeLimit, s"Too big Manifest message.")

    val length = r.getUInt().toIntExact
    r.getBytes(length)
  }

}

/**
  * The `GetUtxoSnapshotChunk` sends send utxo subtree (BatchAVLProverSubtree) identifier
  */
object GetUtxoSnapshotChunkSpec extends MessageSpecV1[SubtreeId] {
  private val SizeLimit = 100

  override val messageCode: MessageCode = 80: Byte

  override val messageName: String = "GetUtxoSnapshotChunk"

  override def serialize(id: SubtreeId, w: Writer): Unit = {
    w.putBytes(id)
  }

  override def parse(r: Reader): SubtreeId = {
    require(r.remaining < SizeLimit, "Too big GetUtxoSnapshotChunk message")
    Digest32 @@ r.getBytes(ModifierIdLength)
  }

}

/**
  * The `UtxoSnapshotChunk` message is a reply to a `GetUtxoSnapshotChunk` message.
  */
object UtxoSnapshotChunkSpec extends MessageSpecV1[Array[Byte]] {
  private val SizeLimit = 4000000

  override val messageCode: MessageCode = 81: Byte

  override val messageName: String = "UtxoSnapshotChunk"

  override def serialize(subtree: Array[Byte], w: Writer): Unit = {
    w.putUInt(subtree.length)
    w.putBytes(subtree)
  }

  override def parse(r: Reader): Array[Byte] = {
    require(r.remaining <= SizeLimit, s"Too big UtxoSnapshotChunk message.")

    val length = r.getUInt().toIntExact
    r.getBytes(length)
  }

}
