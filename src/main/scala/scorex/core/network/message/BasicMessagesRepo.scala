package scorex.core.network.message

import org.ergoplatform.modifiers.NetworkObjectTypeId
import org.ergoplatform.nodeView.state.SnapshotsInfo
import org.ergoplatform.nodeView.state.UtxoState.{ManifestId, SubtreeId}
import org.ergoplatform.modifiers.history.popow.{NipopowAlgos, NipopowProof, NipopowProofSerializer}
import org.ergoplatform.settings.{Algos, ErgoSettings}
import scorex.core.consensus.SyncInfo
import scorex.core.network._
import scorex.core.network.message.Message.MessageCode
import scorex.core.serialization.ErgoSerializer
import scorex.core.NodeViewModifier
import scorex.crypto.hash.Digest32
import scorex.util.Extensions._
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, ScorexLogging, bytesToId, idToBytes}
import org.ergoplatform.sdk.wallet.Constants.ModifierIdLength
import scala.collection.immutable

/**
  * Wrapper for block sections of the same type. Used to send multiple block sections at once ove the wire.
  */
case class ModifiersData(typeId: NetworkObjectTypeId.Value, modifiers: Map[ModifierId, Array[Byte]])

case class InvData(typeId: NetworkObjectTypeId.Value, ids: Seq[ModifierId])

case class NipopowProofData(m: Int = 6, k: Int = 6, headerId: Option[ModifierId]) {
  def headerIdBytesOpt: Option[Array[Byte]] = headerId.map(Algos.decode).flatMap(_.toOption)
}

/**
  * The `SyncInfo` message requests an `Inv` message that provides modifier ids
  * required be sender to synchronize his blockchain with the recipient.
  * It allows a peer which has been disconnected or started for the first
  * time to get the data it needs to request the blocks it hasn't seen.
  *
  * Payload of this message should be determined in underlying applications.
  */
class SyncInfoMessageSpec[SI <: SyncInfo](serializer: ErgoSerializer[SI]) extends MessageSpecV1[SI] {

  override val messageCode: MessageCode = 65: Byte
  override val messageName: String = "Sync"

  override def serialize(data: SI, w: Writer): Unit = serializer.serialize(data, w)

  override def parse(r: Reader): SI = serializer.parse(r)
}

/**
  * The `Inv` message (inventory message) transmits one or more inventories of
  * objects known to the transmitting peer.
  * It can be sent unsolicited to announce new transactions or blocks,
  * or it can be sent in reply to a `SyncInfo` message (or application-specific messages like `GetMempool`).
  *
  */
object InvSpec extends MessageSpecV1[InvData] {

  val maxInvObjects: Int = 400

  override val messageCode: MessageCode = 55: Byte
  override val messageName: String = "Inv"

  override def serialize(data: InvData, w: Writer): Unit = {
    val typeId = data.typeId
    val elems = data.ids
    require(elems.nonEmpty, "empty inv list")
    require(elems.lengthCompare(maxInvObjects) <= 0, s"more invs than $maxInvObjects in a message")
    w.put(typeId)
    w.putUInt(elems.size)
    elems.foreach { id =>
      val bytes = idToBytes(id)
      assert(bytes.length == NodeViewModifier.ModifierIdSize)
      w.putBytes(bytes)
    }
  }

  override def parse(r: Reader): InvData = {
    val typeId = NetworkObjectTypeId.fromByte(r.getByte())
    val count = r.getUInt().toIntExact
    require(count > 0, "empty inv list")
    require(count <= maxInvObjects, s"$count elements in a message while limit is $maxInvObjects")
    val elems = (0 until count).map { _ =>
      bytesToId(r.getBytes(NodeViewModifier.ModifierIdSize))
    }

    InvData(typeId, elems)
  }

}

/**
  * The `RequestModifier` message requests one or more modifiers from another node.
  * The objects are requested by an inventory, which the requesting node
  * typically received previously by way of an `Inv` message.
  *
  * This message cannot be used to request arbitrary data, such as historic transactions no
  * longer in the memory pool. Full nodes may not even be able to provide older blocks if
  * they’ve pruned old transactions from their block database.
  * For this reason, the `RequestModifier` message should usually only be used to request
  * data from a node which previously advertised it had that data by sending an `Inv` message.
  *
  */
object RequestModifierSpec extends MessageSpecV1[InvData] {
  override val messageCode: MessageCode = 22: Byte
  override val messageName: String = "RequestModifier"

  override def serialize(data: InvData, w: Writer): Unit = {
    InvSpec.serialize(data, w)
  }

  override def parse(r: Reader): InvData = {
    InvSpec.parse(r)
  }
}

/**
  * The `Modifier` message is a reply to a `RequestModifier` message which requested these modifiers.
  */
object ModifiersSpec extends MessageSpecV1[ModifiersData] with ScorexLogging {

  val maxMessageSize: Int = 2048576

  private val maxMsgSizeWithReserve = maxMessageSize * 4 // due to big ADProofs

  override val messageCode: MessageCode = 33: Byte
  override val messageName: String = "Modifier"

  private val HeaderLength = 5 // msg type Id + modifiersCount

  override def serialize(data: ModifiersData, w: Writer): Unit = {

    val typeId = data.typeId
    val modifiers = data.modifiers
    require(modifiers.nonEmpty, "empty modifiers list")

    val (msgCount, msgSize) = modifiers.foldLeft((0, HeaderLength)) { case ((c, s), (_, modifier)) =>
      val size = s + NodeViewModifier.ModifierIdSize + 4 + modifier.length
      val count = if (size <= maxMsgSizeWithReserve) c + 1 else c
      count -> size
    }

    w.put(typeId)
    w.putUInt(msgCount)

    modifiers.take(msgCount).foreach { case (id, modifier) =>
      w.putBytes(idToBytes(id))
      w.putUInt(modifier.length)
      w.putBytes(modifier)
    }

    if (msgSize > maxMsgSizeWithReserve) {
      log.warn(s"Message with modifiers ${modifiers.keySet} has size $msgSize exceeding limit $maxMsgSizeWithReserve.")
    }
  }

  override def parse(r: Reader): ModifiersData = {
    val typeId = NetworkObjectTypeId.fromByte(r.getByte()) // 1 byte
    val count = r.getUInt().toIntExact // 8 bytes
    require(count > 0, s"Illegal message with 0 modifiers of type $typeId")
    val resMap = immutable.Map.newBuilder[ModifierId, Array[Byte]]
    (0 until count).foldLeft(HeaderLength) { case (msgSize, _) =>
      val id = bytesToId(r.getBytes(NodeViewModifier.ModifierIdSize))
      val objBytesCnt = r.getUInt().toIntExact
      val newMsgSize = msgSize + NodeViewModifier.ModifierIdSize + objBytesCnt
      if (newMsgSize > maxMsgSizeWithReserve) { // buffer for safety
        throw new Exception("Too big message with modifiers, size: " + maxMsgSizeWithReserve)
      }
      val obj = r.getBytes(objBytesCnt)
      resMap += (id -> obj)
      newMsgSize
    }
    ModifiersData(typeId, resMap.result())
  }
}

/**
  * The `GetPeer` message requests an `Peers` message from the receiving node,
  * preferably one with lots of `PeerSpec` of other receiving nodes.
  * The transmitting node can use those `PeerSpec` addresses to quickly update
  * its database of available nodes rather than waiting for unsolicited `Peers`
  * messages to arrive over time.
  */
object GetPeersSpec extends MessageSpecV1[Unit] {
  override val messageCode: Message.MessageCode = 1: Byte

  override val messageName: String = "GetPeers message"

  override def serialize(obj: Unit, w: Writer): Unit = {
  }

  override def parse(r: Reader): Unit = {
    require(r.remaining == 0, "Non-empty data for GetPeers")
  }
}

object PeersSpec {

  val messageCode: Message.MessageCode = 2: Byte

  val messageName: String = "Peers message"

}

/**
  * The `Peers` message is a reply to a `GetPeer` message and relays connection information about peers
  * on the network.
  */
class PeersSpec(peersLimit: Int) extends MessageSpecV1[Seq[PeerSpec]] {

  override val messageCode: Message.MessageCode = PeersSpec.messageCode

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
  * The `Handshake` message provides information about the transmitting node
  * to the receiving node at the beginning of a connection. Until both peers
  * have exchanged `Handshake` messages, no other messages will be accepted.
  */
object HandshakeSerializer extends MessageSpecV1[Handshake] {
  override val messageCode: MessageCode = 75: Byte
  override val messageName: String = "Handshake"

  val maxHandshakeSize: Int = 8096

  /**
    * Serializing handshake into a byte writer.
    *
    * @param hs - handshake instance
    * @param w  - writer to write bytes to
    */
  override def serialize(hs: Handshake, w: Writer): Unit = {
    // first writes down handshake time, then peer specification of our node
    w.putULong(hs.time)
    PeerSpecSerializer.serialize(hs.peerSpec, w)
  }

  override def parse(r: Reader): Handshake = {
    require(r.remaining <= maxHandshakeSize, s"Too big handshake. Size ${r.remaining} exceeds $maxHandshakeSize limit")
    val time = r.getULong()
    val data = PeerSpecSerializer.parse(r)
    Handshake(data, time)
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

/**
  * The `GetNipopowProof` message requests a `NipopowProof` message from the receiving node
  */
object GetNipopowProofSpec extends MessageSpecV1[NipopowProofData] {

  val SizeLimit = 1000

  val messageCode: MessageCode = 90: Byte
  val messageName: String = "GetNipopowProof"

  override def serialize(data: NipopowProofData, w: Writer): Unit = {
    w.putInt(data.m)
    w.putInt(data.k)
    data.headerIdBytesOpt match {
      case Some(idBytes) =>
        w.put(1)
        w.putBytes(idBytes)
      case None =>
        w.put(0)
    }
    w.putUShort(0) // to allow adding new data in future, we are adding possible pad length
  }

  override def parse(r: Reader): NipopowProofData = {
    require(r.remaining <= SizeLimit, s"Too big GetNipopowProofSpec message(size: ${r.remaining})")

    val m = r.getInt()
    val k = r.getInt()

    val headerIdPresents = r.getByte() == 1
    val headerIdOpt = if (headerIdPresents) {
      Some(ModifierId @@ Algos.encode(r.getBytes(ModifierIdLength)))
    } else {
      None
    }
    val remainingBytes = r.getUShort()
    if (remainingBytes > 0 && remainingBytes < SizeLimit) {
      r.getBytes(remainingBytes) // current version of reader just skips possible additional bytes
    }
    NipopowProofData(m, k, headerIdOpt)
  }

}

/**
  * The `NipopowProof` message is a reply to a `GetNipopowProof` message.
  */
class NipopowProofSpec(serializer: ErgoSerializer[NipopowProof]) extends MessageSpecV1[NipopowProof] {

  import NipopowProofSpec._

  val SizeLimit = 2000000

  override val messageCode: MessageCode = MessageCode
  override val messageName: String = MessageName

  override def serialize(proof: NipopowProof, w: Writer): Unit = {
    serializer.serialize(proof, w)
    w.putUShort(0) // to allow adding new data in future, we are adding possible pad length
  }

  override def parse(r: Reader): NipopowProof = {
    require(r.remaining <= SizeLimit, s"Too big NipopowProofSpec message(size: ${r.remaining})")
    val proof = serializer.parse(r)
    val remainingBytes = r.getUShort()
    if (remainingBytes > 0 && remainingBytes < SizeLimit) {
      r.getBytes(remainingBytes) // current version of reader just skips possible additional bytes
    }
    proof
  }

}

object NipopowProofSpec {

  val MessageCode: Byte = 91
  val MessageName: String = "NipopowProof"

  def apply(ergoSettings: ErgoSettings): NipopowProofSpec = {
    new NipopowProofSpec(new NipopowProofSerializer(new NipopowAlgos(ergoSettings.chainSettings)))
  }

}
