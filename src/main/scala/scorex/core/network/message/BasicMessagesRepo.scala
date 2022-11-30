package scorex.core.network.message


import org.ergoplatform.nodeView.state.SnapshotsInfo
import org.ergoplatform.nodeView.state.UtxoState.{ManifestId, SubtreeId}
import org.ergoplatform.wallet.Constants
import scorex.core.consensus.SyncInfo
import scorex.core.network._
import scorex.core.network.message.Message.MessageCode
import scorex.core.serialization.ScorexSerializer
import scorex.core.{ModifierTypeId, NodeViewModifier}
import scorex.crypto.hash.Digest32
import scorex.util.Extensions._
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, ScorexLogging, bytesToId, idToBytes}

import scala.collection.immutable

case class ModifiersData(typeId: ModifierTypeId, modifiers: Map[ModifierId, Array[Byte]])

case class InvData(typeId: ModifierTypeId, ids: Seq[ModifierId])

/**
  * The `SyncInfo` message requests an `Inv` message that provides modifier ids
  * required be sender to synchronize his blockchain with the recipient.
  * It allows a peer which has been disconnected or started for the first
  * time to get the data it needs to request the blocks it hasn't seen.
  *
  * Payload of this message should be determined in underlying applications.
  */
class SyncInfoMessageSpec[SI <: SyncInfo](serializer: ScorexSerializer[SI]) extends MessageSpecV1[SI] {

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
    val typeId = ModifierTypeId @@ r.getByte()
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
    val typeId = ModifierTypeId @@ r.getByte() // 1 byte
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
  */
object SnapshotsInfoSpec extends MessageSpecV1[SnapshotsInfo] {
  private val SizeLimit = 1000

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
    require(r.remaining <= SizeLimit, s"Too big SnapshotsInfo message.")

    val length = r.getUInt().toIntExact
    SnapshotsInfo((0 until length).map { _ =>
      val height = r.getInt()
      val manifest = Digest32 @@ r.getBytes(Constants.ModifierIdLength)
      height -> manifest
    }.toMap)
  }
}

/**
  * The `GetManifest` sends manifest (BatchAVLProverManifest) identifier
  */
class GetManifestSpec extends MessageSpecV1[ManifestId] {
  private val SizeLimit = 100

  override val messageCode: MessageCode = 78: Byte
  override val messageName: String = "GetManifest"

  override def serialize(id: ManifestId, w: Writer): Unit = {
    w.putBytes(id)
  }

  override def parse(r: Reader): ManifestId = {
    require(r.remaining < SizeLimit, "Too big GetManifest message")
    Digest32 @@ r.getBytes(Constants.ModifierIdLength)
  }
}

/**
  * The `Manifest` message is a reply to a `GetManifest` message.
  */
object ManifestSpec extends MessageSpecV1[Array[Byte]] {
  private val SizeLimit = 10000000

  override val messageCode: MessageCode = 79: Byte

  override val messageName: String = "Manifest"

  override def serialize(manifestBytes: Array[Byte], w: Writer): Unit = {
    w.putUInt(manifestBytes.size)
    w.putBytes(manifestBytes)
  }

  override def parse(r: Reader): Array[Byte] = {
    require(r.remaining <= SizeLimit, s"Too big Manifest message.")

    val length = r.getUInt().toIntExact
    r.getBytes(length)
  }
}

/**
  * The `GetManifest` sends send utxo subtree (BatchAVLProverSubtree) identifier
  */
class GetUtxoSnapshotChunkSpec() extends MessageSpecV1[SubtreeId] {
  private val SizeLimit = 100

  override val messageCode: MessageCode = 80: Byte

  override val messageName: String = "GetUtxoSnapshotChunk"

  override def serialize(id: SubtreeId, w: Writer): Unit = {
    w.putBytes(id)
  }

  override def parse(r: Reader): SubtreeId = {
    require(r.remaining < SizeLimit, "Too big GetUtxoSnapshotChunk message")
    Digest32 @@ r.getBytes(Constants.ModifierIdLength)
  }
}

/**
  * The `Manifest` message is a reply to a `GetManifest` message.
  */
object UtxoSnapshotChunkSpec extends MessageSpecV1[Array[Byte]] {
  private val SizeLimit = 10000000

  override val messageCode: MessageCode = 81: Byte

  override val messageName: String = "UtxoSnapshotChunk"

  override def serialize(subtree: Array[Byte], w: Writer): Unit = {
    w.putUInt(subtree.size)
    w.putBytes(subtree)
  }

  override def parse(r: Reader): Array[Byte] = {
    require(r.remaining <= SizeLimit, s"Too big UtxoSnapshotChunk message.")

    val length = r.getUInt().toIntExact
    r.getBytes(length)
  }
}


