package org.ergoplatform.network.message

import org.ergoplatform.modifiers.{ErgoNodeViewModifier, NetworkObjectTypeId, ProofsTypeId}
import org.ergoplatform.network.message.MessageConstants.MessageCode
import scorex.util.{ModifierId, ScorexLogging, bytesToId, idToBytes}
import scorex.util.serialization.{Reader, Writer}
import scorex.util.Extensions._
import scala.collection.{immutable, mutable}

/**
 * The `Modifier` message is a reply to a `RequestModifier` message which requested these modifiers.
 */
object ModifiersSpec extends MessageSpecV1[ModifiersData] with ScorexLogging {

  val maxMessageSize: Int = 2048576

  val maxMsgSizeWithReserve: Int = maxMessageSize * 4 // due to big ADProofs

  /**
    * Max (estimated) size of a Modifiers message carrying exactly one AD proofs section.
    * Some mainnet AD proofs (e.g. blocks 1,211,757 and 1,211,759, ~8.8 MB each) do not fit into
    * `maxMsgSizeWithReserve`, so a single AD proofs section may use (almost) the whole frame.
    * Equal to `MessageConstants.MaxMessageSize` (defined as `maxMsgSizeWithReserve * 2`) minus a margin
    * for the variable-length encoding of the count and length fields. Defined here rather than via
    * `MessageConstants` to avoid an object initialization cycle (`MessageConstants` refers to this object).
    */
  val maxSingleAdProofsMsgSize: Int = maxMsgSizeWithReserve * 2 - 16

  override val messageCode: MessageCode = 33: Byte
  override val messageName: String = "Modifier"

  private val HeaderLength = 5 // msg type Id + modifiersCount

  // per-modifier overhead used in size estimation: modifier id + modifier length field
  private val PerModifierOverhead = ErgoNodeViewModifier.ModifierIdSize + 4

  /**
    * Max allowed (estimated) message size for a message with `count` modifiers of type `typeId`.
    * Only a message with exactly one AD proofs section gets the bigger limit.
    */
  def maxMessageSizeFor(typeId: NetworkObjectTypeId.Value, count: Int): Int = {
    if (count == 1 && typeId == ProofsTypeId.value) maxSingleAdProofsMsgSize else maxMsgSizeWithReserve
  }

  /**
    * Estimated size of a message carrying one modifier of `modifierLength` bytes.
    */
  def singleModifierMessageSize(modifierLength: Int): Long = {
    HeaderLength.toLong + PerModifierOverhead + modifierLength
  }

  /**
    * Splits modifiers requested by a peer into Modifiers messages, in order.
    * Modifiers are batched while the estimated message size stays below `maxMessageSize`.
    * A modifier too big for that is sent alone if it fits `maxMsgSizeWithReserve`, or, for an AD proofs
    * section, `maxSingleAdProofsMsgSize` when the peer accepts large AD proofs; otherwise it is skipped,
    * as the peer couldn't parse the message.
    *
    * @return messages to send, and skipped (id, length) pairs
    */
  def planBatches(typeId: NetworkObjectTypeId.Value,
                  mods: Seq[(ModifierId, Array[Byte])],
                  peerAcceptsLargeAdProofs: Boolean): (Seq[Seq[(ModifierId, Array[Byte])]], Seq[(ModifierId, Int)]) = {
    val batches = mutable.ArrayBuffer[Seq[(ModifierId, Array[Byte])]]()
    val skipped = mutable.ArrayBuffer[(ModifierId, Int)]()
    var rest = mods
    while (rest.nonEmpty) {
      var size = HeaderLength.toLong
      val batch = rest.takeWhile { case (_, modBytes) =>
        size += PerModifierOverhead + modBytes.length
        size < maxMessageSize
      }
      if (batch.nonEmpty) {
        batches += batch
        rest = rest.drop(batch.length)
      } else {
        val (id, bytes) = rest.head
        val single = singleModifierMessageSize(bytes.length)
        val limit = if (peerAcceptsLargeAdProofs) maxMessageSizeFor(typeId, 1) else maxMsgSizeWithReserve
        if (single <= limit) batches += Seq(rest.head) else skipped += (id -> bytes.length)
        rest = rest.tail
      }
    }
    batches.toSeq -> skipped.toSeq
  }

  override def serialize(data: ModifiersData, w: Writer): Unit = {

    val typeId = data.typeId
    val modifiers = data.modifiers
    require(modifiers.nonEmpty, "empty modifiers list")

    val maxSize = maxMessageSizeFor(typeId, modifiers.size)

    val (msgCount, msgSize) = modifiers.foldLeft((0, HeaderLength.toLong)) { case ((c, s), (_, modifier)) =>
      val size = s + PerModifierOverhead + modifier.length
      val count = if (size <= maxSize) c + 1 else c
      count -> size
    }

    w.put(typeId)
    w.putUInt(msgCount)

    modifiers.take(msgCount).foreach { case (id, modifier) =>
      w.putBytes(idToBytes(id))
      w.putUInt(modifier.length)
      w.putBytes(modifier)
    }

    if (msgSize > maxSize) {
      log.warn(s"Message with modifiers ${modifiers.keySet} has size $msgSize exceeding limit $maxSize.")
    }
  }

  override def parse(r: Reader): ModifiersData = {
    val typeId = NetworkObjectTypeId.fromByte(r.getByte()) // 1 byte
    val count = r.getUInt().toIntExact // 8 bytes
    require(count > 0, s"Illegal message with 0 modifiers of type $typeId")
    val resMap = immutable.Map.newBuilder[ModifierId, Array[Byte]]
    val maxSize = maxMessageSizeFor(typeId, count)
    (0 until count).foldLeft(HeaderLength.toLong) { case (msgSize, _) =>
      val id = bytesToId(r.getBytes(ErgoNodeViewModifier.ModifierIdSize))
      val objBytesCnt = r.getUInt().toIntExact
      // same size estimation as in serialize(), checked before reading (allocating) modifier bytes
      val newMsgSize = msgSize + PerModifierOverhead + objBytesCnt
      if (newMsgSize > maxSize) { // buffer for safety
        throw new Exception("Too big message with modifiers, size: " + maxSize)
      }
      val obj = r.getBytes(objBytesCnt)
      resMap += (id -> obj)
      newMsgSize
    }
    ModifiersData(typeId, resMap.result())
  }
}
