package org.ergoplatform.network.message

import org.ergoplatform.modifiers.{ErgoNodeViewModifier, NetworkObjectTypeId}
import org.ergoplatform.network.message.MessageConstants.MessageCode
import scorex.util.{ModifierId, ScorexLogging, bytesToId, idToBytes}
import scorex.util.serialization.{Reader, Writer}
import scorex.util.Extensions._
import scala.collection.immutable

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
      val size = s + ErgoNodeViewModifier.ModifierIdSize + 4 + modifier.length
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
      val id = bytesToId(r.getBytes(ErgoNodeViewModifier.ModifierIdSize))
      val objBytesCnt = r.getUInt().toIntExact
      val newMsgSize = msgSize + ErgoNodeViewModifier.ModifierIdSize + objBytesCnt
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
