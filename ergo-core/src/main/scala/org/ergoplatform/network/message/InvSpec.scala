package org.ergoplatform.network.message

import org.ergoplatform.modifiers.ErgoNodeViewModifier
import org.ergoplatform.modifiers.NetworkObjectTypeId
import org.ergoplatform.network.message.MessageConstants.MessageCode
import scorex.util.Extensions.LongOps
import scorex.util.{bytesToId, idToBytes}
import scorex.util.serialization.{Reader, Writer}

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
      assert(bytes.length == ErgoNodeViewModifier.ModifierIdSize)
      w.putBytes(bytes)
    }
  }

  override def parse(r: Reader): InvData = {
    val typeId = NetworkObjectTypeId.fromByte(r.getByte())
    val count = r.getUInt().toIntExact
    require(count > 0, "empty inv list")
    require(count <= maxInvObjects, s"$count elements in a message while limit is $maxInvObjects")
    val elems = (0 until count).map { _ =>
      bytesToId(r.getBytes(ErgoNodeViewModifier.ModifierIdSize))
    }

    InvData(typeId, elems)
  }

}
