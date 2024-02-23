package org.ergoplatform.network.message

import org.ergoplatform.network.message.MessageConstants.MessageCode
import scorex.util.serialization.{Reader, Writer}

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
  override val messageName: String      = "RequestModifier"

  override def serialize(data: InvData, w: Writer): Unit = {
    InvSpec.serialize(data, w)
  }

  override def parse(r: Reader): InvData = {
    InvSpec.parse(r)
  }
}
