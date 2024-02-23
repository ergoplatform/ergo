package org.ergoplatform.network.message

import org.ergoplatform.consensus.SyncInfo
import org.ergoplatform.network.message.MessageConstants.MessageCode
import org.ergoplatform.serialization.ErgoSerializer
import scorex.util.serialization.{Reader, Writer}

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
