package org.ergoplatform.utils

import org.ergoplatform.local.ErgoStatsCollector.NodeInfo
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.mempool.TransactionIdsForHeader
import scorex.core.serialization.SerializerRegistry
import scorex.core.serialization.SerializerRegistry.SerializerRecord

object JsonSerialization {
  implicit val serializerReg: SerializerRegistry = SerializerRegistry(Seq(
    SerializerRecord(TransactionIdsForHeader.jsonEncoder),
    SerializerRecord(Header.jsonEncoder),
    SerializerRecord(NodeInfo.jsonEncoder)
  ))

}
