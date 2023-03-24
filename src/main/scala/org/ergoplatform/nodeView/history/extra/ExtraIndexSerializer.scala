package org.ergoplatform.nodeView.history.extra

import scorex.core.serialization.ErgoSerializer
import scorex.util.serialization.{Reader, Writer}

object ExtraIndexSerializer extends ErgoSerializer[ExtraIndex]{

    override def serialize(obj: ExtraIndex, w: Writer): Unit = {
      obj match {
        case m: IndexedErgoAddress =>
          w.put(IndexedErgoAddress.extraIndexTypeId)
          IndexedErgoAddressSerializer.serialize(m, w)
        case m: IndexedErgoTransaction =>
          w.put(IndexedErgoTransaction.extraIndexTypeId)
          IndexedErgoTransactionSerializer.serialize(m, w)
        case m: IndexedErgoBox =>
          w.put(IndexedErgoBox.extraIndexTypeId)
          IndexedErgoBoxSerializer.serialize(m, w)
        case m: NumericTxIndex =>
          w.put(NumericTxIndex.extraIndexTypeId)
          NumericTxIndexSerializer.serialize(m, w)
        case m: NumericBoxIndex =>
          w.put(NumericBoxIndex.extraIndexTypeId)
          NumericBoxIndexSerializer.serialize(m, w)
        case m: IndexedToken =>
          w.put(IndexedToken.extraIndexTypeId)
          IndexedTokenSerializer.serialize(m, w)
        case m =>
          throw new Error(s"Serialization for unknown index: $m")
      }
    }

    override def parse(r: Reader): ExtraIndex = {
      r.getByte() match {
        case IndexedErgoAddress.`extraIndexTypeId` =>
          IndexedErgoAddressSerializer.parse(r)
        case IndexedErgoTransaction.`extraIndexTypeId` =>
          IndexedErgoTransactionSerializer.parse(r)
        case IndexedErgoBox.`extraIndexTypeId` =>
          IndexedErgoBoxSerializer.parse(r)
        case NumericTxIndex.`extraIndexTypeId` =>
          NumericTxIndexSerializer.parse(r)
        case NumericBoxIndex.`extraIndexTypeId` =>
          NumericBoxIndexSerializer.parse(r)
        case IndexedToken.`extraIndexTypeId` =>
          IndexedTokenSerializer.parse(r)
        case m =>
          throw new Error(s"Deserialization for unknown type byte: $m")
      }
    }
  }
