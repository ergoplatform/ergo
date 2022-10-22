package org.ergoplatform.nodeView.history.extra

import scorex.core.serialization.ScorexSerializer
import scorex.util.serialization.{Reader, Writer}

object ExtraIndexSerializer  extends ScorexSerializer[ExtraIndex]{

    override def serialize(obj: ExtraIndex, w: Writer): Unit = {
      obj match {
        case m: IndexedErgoAddress =>
          w.put(IndexedErgoAddress.modifierTypeId)
          IndexedErgoAddressSerializer.serialize(m, w)
        case m: IndexedErgoTransaction =>
          w.put(IndexedErgoTransaction.modifierTypeId)
          IndexedErgoTransactionSerializer.serialize(m, w)
        case m: IndexedErgoBox =>
          w.put(IndexedErgoBox.modifierTypeId)
          IndexedErgoBoxSerializer.serialize(m, w)
        case m: NumericTxIndex =>
          w.put(NumericTxIndex.modifierTypeId)
          NumericTxIndexSerializer.serialize(m, w)
        case m: NumericBoxIndex =>
          w.put(NumericBoxIndex.modifierTypeId)
          NumericBoxIndexSerializer.serialize(m, w)
        case m: IndexedToken =>
          w.put(IndexedToken.modifierTypeId)
          IndexedTokenSerializer.serialize(m, w)
        case m =>
          throw new Error(s"Serialization for unknown index: $m")
      }
    }

    override def parse(r: Reader): ExtraIndex = {
      r.getByte() match {
        case IndexedErgoAddress.`modifierTypeId` =>
          IndexedErgoAddressSerializer.parse(r)
        case IndexedErgoTransaction.`modifierTypeId` =>
          IndexedErgoTransactionSerializer.parse(r)
        case IndexedErgoBox.`modifierTypeId` =>
          IndexedErgoBoxSerializer.parse(r)
        case NumericTxIndex.`modifierTypeId` =>
          NumericTxIndexSerializer.parse(r)
        case NumericBoxIndex.`modifierTypeId` =>
          NumericBoxIndexSerializer.parse(r)
        case IndexedToken.`modifierTypeId` =>
          IndexedTokenSerializer.parse(r)
        case m =>
          throw new Error(s"Deserialization for unknown type byte: $m")
      }
    }
  }
