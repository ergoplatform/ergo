package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.serialization.ErgoSerializer
import scorex.util.serialization.{Reader, Writer}

object ExtraIndexSerializer  extends ErgoSerializer[ExtraIndex]{

    override def serialize(obj: ExtraIndex, w: Writer): Unit = {
      obj match {
        case m: IndexedErgoAddress =>
          w.put(IndexedErgoAddress.extraIndexTypeId)
          IndexedErgoAddressSerializer.serialize(m, w)
        case m: IndexedContractTemplate =>
          w.put(IndexedContractTemplate.extraIndexTypeId)
          IndexedContractTemplateSerializer.serialize(m, w)
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
        case _: StorageRentBox =>
          w.put(StorageRentBox.extraIndexTypeId) // key-only row, no payload
        case m =>
          throw new IllegalStateException(s"Serialization for unknown index: $m")
      }
    }

    override def parse(r: Reader): ExtraIndex = {
      r.getByte() match {
        case IndexedErgoAddress.`extraIndexTypeId` =>
          IndexedErgoAddressSerializer.parse(r)
        case IndexedContractTemplate.`extraIndexTypeId` =>
          IndexedContractTemplateSerializer.parse(r)
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
        case StorageRentBox.`extraIndexTypeId` =>
          // key-only row: everything is in the key, reconstruct via StorageRentBox.fromKey
          throw new IllegalStateException(
            "StorageRentBox rows carry no payload; reconstruct from the key via StorageRentBox.fromKey")
        case m =>
          throw new IllegalStateException(s"Deserialization for unknown type byte: $m")
      }
    }
  }
