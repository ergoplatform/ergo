package org.ergoplatform.modifiers.history

import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.history.extension.{Extension, ExtensionSerializer}
import org.ergoplatform.modifiers.history.header.{Header, HeaderSerializer}
import org.ergoplatform.serialization.ErgoSerializer
import scorex.util.serialization.{Reader, Writer}

object HistoryModifierSerializer extends ErgoSerializer[BlockSection] {

  override def serialize(obj: BlockSection, w: Writer): Unit = {
    obj match {
      case m: Header =>
        w.put(Header.modifierTypeId)
        HeaderSerializer.serialize(m, w)
      case m: ADProofs =>
        w.put(ADProofs.modifierTypeId)
        ADProofsSerializer.serialize(m, w)
      case m: BlockTransactions =>
        w.put(BlockTransactions.modifierTypeId)
        BlockTransactionsSerializer.serialize(m, w)
      case m: Extension =>
        w.put(Extension.modifierTypeId)
        ExtensionSerializer.serialize(m, w)
      case m =>
        throw new Error(s"Serialization for unknown modifier: $m")
    }
  }

  override def parse(r: Reader): BlockSection = {
    r.getByte() match {
      case Header.`modifierTypeId` =>
        HeaderSerializer.parse(r)
      case ADProofs.`modifierTypeId` =>
        ADProofsSerializer.parse(r)
      case BlockTransactions.`modifierTypeId` =>
        BlockTransactionsSerializer.parse(r)
      case Extension.`modifierTypeId` =>
        ExtensionSerializer.parse(r)
      case m =>
        throw new Error(s"Deserialization for unknown type byte: $m")
    }
  }
}
