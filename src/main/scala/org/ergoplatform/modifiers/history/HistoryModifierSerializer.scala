package org.ergoplatform.modifiers.history

import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.history.extension.{Extension, ExtensionSerializer}
import org.ergoplatform.modifiers.history.header.{Header, HeaderSerializer}
import org.ergoplatform.nodeView.history.extra.{IndexedErgoAddress, IndexedErgoAddressSerializer, IndexedErgoBox, IndexedErgoBoxSerializer, IndexedErgoTransaction, IndexedErgoTransactionSerializer, IndexedErgoTree, IndexedErgoTreeSerializer}
import scorex.core.serialization.ScorexSerializer
import scorex.util.serialization.{Reader, Writer}

object HistoryModifierSerializer extends ScorexSerializer[BlockSection] {

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
      case m: IndexedErgoTree =>
        w.put(IndexedErgoTree.modifierTypeId)
        IndexedErgoTreeSerializer.serialize(m, w)
      case m: IndexedErgoAddress =>
        w.put(IndexedErgoAddress.modifierTypeId)
        IndexedErgoAddressSerializer.serialize(m, w)
      case m: IndexedErgoTransaction =>
        w.put(IndexedErgoTransaction.modifierTypeId)
        IndexedErgoTransactionSerializer.serialize(m, w)
      case m: IndexedErgoBox =>
        w.put(IndexedErgoBox.modifierTypeId)
        IndexedErgoBoxSerializer.serialize(m, w)
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
      case IndexedErgoTree.`modifierTypeId` =>
        IndexedErgoTreeSerializer.parse(r)
      case IndexedErgoAddress.`modifierTypeId` =>
        IndexedErgoAddressSerializer.parse(r)
      case IndexedErgoTransaction.`modifierTypeId` =>
        IndexedErgoTransactionSerializer.parse(r)
      case IndexedErgoBox.`modifierTypeId` =>
        IndexedErgoBoxSerializer.parse(r)
      case m =>
        throw new Error(s"Deserialization for unknown type byte: $m")
    }
  }
}
