package org.ergoplatform.modifiers.history

import org.ergoplatform.modifiers.ErgoPersistentModifier
import scorex.core.serialization.ScorexSerializer
import scorex.util.serialization.{Reader, Writer}

object HistoryModifierSerializer extends ScorexSerializer[ErgoPersistentModifier] {

  override def serialize(obj: ErgoPersistentModifier, w: Writer): Unit =
    obj match {
      case m: Header =>
        w.put(Header.modifierTypeId)
        HeaderSerializer.serialize(m, w)
      case m: ADProofs =>
        w.put(ADProofs.TypeId)
        ADProofSerializer.serialize(m, w)
      case m: BlockTransactions =>
        w.put(BlockTransactions.TypeId)
        BlockTransactionsSerializer.serialize(m, w)
      case m: Extension =>
        w.put(Extension.TypeId)
        ExtensionSerializer.serialize(m, w)
      case m: PoPowProof =>
        w.put(PoPowProof.TypeId)
        PoPowProofSerializer.serialize(m, w)
      case m =>
        throw new Error(s"Serialization for unknown modifier: $m")
    }

  override def parse(r: Reader): ErgoPersistentModifier =
    r.getByte() match {
      case Header.`modifierTypeId` =>
        HeaderSerializer.parse(r)
      case ADProofs.TypeId =>
        ADProofSerializer.parse(r)
      case BlockTransactions.TypeId =>
        BlockTransactionsSerializer.parse(r)
      case Extension.TypeId =>
        ExtensionSerializer.parse(r)
      case PoPowProof.TypeId =>
        PoPowProofSerializer.parse(r)
      case m =>
        throw new Error(s"Deserialization for unknown type byte: $m")
    }

}
