package org.ergoplatform.modifiers.history

import org.ergoplatform.modifiers.ErgoPersistentModifier
import scorex.core.serialization.Serializer

import scala.util.{Failure, Try}

object HistoryModifierSerializer extends Serializer[ErgoPersistentModifier] {
  override def toBytes(obj: ErgoPersistentModifier): Array[Byte] = obj match {
    case m: Header =>
      Header.modifierTypeId +: HeaderSerializer.toBytes(m)
    case m: ADProofs =>
      ADProofs.modifierTypeId +: ADProofSerializer.toBytes(m)
    case m: BlockTransactions =>
      BlockTransactions.modifierTypeId +: BlockTransactionsSerializer.toBytes(m)
    case m =>
      throw new Error(s"Serialization for unknown modifier: ${m.json.noSpaces}")
  }

  override def parseBytes(bytes: Array[Byte]): Try[ErgoPersistentModifier] = Try(bytes.head).flatMap {
    case Header.`modifierTypeId` =>
      HeaderSerializer.parseBytes(bytes.tail)
    case ADProofs.`modifierTypeId` =>
      ADProofSerializer.parseBytes(bytes.tail)
    case BlockTransactions.`modifierTypeId` =>
      BlockTransactionsSerializer.parseBytes(bytes.tail)
    case m =>
      Failure(new Error(s"Deserialization for unknown type byte: $m"))
  }
}
