package org.ergoplatform.modifiers.history

import scorex.core.serialization.Serializer

import scala.util.Try

object HistoryModifierSerializer extends Serializer[HistoryModifier] {
  override def toBytes(obj: HistoryModifier): Array[Byte] = obj match {
    case h: Header =>
      Header.ModifierTypeId +: HeaderSerializer.toBytes(h)
    case m =>
      throw new Error(s"Serialization for unknown modifier: ${m.json.noSpaces}")
  }

  override def parseBytes(bytes: Array[Byte]): Try[HistoryModifier] = Try {
    bytes.head match {
      case Header.ModifierTypeId =>
        HeaderSerializer.parseBytes(bytes.tail).get
      case m =>
        throw new Error(s"Deserialization for unknown type byte: $m")
    }
  }
}
