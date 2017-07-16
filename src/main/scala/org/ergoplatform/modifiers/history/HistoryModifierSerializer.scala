package org.ergoplatform.modifiers.history

import scorex.core.serialization.Serializer

import scala.util.Try

object HistoryModifierSerializer extends Serializer[HistoryModifier] {
  override def toBytes(obj: HistoryModifier): Array[Byte] = obj match {
    case h: Header =>
      Header.ModifierTypeId +: HeaderSerializer.toBytes(h)
    case _ => ???
  }

  override def parseBytes(bytes: Array[Byte]): Try[HistoryModifier] = Try {
    bytes.head match {
      case Header.ModifierTypeId =>
        HeaderSerializer.parseBytes(bytes.tail).get
      case _ =>
        ???
    }
  }
}
