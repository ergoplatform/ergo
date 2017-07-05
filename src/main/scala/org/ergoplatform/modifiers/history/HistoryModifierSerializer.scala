package org.ergoplatform.modifiers.history

import scorex.core.serialization.Serializer

import scala.util.Try

object HistoryModifierSerializer extends Serializer[HistoryModifier] {
  override def toBytes(obj: HistoryModifier): Array[Byte] = ???

  override def parseBytes(bytes: Array[Byte]): Try[HistoryModifier] = ???
}
