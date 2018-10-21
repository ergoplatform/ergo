package org.ergoplatform.modifiers.history

import org.ergoplatform.mining.AutoleakusSolution
import scorex.core.serialization.Serializer

import scala.util.Try

object PowSolutionsSerializer extends Serializer[AutoleakusSolution] {
  override def toBytes(obj: AutoleakusSolution): Array[Byte] = ???

  override def parseBytes(bytes: Array[Byte]): Try[AutoleakusSolution] = ???
}
