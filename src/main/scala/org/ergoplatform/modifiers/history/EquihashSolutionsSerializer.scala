package org.ergoplatform.modifiers.history

import com.google.common.primitives.Ints
import scorex.core.serialization.Serializer

import scala.util.Try

object EquihashSolutionsSerializer extends Serializer[PoWScheme.Solution] {
  override def toBytes(obj: PoWScheme.Solution) = {
    obj.map(Ints.toByteArray).reduceLeft(_ ++ _)
  }

  override def parseBytes(bytes: Array[Byte]) = Try {
    for {i <- bytes.indices by Ints.BYTES} yield {
      Ints.fromByteArray(bytes.slice(i, i + Ints.BYTES))
    }
  }
}
