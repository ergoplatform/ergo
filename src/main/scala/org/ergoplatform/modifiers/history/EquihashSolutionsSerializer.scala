package org.ergoplatform.modifiers.history

import com.google.common.primitives.Ints
import org.ergoplatform.mining.PoWScheme
import scorex.core.serialization.Serializer

import scala.util.Try

object EquihashSolutionsSerializer extends Serializer[PoWScheme.Solution] {
  override def toBytes(obj: PoWScheme.Solution): Array[Byte] = {
    obj.map(Ints.toByteArray).reduceLeft(_ ++ _)
  }

  override def parseBytes(bytes: Array[Byte]): Try[PoWScheme.Solution] = Try {
    for {i <- bytes.indices by Ints.BYTES} yield {
      Ints.fromByteArray(bytes.slice(i, i + Ints.BYTES))
    }
  }
}
