package org.ergoplatform.modifiers.history

import com.google.common.primitives.Ints
import org.ergoplatform.mining.EquihashSolution
import scorex.core.serialization.Serializer

import scala.util.Try

object EquihashSolutionsSerializer extends Serializer[EquihashSolution] {
  override def toBytes(obj: EquihashSolution): Array[Byte] = {
    obj.ints.map(Ints.toByteArray).reduceLeft(_ ++ _)
  }

  override def parseBytes(bytes: Array[Byte]): Try[EquihashSolution] = Try {
    val seq = for {i <- bytes.indices by Ints.BYTES} yield {
      Ints.fromByteArray(bytes.slice(i, i + Ints.BYTES))
    }
    EquihashSolution(seq)
  }
}
