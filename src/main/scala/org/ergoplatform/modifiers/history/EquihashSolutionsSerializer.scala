package org.ergoplatform.modifiers.history

import com.google.common.primitives.Ints
import org.ergoplatform.mining.PowSolution
import scorex.core.serialization.Serializer

import scala.util.Try

object EquihashSolutionsSerializer extends Serializer[PowSolution] {
  override def toBytes(obj: PowSolution): Array[Byte] = {
    obj.ints.map(Ints.toByteArray).reduceLeft(_ ++ _)
  }

  override def parseBytes(bytes: Array[Byte]): Try[PowSolution] = Try {
    val seq = for {i <- bytes.indices by Ints.BYTES} yield {
      Ints.fromByteArray(bytes.slice(i, i + Ints.BYTES))
    }
    PowSolution(seq)
  }
}
