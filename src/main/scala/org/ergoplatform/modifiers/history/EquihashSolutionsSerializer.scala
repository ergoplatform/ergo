package org.ergoplatform.modifiers.history

import com.google.common.primitives.{Ints, Shorts}
import org.ergoplatform.mining.Miner
import org.ergoplatform.mining.Miner.Solution
import scorex.core.serialization.Serializer

import scala.util.Try

object EquihashSolutionsSerializer extends Serializer[Miner.Solution] {
  override def toBytes(obj: Solution) = {
    obj.map(Ints.toByteArray).reduceLeft(_ ++ _)
  }

  override def parseBytes(bytes: Array[Byte]) = Try {
    for {i <- bytes.indices by Ints.BYTES} yield {
      Ints.fromByteArray(bytes.slice(i, i + Ints.BYTES))
    }
  }
}
