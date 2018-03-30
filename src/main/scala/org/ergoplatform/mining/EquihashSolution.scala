package org.ergoplatform.mining

import io.circe.Encoder
import org.ergoplatform.modifiers.history.EquihashSolutionsSerializer
import org.ergoplatform.settings.Constants
import scorex.core.serialization.{BytesSerializable, Serializer}

case class EquihashSolution(ints: Seq[Int]) extends BytesSerializable {
  def indexedSeq: IndexedSeq[Int] = ints.toIndexedSeq
  def headOption: Option[Int] = ints.headOption
  def byteLength: Int = ints.length * 4
  def serializer: Serializer[EquihashSolution] = EquihashSolutionsSerializer
  type M = EquihashSolution
}

object EquihashSolution {

  val length: Int = Constants.hashLength
  def empty: EquihashSolution = EquihashSolution(Seq.fill(length)(0))

  /** This is for json representation of [[EquihashSolution]] instances */
  implicit val jsonEncoder: Encoder[EquihashSolution] =
    Encoder.encodeSeq[Int].contramap[EquihashSolution](_.ints)
}
