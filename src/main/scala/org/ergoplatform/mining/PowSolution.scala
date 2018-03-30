package org.ergoplatform.mining

import io.circe.Encoder
import org.ergoplatform.modifiers.history.EquihashSolutionsSerializer
import org.ergoplatform.settings.Constants
import scorex.core.serialization.{BytesSerializable, Serializer}

case class PowSolution(ints: Seq[Int]) extends BytesSerializable {
  def indexedSeq: IndexedSeq[Int] = ints.toIndexedSeq
  def headOption: Option[Int] = ints.headOption
  def byteLength: Int = ints.length * 4
  def serializer: Serializer[PowSolution] = EquihashSolutionsSerializer
  type M = PowSolution
}

object PowSolution {

  val length: Int = Constants.hashLength
  def empty: PowSolution = PowSolution(Seq.fill(length)(0))

  /** This is for json representation of [[PowSolution]] instances */
  implicit val jsonEncoder: Encoder[PowSolution] =
    Encoder.encodeSeq[Int].contramap[PowSolution](_.ints)
}
