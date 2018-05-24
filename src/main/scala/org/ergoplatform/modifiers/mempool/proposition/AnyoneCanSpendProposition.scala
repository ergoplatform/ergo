package org.ergoplatform.modifiers.mempool.proposition

/*
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.ProofOfKnowledgeProposition

import scala.util.{Failure, Success, Try}

object AnyoneCanSpendProposition extends ProofOfKnowledgeProposition[Nothing] {
  override type M = AnyoneCanSpendProposition.type

  override lazy val serializer: Serializer[M] = AnyoneCanSpendPropositionSerializer
}

object AnyoneCanSpendPropositionSerializer extends Serializer[AnyoneCanSpendProposition.type] with Serializable {
  val Length = 1
  val ByteValue: Array[Byte] = Array.fill(Length)(-127: Byte)

  override def toBytes(obj: AnyoneCanSpendProposition.type): Array[Byte] = ByteValue

  override def parseBytes(bytes: Array[Byte]): Try[AnyoneCanSpendProposition.type] = bytes match {
    case b if b sameElements ByteValue => Success(AnyoneCanSpendProposition)
    case l => Failure(new Error(s"Incorrect proposition ${l.headOption}"))
  }
} */

