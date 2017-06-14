package org.ergoplatform.transaction.proposition

import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.ProofOfKnowledgeProposition

import scala.util.{Failure, Success, Try}

class AnyoneCanSpendProposition extends ProofOfKnowledgeProposition[Nothing] {
  override type M = AnyoneCanSpendProposition

  override def serializer: Serializer[AnyoneCanSpendProposition] = AnyoneCanSpendPropositionSerializer
}

object AnyoneCanSpendPropositionSerializer extends Serializer[AnyoneCanSpendProposition] {
  override def toBytes(obj: AnyoneCanSpendProposition): Array[Byte] = Array(-127: Byte)

  override def parseBytes(bytes: Array[Byte]): Try[AnyoneCanSpendProposition] = bytes match {
    case b if b sameElements Array(-127.toByte) => Success(new AnyoneCanSpendProposition)
    case _ => Failure(new Error("Incorrect proposition"))
  }
}

