package org.ergoplatform.modifiers.mempool

import com.google.common.primitives.{Bytes, Ints, Longs}
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction._
import org.ergoplatform.modifiers.mempool.proposition.{AnyoneCanSpendNoncedBox, AnyoneCanSpendProposition}
import org.ergoplatform.settings.Algos
import scorex.core.serialization.Serializer
import scorex.core.transaction.Transaction
import scorex.crypto.authds.ADKey
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Digest32

import scala.util.Try

/**
  * Transaction without any lockers and unlockers
  */
case class AnyoneCanSpendTransaction(from: IndexedSeq[Nonce], to: IndexedSeq[Value]) extends
  Transaction[AnyoneCanSpendProposition.type] with MempoolModifier {

  override type M = AnyoneCanSpendTransaction

  lazy val boxIdsToOpen: IndexedSeq[ADKey] = from.map { nonce =>
    ADKey @@ AnyoneCanSpendNoncedBox.idFromBox(nonce)
  }

  lazy val hashNoNonces: Digest32 = Algos.hash(
    Bytes.concat(scorex.core.utils.concatFixLengthBytes(boxIdsToOpen),
      scorex.core.utils.concatFixLengthBytes(to.map(v => Longs.toByteArray(v))))
  )

  lazy val newBoxes: Traversable[AnyoneCanSpendNoncedBox] = to.zipWithIndex.map { case (value, idx) =>
    val nonce = nonceFromDigest(Algos.hash(hashNoNonces ++ Ints.toByteArray(idx)))
    AnyoneCanSpendNoncedBox(nonce, value)
  }

  override lazy val serializer: Serializer[AnyoneCanSpendTransaction] = AnyoneCanSpendTransactionSerializer

  override lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "inputs" -> (from zip boxIdsToOpen).map { case (nonce, id) =>
      Map(
        "id" -> Algos.encode(id).asJson,
        "nonce" -> nonce.asJson,
        "signature" -> "".asJson
      ).asJson
    }.asJson,
    "outputs" -> to.map { s =>
      Map(
        "script" -> "".asJson,
        "value" -> s.asJson
      ).asJson
    }.asJson
  ).asJson

  override def toString: String = s"AnyoneCanSpendTransaction(${json.noSpaces})"

  lazy val semanticValidity: Try[Unit] = Try {
    require(to.forall(_ >= 0))
    //todo: check new boxes
  }

  //no signatures in the current transaction definition
  override val messageToSign: Array[Byte] = hashNoNonces
}

object AnyoneCanSpendTransaction {
  type Value = Long
  type Nonce = Long

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(8))
}

object AnyoneCanSpendTransactionSerializer extends Serializer[AnyoneCanSpendTransaction] {

  override def toBytes(m: AnyoneCanSpendTransaction): Array[Byte] = {
    Bytes.concat(
      Ints.toByteArray(m.from.length),
      Ints.toByteArray(m.to.length),
      m.from.foldLeft(Array[Byte]())((a, b) => Bytes.concat(a, Longs.toByteArray(b))),
      m.to.foldLeft(Array[Byte]())((a, b) => Bytes.concat(a, Longs.toByteArray(b)))
    ).ensuring(_.length % 8 == 0)
  }

  override def parseBytes(bytes: Array[Byte]): Try[AnyoneCanSpendTransaction] = Try {
    require(bytes.length % 8 == 0)

    val fromLength = Ints.fromByteArray(bytes.slice(0, 4))
    val toLength = Ints.fromByteArray(bytes.slice(4, 8))
    val elementLength = 8
    val s = 8

    val from = (0 until fromLength) map { i =>
      Longs.fromByteArray(bytes.slice(s + i * elementLength, s + (i + 1) * elementLength))
    }

    val s2 = s + fromLength * elementLength
    val to = (0 until toLength) map { i =>
      Longs.fromByteArray(bytes.slice(s2 + i * elementLength, s2 + (i + 1) * elementLength))
    }

    AnyoneCanSpendTransaction(from, to)
  }
}