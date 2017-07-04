package org.ergoplatform.modifiers.mempool

import com.google.common.primitives.{Bytes, Ints, Longs}
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction._
import org.ergoplatform.modifiers.mempool.proposition.{AnyoneCanSpendNoncedBox, AnyoneCanSpendProposition, AnyoneCanSpendPropositionSerializer}
import org.ergoplatform.settings.Constants
import scorex.core.serialization.Serializer
import scorex.core.transaction.BoxTransaction
import scorex.core.transaction.box.BoxUnlocker
import scorex.crypto.encode.Base58

import scala.util.Try

/**
  * Transaction without any lockers and unlockers
  */
case class AnyoneCanSpendTransaction(from: IndexedSeq[(AnyoneCanSpendProposition, Value)],
                                     to: IndexedSeq[(AnyoneCanSpendProposition, Nonce)],
                                     override val timestamp: Long) extends
  BoxTransaction[AnyoneCanSpendProposition, AnyoneCanSpendNoncedBox] with MempoolModifier {

  //TODO remove fee and timestamp from base class?
  override val fee: Value = from.map(_._2).sum - to.map(_._2).sum

  override type M = AnyoneCanSpendTransaction

  lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = from.map { case (prop, nonce) =>
    AnyoneCanSpendNoncedBox.idFromBox(prop, nonce)
  }

  override lazy val unlockers: Traversable[BoxUnlocker[AnyoneCanSpendProposition]] = Seq()

  lazy val hashNoNonces = Constants.hash(
    Bytes.concat(scorex.core.utils.concatFixLengthBytes(to.map(_._1.bytes)),
      scorex.core.utils.concatFixLengthBytes(unlockers.map(_.closedBoxId)),
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee))
  )

  override lazy val newBoxes: Traversable[AnyoneCanSpendNoncedBox] = to.zipWithIndex.map { case ((prop, value), idx) =>
    val nonce = nonceFromDigest(Constants.hash(prop.bytes ++ hashNoNonces ++ Ints.toByteArray(idx)))
    AnyoneCanSpendNoncedBox(prop, nonce, value)
  }

  override lazy val serializer = AnyoneCanSpendTransactionSerializer

  override lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    //    "newBoxes" -> newBoxes.map(b => Base58.encode(b.id).asJson).asJson,
    //    "boxesToRemove" -> boxIdsToOpen.map(id => Base58.encode(id).asJson).asJson,
    "from" -> from.map { s =>
      Map(
        "nonce" -> s._2.asJson
      ).asJson
    }.asJson,
    "to" -> to.map { s =>
      Map(
        "value" -> s._2.asJson
      ).asJson
    }.asJson,
    "fee" -> fee.asJson,
    "timestamp" -> timestamp.asJson
  ).asJson

  override def toString: String = s"AnyoneCanSpendTransaction(${json.noSpaces})"

  lazy val semanticValidity: Try[Unit] = Try {
    require(to.forall(_._2 >= 0))
    require(fee >= 0)
    require(timestamp >= 0)
  }

}

object AnyoneCanSpendTransaction {
  type Value = Long
  type Nonce = Long

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(8))
}

object AnyoneCanSpendTransactionSerializer extends Serializer[AnyoneCanSpendTransaction] {

  override def toBytes(m: AnyoneCanSpendTransaction): Array[Byte] = {
    Bytes.concat(
      Longs.toByteArray(m.timestamp),
      Ints.toByteArray(m.from.length),
      Ints.toByteArray(m.to.length),
      m.from.foldLeft(Array[Byte]())((a, b) => Bytes.concat(a, b._1.bytes, Longs.toByteArray(b._2))),
      m.to.foldLeft(Array[Byte]())((a, b) => Bytes.concat(a, b._1.bytes, Longs.toByteArray(b._2)))
    )

  }

  override def parseBytes(bytes: Array[Byte]): Try[AnyoneCanSpendTransaction] = Try {
    val timestamp = Longs.fromByteArray(bytes.slice(0, 8))
    val fromLength = Ints.fromByteArray(bytes.slice(8, 12))
    val toLength = Ints.fromByteArray(bytes.slice(12, 16))
    val elementLength = 8 + 1
    val s = 16
    val from = (0 until fromLength) map { i =>
      val b = bytes.slice(s + i * elementLength, s + i * elementLength + 1)
      val v = Longs.fromByteArray(bytes.slice(s + i * elementLength + 1, s + (i + 1) * elementLength))
      (AnyoneCanSpendPropositionSerializer.parseBytes(b).get, v)
    }
    val s2 = s + fromLength * elementLength
    val to = (0 until toLength) map { i =>
      val b = bytes.slice(s2 + i * elementLength, s2 + i * elementLength + 1)
      val v = Longs.fromByteArray(bytes.slice(s2 + i * elementLength + 1, s2 + (i + 1) * elementLength))
      (AnyoneCanSpendPropositionSerializer.parseBytes(b).get, v)
    }
    AnyoneCanSpendTransaction(from, to, timestamp)
  }
}