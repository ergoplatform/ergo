package org.ergoplatform.modifiers

import com.google.common.primitives.{Bytes, Ints, Longs}
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.transaction.proposition.{AnyoneCanSpendNoncedBox, AnyoneCanSpendProposition}
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.serialization.Serializer
import scorex.core.transaction.BoxTransaction
import scorex.core.transaction.box.BoxUnlocker
import scorex.crypto.encode.Base58
import AnyoneCanSpendTransaction._

import scala.util.Try

/**
  * Transaction witohut any lockers and unlockers
  */
case class AnyoneCanSpendTransaction(from: IndexedSeq[(AnyoneCanSpendProposition, Value)],
                                     to: IndexedSeq[(AnyoneCanSpendProposition, Nonce)],
                                     override val timestamp: Long) extends
  BoxTransaction[AnyoneCanSpendProposition, AnyoneCanSpendNoncedBox] {

  //TODO remove fee and timestamp from base class?
  override val fee: Value = from.map(_._2).sum - to.map(_._2).sum

  override type M = AnyoneCanSpendTransaction

  lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = from.map { case (prop, nonce) =>
    AnyoneCanSpendNoncedBox.idFromBox(prop, nonce)
  }

  override lazy val unlockers: Traversable[BoxUnlocker[Nothing]] = Seq()

  lazy val hashNoNonces = FastCryptographicHash(
    Bytes.concat(scorex.core.utils.concatFixLengthBytes(to.map(_._1.bytes)),
      scorex.core.utils.concatFixLengthBytes(unlockers.map(_.closedBoxId)),
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee))
  )

  override lazy val newBoxes: Traversable[AnyoneCanSpendNoncedBox] = to.zipWithIndex.map { case ((prop, value), idx) =>
    val nonce = nonceFromDigest(FastCryptographicHash(prop.bytes ++ hashNoNonces ++ Ints.toByteArray(idx)))
    AnyoneCanSpendNoncedBox(prop, nonce, value)
  }

  override lazy val serializer = AnyoneCanSpendTransactionSerializer

  override lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "newBoxes" -> newBoxes.map(b => Base58.encode(b.id).asJson).asJson,
    "boxesToRemove" -> boxIdsToOpen.map(id => Base58.encode(id).asJson).asJson,
    "from" -> from.map { s =>
      Map(
        "proposition" -> Base58.encode(s._1.bytes).asJson,
        "nonce" -> s._2.asJson
      ).asJson
    }.asJson,
    "to" -> to.map { s =>
      Map(
        "proposition" -> Base58.encode(s._1.bytes).asJson,
        "value" -> s._2.asJson
      ).asJson
    }.asJson,
    "fee" -> fee.asJson,
    "timestamp" -> timestamp.asJson
  ).asJson

  override def toString: String = s"SimpleBoxTransaction(${json.noSpaces})"

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

  override def toBytes(m: AnyoneCanSpendTransaction): Array[Byte] = ???

  override def parseBytes(bytes: Array[Byte]): Try[AnyoneCanSpendTransaction] = ???
}