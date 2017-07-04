package org.ergoplatform.modifiers.mempool.proposition

import com.google.common.primitives.Longs
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.settings.Constants
import scorex.core.serialization.{JsonSerializable, Serializer}
import scorex.core.transaction.box.Box
import scorex.crypto.encode.Base58

import scala.util.Try

case class AnyoneCanSpendNoncedBox(override val proposition: AnyoneCanSpendProposition,
                                   nonce: Long,
                                   override val value: Long
                                  ) extends Box[AnyoneCanSpendProposition] with JsonSerializable {

  override lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "nonce" -> nonce.asJson,
    "value" -> value.asJson
  ).asJson


  override val id: Array[Byte] = Constants.hash(Longs.toByteArray(nonce))

  override type M = AnyoneCanSpendNoncedBox

  override lazy val serializer: Serializer[AnyoneCanSpendNoncedBox] = AnyoneCanSpendNoncedBoxSerializer
}

object AnyoneCanSpendNoncedBox {

  def idFromBox(prop: AnyoneCanSpendProposition, nonce: Long): Array[Byte] =
    Constants.hash(prop.serializer.toBytes(prop) ++ Longs.toByteArray(nonce))

}

object AnyoneCanSpendNoncedBoxSerializer extends Serializer[AnyoneCanSpendNoncedBox] {

  override def toBytes(obj: AnyoneCanSpendNoncedBox): Array[Byte] =
    obj.proposition.serializer.toBytes(obj.proposition) ++
      Longs.toByteArray(obj.nonce) ++
      Longs.toByteArray(obj.value)


  override def parseBytes(bytes: Array[Byte]): Try[AnyoneCanSpendNoncedBox] = Try {
    val proposition = AnyoneCanSpendPropositionSerializer.parseBytes(bytes.take(1)).get
    val nonce = Longs.fromByteArray(bytes.slice(1, 9))
    val value = Longs.fromByteArray(bytes.slice(9, 17))
    AnyoneCanSpendNoncedBox(proposition, nonce, value)
  }
}

