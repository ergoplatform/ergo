package org.ergoplatform.modifiers.mempool.proposition

import com.google.common.primitives.Longs
import io.circe.Encoder
import io.circe.syntax._
import org.ergoplatform.settings.Algos
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.Box
import scorex.crypto.authds._
import scorex.crypto.encode.Base58

import scala.util.Try

case class AnyoneCanSpendNoncedBox(nonce: Long, override val value: Long)
  extends Box[AnyoneCanSpendProposition.type] {

  override val proposition = AnyoneCanSpendProposition

  override val id: ADKey = ADKey @@ Algos.hash(Longs.toByteArray(nonce))

  override type M = AnyoneCanSpendNoncedBox

  override lazy val serializer: Serializer[AnyoneCanSpendNoncedBox] = AnyoneCanSpendNoncedBoxSerializer

  override lazy val bytes: ADValue = ADValue @@ serializer.toBytes(this)
}

object AnyoneCanSpendNoncedBox {
  def idFromBox(nonce: Long): Array[Byte] =
    Algos.hash(Longs.toByteArray(nonce))

  implicit val jsonEncoder: Encoder[AnyoneCanSpendNoncedBox] = (b: AnyoneCanSpendNoncedBox) =>
    Map(
      "id" -> Base58.encode(b.id).asJson,
      "nonce" -> b.nonce.asJson,
      "value" -> b.value.asJson
    ).asJson

}

object AnyoneCanSpendNoncedBoxSerializer extends Serializer[AnyoneCanSpendNoncedBox] {
  val Length: Int = 8 + 8

  override def toBytes(obj: AnyoneCanSpendNoncedBox): Array[Byte] =
    Longs.toByteArray(obj.nonce) ++ Longs.toByteArray(obj.value)

  override def parseBytes(bytes: Array[Byte]): Try[AnyoneCanSpendNoncedBox] = Try {
    val nonce = Longs.fromByteArray(bytes.slice(0, 8))
    val value = Longs.fromByteArray(bytes.slice(8, 16))
    AnyoneCanSpendNoncedBox(nonce, value)
  }
}
