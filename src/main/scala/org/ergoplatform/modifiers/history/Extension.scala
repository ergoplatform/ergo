package org.ergoplatform.modifiers.history

import com.google.common.primitives.Bytes
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.ModifierTypeId
import scorex.core.serialization.Serializer
import scorex.crypto.authds.LeafData
import scorex.crypto.hash.Digest32
import scorex.util.{ModifierId, bytesToId, idToBytes}

import scala.annotation.tailrec
import scala.util.Try

/**
  * Extension section of Ergo block. Contains key-value storage
  * represented as Seq[(Array[Byte], Array[Byte])] with mandatory and optional fields.
  *
  * @param headerId - id of corresponding header
  * @param fields - fields as a sequence of key -> value records. A key is 2-bytes long, value is 64 bytes max.
  */
case class Extension(headerId: ModifierId,
                     fields: Seq[(Array[Byte], Array[Byte])],
                     override val sizeOpt: Option[Int] = None) extends BlockSection {
  override val modifierTypeId: ModifierTypeId = Extension.modifierTypeId

  override def digest: Digest32 = Extension.rootHash(fields)

  override type M = Extension

  override def serializer: Serializer[Extension] = ExtensionSerializer

  override def toString: String = {
    s"Extension(id: $id, headerId: ${Algos.encode(headerId)}, " +
      s"fields: ${fields.map(kv => s"${Algos.encode(kv._1)} -> ${Algos.encode(kv._2)}")}) "
  }

}

case class ExtensionCandidate(fields: Seq[(Array[Byte], Array[Byte])]) {
  def toExtension(headerId: ModifierId): Extension = Extension(headerId, fields)
}

object Extension extends ApiCodecs {

  val FieldKeySize: Int = 2

  val FieldValueMaxSize: Int = 64

  def apply(header: Header): Extension = Extension(header.id, Seq())

  def rootHash(e: Extension): Digest32 = rootHash(e.fields)

  def rootHash(e: ExtensionCandidate): Digest32 = rootHash(e.fields)

  def rootHash(fields: Seq[(Array[Byte], Array[Byte])]): Digest32 = {
    val elements: Seq[Array[Byte]] = fields.map { f =>
      Bytes.concat(Array(f._1.length.toByte), f._1, f._2)
    }
    Algos.merkleTreeRoot(LeafData @@ elements)
  }

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (108: Byte)

  implicit val jsonEncoder: Encoder[Extension] = { e: Extension =>
    Map(
      "headerId" -> Algos.encode(e.headerId).asJson,
      "digest" -> Algos.encode(e.digest).asJson,
      "fields" -> e.fields.map(kv => Algos.encode(kv._1) -> Algos.encode(kv._2).asJson).asJson
    ).asJson
  }

  implicit val jsonDecoder: Decoder[Extension] = { c: HCursor =>
    for {
      headerId <- c.downField("headerId").as[ModifierId]
      fields <- c.downField("fields").as[List[(Array[Byte], Array[Byte])]]
    } yield Extension(headerId, fields)
  }
}

object ExtensionSerializer extends Serializer[Extension] {
  val Delimiter: Array[Byte] = Array(0: Byte, Byte.MinValue)

  override def toBytes(obj: Extension): Array[Byte] = {
    val mandBytes = scorex.core.utils.concatBytes(obj.fields.map(f =>
      Bytes.concat(f._1, Array(f._2.length.toByte), f._2)))

    Bytes.concat(idToBytes(obj.headerId), mandBytes)
  }

  override def parseBytes(bytes: Array[Byte]): Try[Extension] = Try {
    val totalLength = bytes.length

    require(totalLength < Constants.extensionMaxSize)

    @tailrec
    def parseFields(pos: Int,
                     acc: Seq[(Array[Byte], Array[Byte])]): Seq[(Array[Byte], Array[Byte])] = {
      val keySize = Extension.FieldKeySize
      if (pos == totalLength) {
        // deserialization complete
        acc.reverse
      } else {
        val key = bytes.slice(pos, pos + keySize)
        val length: Byte = bytes(pos + keySize)
        require(length >= 0, s"value size should not be negative, length = $length, pos = $pos")
        require(length <= Extension.FieldValueMaxSize, "value size should be <= " + Extension.FieldValueMaxSize)
        val value = bytes.slice(pos + keySize + 1, pos + keySize + 1 + length)
        parseFields(pos + keySize + 1 + length, (key, value) +: acc)
      }
    }

    val headerId = bytesToId(bytes.take(32))
    val mandatory = parseFields(32, Seq())
    Extension(headerId, mandatory, Some(bytes.length))
  }

}
