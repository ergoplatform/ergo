package org.ergoplatform.modifiers.history

import com.google.common.primitives.{Bytes, Chars}
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
                     interlinks: Seq[ModifierId],
                     fields: Seq[(Array[Byte], Array[Byte])],
                     override val sizeOpt: Option[Int] = None) extends BlockSection {
  override val modifierTypeId: ModifierTypeId = Extension.modifierTypeId

  override def digest: Digest32 = Extension.rootHash(interlinks, fields)

  override type M = Extension

  override def serializer: Serializer[Extension] = ExtensionSerializer

  override def toString: String = {
    s"Extension(id: $id, headerId: ${Algos.encode(headerId)}, interlinks: ${interlinks.map(Algos.encode)}" +
      s"fields: ${fields.map(kv => s"${Algos.encode(kv._1)} -> ${Algos.encode(kv._2)}")}) "
  }

}

case class ExtensionCandidate(interlinks: Seq[ModifierId], fields: Seq[(Array[Byte], Array[Byte])]) {
  def toExtension(headerId: ModifierId): Extension = Extension(headerId, interlinks, fields)
}

object Extension extends ApiCodecs {

  val FieldKeySize: Int = 2

  val FieldValueMaxSize: Int = 64

  def apply(header: Header): Extension = Extension(header.id, Seq.empty, Seq.empty)

  def rootHash(e: Extension): Digest32 = rootHash(e.interlinks, e.fields)

  def rootHash(e: ExtensionCandidate): Digest32 = rootHash(e.interlinks, e.fields)

  def rootHash(interlinks: Seq[ModifierId], fields: Seq[(Array[Byte], Array[Byte])]): Digest32 = {
    val elements: Seq[Array[Byte]] = fields
      .map { case (k, v) =>
        Bytes.concat(Array(k.length.toByte), k, v)
      } ++
      interlinks.map(idToBytes)
    Algos.merkleTreeRoot(LeafData @@ elements)
  }

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (108: Byte)

  implicit val jsonEncoder: Encoder[Extension] = { e: Extension =>
    Map(
      "headerId" -> Algos.encode(e.headerId).asJson,
      "digest" -> Algos.encode(e.digest).asJson,
      "interlinks" -> e.interlinks.map(Algos.encode(_).asJson).asJson,
      "fields" -> e.fields.map(kv => Algos.encode(kv._1) -> Algos.encode(kv._2).asJson).asJson
    ).asJson
  }

  implicit val jsonDecoder: Decoder[Extension] = { c: HCursor =>
    for {
      headerId <- c.downField("headerId").as[ModifierId]
      interlinks <- c.downField("interlinks").as[List[ModifierId]]
      fields <- c.downField("fields").as[List[(Array[Byte], Array[Byte])]]
    } yield Extension(headerId, interlinks, fields)
  }

}

object ExtensionSerializer extends Serializer[Extension] {

  val Delimiter: Array[Byte] = Array(0: Byte, Byte.MinValue)

  override def toBytes(obj: Extension): Array[Byte] = {

    val interlinkBytes = packInterlinks(obj.interlinks.toList, Array[Byte]())
    val interlinkBytesSize = Chars.toByteArray(interlinkBytes.length.toChar)
    val fieldsBytes = scorex.core.utils.concatBytes {
      obj.fields.map { case (k, v) =>
        Bytes.concat(k, Array(v.length.toByte), v)
      }
    }

    Bytes.concat(idToBytes(obj.headerId), interlinkBytesSize, interlinkBytes, fieldsBytes)
  }

  override def parseBytes(bytes: Array[Byte]): Try[Extension] = Try {
    val totalLength = bytes.length

    require(totalLength < Constants.ExtensionMaxSize)

    @tailrec
    def parseFields(pos: Int, acc: Seq[(Array[Byte], Array[Byte])] = Seq.empty): Seq[(Array[Byte], Array[Byte])] = {
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

    @tailrec
    def parseInterlinks(startIdx: Int, endIdx: Int, acc: Seq[ModifierId] = Seq.empty): Seq[ModifierId] = {
      if (endIdx > startIdx) {
        val repeatN: Int = 0xff & bytes(startIdx)
        require(repeatN != 0)
        val link: ModifierId = bytesToId(bytes.slice(startIdx + 1, startIdx + 33))
        val links: Seq[ModifierId] = Array.fill(repeatN)(link)
        parseInterlinks(startIdx + 33, endIdx, acc ++ links)
      } else {
        acc
      }
    }

    val headerId = bytesToId(bytes.take(32))
    val interlinksSize = Chars.fromByteArray(bytes.slice(32, 34))
    val interlinks = parseInterlinks(34, 34 + interlinksSize)
    val mandatory = parseFields(34 + interlinksSize)
    Extension(headerId, interlinks, mandatory, Some(bytes.length))
  }

  @tailrec
  private def packInterlinks(links: List[ModifierId], acc: Array[Byte]): Array[Byte] = links match {
    case headLink :: _ =>
      val repeatingInt = links.count(_ == headLink)
      val repeating: Byte = repeatingInt.toByte
      packInterlinks(links.drop(repeatingInt), Bytes.concat(acc, Array(repeating), idToBytes(headLink)))
    case Nil =>
      acc
  }

}
