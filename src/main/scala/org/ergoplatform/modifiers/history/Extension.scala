package org.ergoplatform.modifiers.history

import com.google.common.primitives.Bytes
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.ModifierTypeId
import scorex.core.serialization.ScorexSerializer
import scorex.crypto.authds.LeafData
import scorex.crypto.hash.Digest32
import scorex.util.serialization.{Reader, Writer}
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

  override def serializer: ScorexSerializer[Extension] = ExtensionSerializer

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

  //predefined key prefixes
  val SystemParametersPrefix: Byte = 0x00
  val InterlinksVectorPrefix: Byte = 0x01

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

object ExtensionSerializer extends ScorexSerializer[Extension] {

  override def serialize(obj: Extension, w: Writer): Unit = {
    w.putBytes(idToBytes(obj.headerId))
    w.putUShort(obj.fields.size)
    obj.fields.foreach { case (key, value) =>
      w.putBytes(key)
      w.putUByte(value.length)
      w.putBytes(value)
    }
  }

  override def parse(r: Reader): Extension = {
    val startPosition = r.position
    val headerId = bytesToId(r.getBytes(Constants.ModifierIdSize))
    val fieldsSize = r.getUShort()
    val fieldsView = (1 to fieldsSize).view.map {_ =>
      val key = r.getBytes(Extension.FieldKeySize)
      val length = r.getUByte()
      require(length <= Extension.FieldValueMaxSize, "value size should be <= " + Extension.FieldValueMaxSize)
      val value = r.getBytes(length)
      (key, value)
    }
    val fields = fieldsView.takeWhile(r.position - startPosition < Constants.ExtensionMaxSize).toSeq
    require(r.position - startPosition < Constants.ExtensionMaxSize)
    Extension(headerId, fields, Some(r.position - startPosition))
  }

}