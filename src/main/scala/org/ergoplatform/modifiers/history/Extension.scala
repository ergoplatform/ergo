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
  * Extension section of Ergo block. Contains two key-value storages,
  * represented as Seq[(Array[Byte], Array[Byte])] with mandatory and optional fields.
  *
  * @param headerId        - id of corresponding header
  * @param mandatoryFields - fields that are known to all nodes in the network and may be changed
  *                        via soft/hard forks only. These fields have 4 bytes key and at most `MandatoryFieldValueSize`
  *                        bytes value.
  * @param optionalFields  - random data miner may add to a block. This section contains at most `MaxOptionalFields`
  *                        elements with `OptionalFieldKeySize` byte key size and at most `OptionalFieldValueSize`
  *                        bytes value size.
  */
case class Extension(headerId: ModifierId,
                     mandatoryFields: Seq[(Array[Byte], Array[Byte])],
                     optionalFields: Seq[(Array[Byte], Array[Byte])],
                     override val sizeOpt: Option[Int] = None) extends BlockSection {
  override val modifierTypeId: ModifierTypeId = Extension.modifierTypeId

  override def digest: Digest32 = Extension.rootHash(mandatoryFields, optionalFields)

  override def toString: String = {
    s"Extension(${Algos.encode(headerId)}, " +
      s"${mandatoryFields.map(kv => s"${Algos.encode(kv._1)} -> ${Algos.encode(kv._2)}")})" +
      s"${optionalFields.map(kv => s"${Algos.encode(kv._1)} -> ${Algos.encode(kv._2)}")})"
  }

}

case class ExtensionCandidate(mandatoryFields: Seq[(Array[Byte], Array[Byte])],
                              optionalFields: Seq[(Array[Byte], Array[Byte])])

object Extension extends ApiCodecs {

  val MandatoryFieldKeySize: Int = 4

  val OptionalFieldKeySize: Int = 32

  val MaxMandatoryFieldValueSize: Int = 64

  val MaxOptionalFieldValueSize: Int = 64

  val MaxOptionalFields: Int = 2

  def apply(headerId: ModifierId): Extension = Extension(headerId, Seq(), Seq())

  def rootHash(e: Extension): Digest32 = rootHash(e.mandatoryFields, e.optionalFields)

  def rootHash(e: ExtensionCandidate): Digest32 = rootHash(e.mandatoryFields, e.optionalFields)

  def rootHash(mandatoryFields: Seq[(Array[Byte], Array[Byte])],
               optionalFields: Seq[(Array[Byte], Array[Byte])]): Digest32 = {
    val elements: Seq[Array[Byte]] = (mandatoryFields ++ optionalFields).map { f =>
      Bytes.concat(Array(f._1.length.toByte), f._1, f._2)
    }
    Algos.merkleTreeRoot(LeafData @@ elements)
  }

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (108: Byte)

  implicit val jsonEncoder: Encoder[Extension] = { e: Extension =>
    Map(
      "headerId" -> Algos.encode(e.headerId).asJson,
      "digest" -> Algos.encode(e.digest).asJson,
      "mandatoryFields" -> e.mandatoryFields.map(kv => Algos.encode(kv._1) -> Algos.encode(kv._2).asJson).asJson,
      "optionalFields" -> e.optionalFields.map(kv => Algos.encode(kv._1) -> Algos.encode(kv._2).asJson).asJson
    ).asJson
  }

  implicit val jsonDecoder: Decoder[Extension] = { c: HCursor =>
    for {
      headerId <- c.downField("headerId").as[ModifierId]
      mandatoryFields <- c.downField("mandatoryFields").as[List[(Array[Byte], Array[Byte])]]
      optionalFields <- c.downField("optionalFields").as[List[(Array[Byte], Array[Byte])]]
    } yield Extension(headerId, mandatoryFields, optionalFields)
  }
}

object ExtensionSerializer extends ScorexSerializer[Extension] {

  override def serialize(obj: Extension, w: Writer): Unit = {
    w.putBytes(idToBytes(obj.headerId))

    w.putUShort(obj.mandatoryFields.size)
    obj.mandatoryFields.foreach { case (key, value) =>
      w.putBytes(key)
      w.putUByte(value.length)
      w.putBytes(value)
    }

    w.putUShort(obj.optionalFields.size)
    obj.optionalFields.foreach { case (key, value) =>
      w.putBytes(key)
      w.putUByte(value.length)
      w.putBytes(value)
    }
  }

  override def parse(r: Reader): Extension = {
    val startPosition = r.position
    val headerId = bytesToId(r.getBytes(Constants.ModifierIdSize))
    val mandBytesSize = r.getUShort()
    val madatoryFields = (1 to mandBytesSize) map {_ =>
      val key = r.getBytes(Extension.MandatoryFieldKeySize)
      val size = r.getUShort()
      val value = r.getBytes(size)
      (key, value)
    }

    val optFieldsSize = r.getUShort()
    val optFields = (1 to optFieldsSize) map { _ =>
      val key = r.getBytes(Extension.OptionalFieldKeySize)
      val size = r.getUShort()
      val value = r.getBytes(size)
      (key, value)
    }
    Extension(headerId, madatoryFields, optFields, Some(r.position - startPosition))
  }
}
