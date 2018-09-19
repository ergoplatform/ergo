package org.ergoplatform.modifiers.history

import com.google.common.primitives.Bytes
import io.circe.Encoder
import io.circe.syntax._
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.settings.Algos
import scorex.core.ModifierTypeId
import scorex.core.serialization.Serializer
import scorex.crypto.authds.LeafData
import scorex.crypto.hash.Digest32
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

  override type M = Extension

  override def serializer: Serializer[Extension] = ExtensionSerializer

  override def toString: String = {
    s"Extension(${Algos.encode(headerId)}, " +
      s"${mandatoryFields.map(kv => s"${Algos.encode(kv._1)} -> ${Algos.encode(kv._2)}")})" +
      s"${optionalFields.map(kv => s"${Algos.encode(kv._1)} -> ${Algos.encode(kv._2)}")})"
  }

}

case class ExtensionCandidate(mandatoryFields: Seq[(Array[Byte], Array[Byte])],
                              optionalFields: Seq[(Array[Byte], Array[Byte])])

object Extension {

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

  implicit val jsonEncoder: Encoder[Extension] = (e: Extension) =>
    Map(
      "headerId" -> Algos.encode(e.headerId).asJson,
      "digest" -> Algos.encode(e.digest).asJson,
      "mandatoryFields" -> e.mandatoryFields.map(kv => Algos.encode(kv._1) -> Algos.encode(kv._2).asJson).asJson,
      "optionalFields" -> e.optionalFields.map(kv => Algos.encode(kv._1) -> Algos.encode(kv._2).asJson).asJson
    ).asJson

}

object ExtensionSerializer extends Serializer[Extension] {
  val Delimiter: Array[Byte] = Array.fill(4)(0: Byte)

  override def toBytes(obj: Extension): Array[Byte] = {
    val mandBytes = scorex.core.utils.concatBytes(obj.mandatoryFields.map(f =>
      Bytes.concat(f._1, Array(f._2.length.toByte), f._2)))
    val optBytes = scorex.core.utils.concatBytes(obj.optionalFields.map(f =>
      Bytes.concat(f._1, Array(f._2.length.toByte), f._2)))
    if (optBytes.nonEmpty) {
      Bytes.concat(idToBytes(obj.headerId), mandBytes, Delimiter, optBytes)
    } else {
      Bytes.concat(idToBytes(obj.headerId), mandBytes)
    }
  }

  override def parseBytes(bytes: Array[Byte]): Try[Extension] = Try {
    val totalLength = bytes.length
    // todo check bytes length immediately after voting implementation to prevent DoS

    @tailrec
    def parseSection(pos: Int,
                     keySize: Int,
                     acc: Seq[(Array[Byte], Array[Byte])]): (Seq[(Array[Byte], Array[Byte])], Int) = {
      if (pos == totalLength) {
        // deserialization complete
        (acc.reverse, pos)
      } else {
        val key = bytes.slice(pos, pos + keySize)
        if (keySize == Extension.MandatoryFieldKeySize && java.util.Arrays.equals(key, Delimiter)) {
          // mandatory fields deserialization complete
          (acc.reverse, pos + keySize)
        } else {
          val length: Byte = bytes(pos + keySize)
          require(length >= 0, "value size should not be negative")
          val value = bytes.slice(pos + keySize + 1, pos + keySize + 1 + length)
          parseSection(pos + keySize + 1 + length, keySize, (key, value) +: acc)
        }
      }
    }

    val headerId = bytesToId(bytes.take(32))
    val (mandatory, newPos) = parseSection(32, Extension.MandatoryFieldKeySize, Seq())
    val (optional, _) = parseSection(newPos, Extension.OptionalFieldKeySize, Seq())
    Extension(headerId, mandatory, optional, Some(bytes.length))
  }
}
