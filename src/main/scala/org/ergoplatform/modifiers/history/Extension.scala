package org.ergoplatform.modifiers.history

import com.google.common.primitives.{Bytes, Shorts}
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.settings.Algos
import scorex.core.serialization.Serializer
import scorex.core.{ModifierId, ModifierTypeId}
import scorex.crypto.authds.LeafData
import scorex.crypto.encode.Base16
import scorex.crypto.hash.Digest32

import scala.annotation.tailrec
import scala.util.Try

/**
  * Extension section of Ergo block. Contains two maps with mandatory and optional fields, keys are Base16 encoded
  *
  * @param headerId        - id of corresponding header
  * @param mandatoryFields - fields, that are known to all node in the network and may be changed
  *                        via soft/hard forks only. This fields have 4 bytes key and at most 64 bytes value.
  * @param optionalFields  - random data miner may add to a block. This section contains at most 8
  *                        elements with 32 byte key size and at most 1024 bytes value size.
  */
case class Extension(headerId: ModifierId,
                     mandatoryFields: Seq[(Array[Byte], Array[Byte])],
                     optionalFields: Seq[(Array[Byte], Array[Byte])]) extends BlockSection {
  override val modifierTypeId: ModifierTypeId = Extension.modifierTypeId

  override def digest: Digest32 = {
    val elements: Seq[Array[Byte]] = (mandatoryFields ++ optionalFields).map { f =>
      Bytes.concat(Array(f._1.length.toByte), f._1, f._2)
    }
    Algos.merkleTreeRoot(LeafData @@ elements)
  }

  override type M = Extension

  override def serializer: Serializer[Extension] = ExtensionSerializer

  override def toString: String = {
    s"Extension(${Algos.encode(headerId)}, " +
      s"${mandatoryFields.map(kv => s"${Algos.encode(kv._1)} -> ${Algos.encode(kv._2)}")})" +
      s"${optionalFields.map(kv => s"${Algos.encode(kv._1)} -> ${Algos.encode(kv._2)}")})"
  }

}

object Extension {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (108: Byte)

}

object ExtensionSerializer extends Serializer[Extension] {
  val Delimeter: Array[Byte] = Array.fill(4)(0: Byte)

  override def toBytes(obj: Extension): Array[Byte] = {
    val mandBytes = scorex.core.utils.concatBytes(obj.mandatoryFields.map(f =>
      Bytes.concat(f._1, Shorts.toByteArray(f._2.length.toShort), f._2)))
    val optBytes = scorex.core.utils.concatBytes(obj.optionalFields.map(f =>
      Bytes.concat(f._1, Shorts.toByteArray(f._2.length.toShort), f._2)))
    Bytes.concat(obj.headerId, mandBytes, Delimeter, optBytes)
  }

  override def parseBytes(bytes: Array[Byte]): Try[Extension] = Try {
    val totalLength = bytes.length

    @tailrec
    def parseSection(pos: Int,
                     keySize: Int,
                     acc: Seq[(Array[Byte], Array[Byte])]): (Seq[(Array[Byte], Array[Byte])], Int) = {
      val key = bytes.slice(pos, pos + keySize)
      if (!(key sameElements Delimeter) && pos < totalLength) {
        val length = Shorts.fromByteArray(bytes.slice(pos + keySize, pos + keySize + 2))
        val value = bytes.slice(pos + keySize + 2, pos + keySize + 2 + length)
        parseSection(pos + keySize + 2 + length, keySize, acc :+ (key, value))
      } else {
        (acc, pos + keySize)
      }
    }

    val headerId = ModifierId @@ bytes.take(32)
    val (mandatory, newPos) = parseSection(32, 4, Seq())
    val (optional, _) = parseSection(newPos, 32, Seq())
    Extension(headerId, mandatory, optional)
  }
}