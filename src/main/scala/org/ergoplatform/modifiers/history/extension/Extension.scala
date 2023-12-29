package org.ergoplatform.modifiers.history.extension

import com.google.common.primitives.Bytes
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.modifiers.{ExtensionTypeId, NetworkObjectTypeId, NonHeaderBlockSection}
import org.ergoplatform.settings.Algos
import scorex.core.serialization.ErgoSerializer
import scorex.crypto.authds.LeafData
import scorex.crypto.authds.merkle.MerkleTree
import scorex.crypto.hash.Digest32
import scorex.util.ModifierId

/**
  * Extension section of Ergo block. Contains key-value storage
  * represented as Seq[(Array[Byte], Array[Byte])] with mandatory (consensus-critical) and optional fields.
  *
  * @param headerId - id of corresponding header
  * @param fields   - fields as a sequence of key -> value records. A key is 2-bytes long, value is 64 bytes max.
  */
case class Extension(headerId: ModifierId,
                     override val fields: Seq[(Array[Byte], Array[Byte])],
                     override val sizeOpt: Option[Int] = None)
  extends ExtensionCandidate(fields) with NonHeaderBlockSection {

  override val modifierTypeId: NetworkObjectTypeId.Value = Extension.modifierTypeId

  override type M = Extension

  override def serializer: ErgoSerializer[Extension] = ExtensionSerializer

  override def toString: String = {
    val fieldsEncoded = fields.map(kv => s"${Algos.encode(kv._1)} -> ${Algos.encode(kv._2)}")
    s"Extension(id: $id, headerId: $headerId, fields: $fieldsEncoded)"
  }

}

object Extension extends ApiCodecs {

  val FieldKeySize: Int = 2
  val FieldValueMaxSize: Int = 64

  /**
    * Predefined key spaces. Defined by first byte of a key, then there could be up to 256 keys per key space, as
    * a key is of 2 bytes always.
    */

  /**
    * Every voting epoch length blocks (so every 1024 blocks for the Ergo mainnet, current blockchain parameters are
    * written into extension section, to support light clients which do not processing all the blocks but need to know
    * current blockchain parameters (i.e. without processing all the historical blocks) to process new blocks.
    *
    * All the parameters are written into key space defined by the value below.
    */
  val SystemParametersPrefix: Byte = 0x00

  /**
    * Every block interlinks vector needed in order to build and validate NiPoPoWs is written into extension section,
    *
    * All the interlinks are written into a single key space defined by the value below.
    */
  val InterlinksVectorPrefix: Byte = 0x01

  /**
    * It is possible to switch some soft-forkable rules and introduce new ones via 90+% mines voting. Changes in rules
    * against the genesis block are to be written into a single key space defined by the value below.
    */
  val ValidationRulesPrefix: Byte = 0x02

  /**
    * Prefix for keys related to sub-blocks related data.
    */
  val SubBlocksDataPrefix: Byte = 0x03

  /**
    * Prefix for keys related to sidechains data.
    */
  val SidechainsDataPrefix: Byte = 0x04

  /**
    * Id a type of network object encoding extension
    */
  val modifierTypeId: NetworkObjectTypeId.Value = ExtensionTypeId.value

  /**
    * @return key-value pair `kv` encoded as Merkle tree leaf (byte array)
    */
  def kvToLeaf(kv: (Array[Byte], Array[Byte])): Array[Byte] =
    Bytes.concat(Array(kv._1.length.toByte), kv._1, kv._2)

  /**
    * @return Merkle tree built on top of key-value pairs
    */
  def merkleTree(fields: Seq[(Array[Byte], Array[Byte])]): MerkleTree[Digest32] = {
    Algos.merkleTree(LeafData @@ fields.map(kvToLeaf))
  }

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

