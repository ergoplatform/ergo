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
  * represented as Seq[(Array[Byte], Array[Byte])] with mandatory and optional fields.
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
    s"Extension(id: $id, headerId: ${Algos.encode(headerId)}, " +
      s"fields: ${fields.map(kv => s"${Algos.encode(kv._1)} -> ${Algos.encode(kv._2)}")}) "
  }

}

object Extension extends ApiCodecs {

  val FieldKeySize: Int = 2
  val FieldValueMaxSize: Int = 64

  //predefined key prefixes
  val SystemParametersPrefix: Byte = 0x00
  val InterlinksVectorPrefix: Byte = 0x01
  val ValidationRulesPrefix: Byte = 0x02

  def kvToLeaf(kv: (Array[Byte], Array[Byte])): Array[Byte] =
    Bytes.concat(Array(kv._1.length.toByte), kv._1, kv._2)

  def merkleTree(fields: Seq[(Array[Byte], Array[Byte])]): MerkleTree[Digest32] = {
    Algos.merkleTree(LeafData @@ fields.map(kvToLeaf))
  }

  val modifierTypeId: NetworkObjectTypeId.Value = ExtensionTypeId.value

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

