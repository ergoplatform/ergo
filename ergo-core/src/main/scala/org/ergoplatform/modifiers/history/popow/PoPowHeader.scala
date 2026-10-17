package org.ergoplatform.modifiers.history.popow

import cats.syntax.either._
import cats.Traverse
import cats.implicits.{catsStdInstancesForEither, catsStdInstancesForList}
import io.circe.{Decoder, Encoder, Json}
import org.ergoplatform.core.BytesSerializable
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.header.{Header, HeaderSerializer}
import org.ergoplatform.settings.Algos
import org.ergoplatform.settings.Algos.HF
import org.ergoplatform.serialization.ErgoSerializer
import scorex.crypto.authds.{LeafData, Side}
import scorex.crypto.authds.merkle.{BatchMerkleProof, Leaf}
import scorex.crypto.authds.merkle.serialization.BatchMerkleProofSerializer
import scorex.crypto.hash.Digest32
import scorex.util.Extensions._
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, bytesToId, idToBytes}

import java.nio.ByteBuffer
import scala.util.Try

/**
  * Block header along with unpacked interlinks
  *
  * Interlinks are stored in reverse order: first element is always genesis header, then level of lowest target met etc
  *
  * Not used in the consensus protocol
  *
  */
case class PoPowHeader(header: Header,
                       interlinks: Seq[ModifierId],
                       interlinksProof: BatchMerkleProof[Digest32]) extends BytesSerializable {

  override type M = PoPowHeader

  override def serializer: ErgoSerializer[M] = PoPowHeaderSerializer

  def id: ModifierId = header.id

  def height: Int = header.height

  def checkInterlinksProof(): Boolean = {
    val proofIsEmpty = interlinksProof.indices.isEmpty && interlinksProof.proofs.isEmpty
    if (header.isGenesis) {
      interlinks.isEmpty && proofIsEmpty
    } else if (!PoPowHeader.hasCanonicalInterlinkRuns(interlinks)) {
      false
    } else {
      PoPowHeader.checkInterlinksProof(interlinks, interlinksProof, header.extensionRoot)
    }
  }
}

object PoPowHeader {

  import io.circe.syntax._

  implicit val hf: HF = Algos.hash

  private[popow] def hasCanonicalInterlinkRuns(interlinks: Seq[ModifierId]): Boolean = {
    interlinks.headOption.exists { first =>
      var current = first
      var runLength = 1
      var closedRuns = Set.empty[ModifierId]

      interlinks.iterator.zipWithIndex.drop(1).forall { case (interlink, position) =>
        if (interlink == current) {
          if (runLength == 255) {
            false
          } else {
            runLength += 1
            true
          }
        } else if (position > 255) {
          false
        } else {
          closedRuns += current
          if (closedRuns.contains(interlink)) {
            false
          } else {
            current = interlink
            runLength = 1
            true
          }
        }
      }
    }
  }

  /**
    * Validates the exact packed interlink leaves against the full extension root
    */
  def checkInterlinksProof(interlinks: Seq[ModifierId],
                           proof: BatchMerkleProof[Digest32],
                           extensionRoot: Digest32): Boolean = {
    if (!PoPowHeaderSerializer.hasValidMerkleProofStructure(
      proof.indices.map(_._1),
      proof.proofs.size
    )) {
      false
    } else {
      Try {
        val expectedLeafHashes = NipopowAlgos.packInterlinks(interlinks)
          .map(Extension.kvToLeaf)
          .map(kv => Leaf[Digest32](LeafData @@ kv)(Algos.hash).hash)
        val provenLeafHashes = proof.indices.map(_._2)

        interlinks.nonEmpty &&
          expectedLeafHashes.size == provenLeafHashes.size &&
          expectedLeafHashes.zip(provenLeafHashes).forall { case (expected, proven) =>
            expected sameElements proven
          } &&
          proof.valid(extensionRoot)
      }.getOrElse(false)
    }
  }

  /**
    * Create PoPowHeader from a given block
    */
  def fromBlock(b: ErgoFullBlock): Try[PoPowHeader] = {
    val proof = NipopowAlgos.proofForInterlinkVector(b.extension).get
    NipopowAlgos.unpackInterlinks(b.extension.fields).map { interlinkVector =>
      PoPowHeader(b.header, interlinkVector, proof)
    }
  }

  implicit val interlinksEncoder: Encoder[Seq[ModifierId]] = Encoder.instance { interlinksVector: Seq[ModifierId] =>
    interlinksVector.map(id => id: String).asJson
  }

  implicit val batchMerkleProofEncoder: Encoder[BatchMerkleProof[Digest32]] = Encoder.instance { proof: BatchMerkleProof[Digest32] =>
    import org.ergoplatform.wallet.serialization.JsonCodecsWrapper.arrayBytesEncoder

    val indicesAsJson = proof.indices.map(i => Json.obj(fields =
      "index" -> i._1.asJson,
      "digest" -> i._2.array.asJson(arrayBytesEncoder)
    ))
    val proofsAsJson = proof.proofs.map(p => Json.obj(fields =
      "digest" -> p._1.array.asJson(arrayBytesEncoder),
      "side" -> p._2.toByte.asJson
    ))
    Json.obj(fields =
      "indices" -> Json.fromValues(indicesAsJson),
      "proofs" -> Json.fromValues(proofsAsJson)
    )
  }

  implicit val batchMerkleProofDecoder: Decoder[BatchMerkleProof[Digest32]] = Decoder.instance { p =>
    import org.ergoplatform.wallet.serialization.JsonCodecsWrapper.arrayBytesDecoder

    for {
      indicesJson <- p.downField("indices").as[List[Json]]
      indices <- Traverse[List].traverse(indicesJson)(
        indexJson => indexJson.hcursor.downField("index").as[Int]
      )
      indexDigests <- Traverse[List].traverse(indicesJson)(
        indexJson => indexJson.hcursor.downField("digest").as[Array[Byte]](arrayBytesDecoder)
      )
      proofsJson <- p.downField("proofs").as[List[Json]]
      proofBytes <- Traverse[List].traverse(proofsJson)(
        proofsJson => proofsJson.hcursor.downField("digest").as[Array[Byte]](arrayBytesDecoder)
      )
      proofSides <- Traverse[List].traverse(proofsJson)(
        proofsJson => proofsJson.hcursor.downField("side").as[Byte]
      )
    } yield BatchMerkleProof(
      indices zip indexDigests.map(i => Digest32 @@ i),
      proofBytes.map(p => Digest32 @@ p) zip proofSides.map(s => Side @@ s))
  }

  implicit val popowHeaderJsonEncoder: Encoder[PoPowHeader] = Encoder.instance { p: PoPowHeader =>
    Map(
      "header" -> p.header.asJson,
      //order in JSON array is preserved according to RFC 7159
      "interlinks" -> p.interlinks.asJson,
      "interlinksProof" -> p.interlinksProof.asJson
    ).asJson
  }

  implicit val popowHeaderJsonDecoder: Decoder[PoPowHeader] = Decoder.instance { c =>
    for {
      header <- c.downField("header").as[Header]
      interlinks <- c.downField("interlinks").as[Seq[String]]
      proof <- c.downField("interlinksProof").as[BatchMerkleProof[Digest32]]
    } yield PoPowHeader(header, interlinks.map(s => ModifierId @@ s), proof)
  }
}

/**
  * Binary serializer for PoPowHeader
  */
object PoPowHeaderSerializer extends ErgoSerializer[PoPowHeader] {
  import org.ergoplatform.sdk.wallet.Constants.ModifierIdLength

  // Generous wire sanity limits shared with the sigma-rust NiPoPoW parser.
  private[ergoplatform] final val MaxHeaderBytes = 10000
  private[ergoplatform] final val MaxInterlinks = 10000
  private[ergoplatform] final val MaxMerkleProofBytes = 1000000
  private[ergoplatform] final val MaxSerializedBytes =
    MaxHeaderBytes + MaxInterlinks * ModifierIdLength + MaxMerkleProofBytes + 64

  private val MerkleProofCountBytes = 8
  private val MerkleIndexBytes = 36
  private val MerkleProofNodeBytes = 33
  private[ergoplatform] final val MaxMerkleProofDepth =
    Extension.FieldKeySize * java.lang.Byte.SIZE
  private val MaxMerkleLeafIndex = (1 << MaxMerkleProofDepth) - 1

  implicit val hf: HF = Algos.hash
  val merkleProofSerializer = new BatchMerkleProofSerializer[Digest32, HF]

  private def requireWithinLimit(value: Int, limit: Int, what: String): Unit =
    require(value <= limit, s"$what $value exceeds sanity limit $limit")

  private[ergoplatform] def hasValidMerkleProofStructure(indices: Seq[Int],
                                                          proofCount: Int): Boolean = {
    if (indices.isEmpty) {
      proofCount == 0
    } else if (proofCount < 0 ||
      indices.exists(index => index < 0 || index > MaxMerkleLeafIndex) ||
      indices.distinct.size != indices.size) {
      false
    } else {
      var current = indices.sorted.toVector
      var remainingProofs = proofCount
      var depth = 0
      var valid = true

      while (valid && !(current.size == 1 && current.head == 0 && remainingProofs == 0) &&
        depth < MaxMerkleProofDepth) {
        val currentIndices = current.toSet
        val missingSiblings = current.count(index => !currentIndices.contains(index ^ 1))
        if (missingSiblings > remainingProofs) {
          valid = false
        } else {
          remainingProofs -= missingSiblings
          current = current.map(_ / 2).distinct
          depth += 1
        }
      }

      valid && current.size == 1 && current.head == 0 && remainingProofs == 0
    }
  }

  private def validateMerkleProofPayload(bytes: Array[Byte]): Unit = {
    require(bytes.length >= MerkleProofCountBytes,
      s"Merkle proof counts require at least $MerkleProofCountBytes bytes")
    // BatchMerkleProofSerializer stores both counts as fixed-width big-endian ints.
    val counts = ByteBuffer.wrap(bytes)
    val indexCount = counts.getInt
    val proofCount = counts.getInt
    require(indexCount >= 0 && proofCount >= 0,
      "Merkle proof counts must be non-negative")

    val indexBytes = Math.multiplyExact(indexCount.toLong, MerkleIndexBytes.toLong)
    val proofBytes = Math.multiplyExact(proofCount.toLong, MerkleProofNodeBytes.toLong)
    val requiredBytes = Math.addExact(
      MerkleProofCountBytes.toLong,
      Math.addExact(indexBytes, proofBytes)
    )
    require(requiredBytes == bytes.length.toLong,
      s"Merkle proof counts require $requiredBytes bytes, payload has ${bytes.length}")

    val indices = Vector.newBuilder[Int]
    var index = 0
    while (index < indexCount) {
      indices += counts.getInt
      counts.position(counts.position() + MerkleIndexBytes - Integer.BYTES)
      index += 1
    }
    require(hasValidMerkleProofStructure(indices.result(), proofCount),
      "Merkle proof structure exceeds the extension key space")
  }

  override def serialize(obj: PoPowHeader, w: Writer): Unit = {
    val headerBytes = obj.header.bytes
    w.putUInt(headerBytes.length.toLong)
    w.putBytes(headerBytes)
    w.putUInt(obj.interlinks.size.toLong)
    obj.interlinks.foreach(x => w.putBytes(idToBytes(x)))
    val proofBytes = merkleProofSerializer.serialize(obj.interlinksProof)
    w.putUInt(proofBytes.length.toLong)
    w.putBytes(proofBytes)
  }

  override def parse(r: Reader): PoPowHeader = {
    val headerLength = r.getUInt().toIntExact
    requireWithinLimit(headerLength, MaxHeaderBytes, "header length")
    val header = HeaderSerializer.parseBytes(r.getBytes(headerLength))
    val linksQty = r.getUInt().toIntExact
    requireWithinLimit(linksQty, MaxInterlinks, "interlink count")
    val interlinks = (0 until linksQty).map(_ => bytesToId(r.getBytes(ModifierIdLength)))
    val interlinksProofLength = r.getUInt().toIntExact
    requireWithinLimit(interlinksProofLength, MaxMerkleProofBytes, "Merkle proof length")
    val proofBytes = r.getBytes(interlinksProofLength)
    validateMerkleProofPayload(proofBytes)
    val interlinksProof = merkleProofSerializer.deserialize(proofBytes).get
    PoPowHeader(header, interlinks, interlinksProof)
  }

}
