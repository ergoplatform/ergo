package org.ergoplatform.modifiers.history.popow

import cats.Traverse
import cats.implicits.{catsStdInstancesForEither, catsStdInstancesForList}
import io.circe.{Decoder, Encoder, Json}
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.extension.Extension.merkleTree
import org.ergoplatform.modifiers.history.header.{Header, HeaderSerializer}
import org.ergoplatform.settings.Algos
import org.ergoplatform.settings.Algos.HF
import scorex.core.serialization.{BytesSerializable, ScorexSerializer}
import scorex.crypto.authds.Side
import scorex.crypto.authds.merkle.BatchMerkleProof
import scorex.crypto.authds.merkle.serialization.BatchMerkleProofSerializer
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util.Extensions._
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, bytesToId, idToBytes}

import scala.util.Try

/**
  * Block header along with unpacked interlinks
  *
  * Interlinks are stored in reverse order: first element is always genesis header, then level of lowest target met etc
  *
  * Not used in the consensus protocol
  *
  */
case class PoPowHeader(header: Header, interlinks: Seq[ModifierId], interlinksProof: BatchMerkleProof[Digest32]) extends BytesSerializable {

  override type M = PoPowHeader

  override def serializer: ScorexSerializer[M] = PoPowHeaderSerializer

  def id: ModifierId = header.id

  def height: Int = header.height

  def checkInterlinksProof(): Boolean = PoPowHeader.checkInterlinksProof(interlinks, interlinksProof)
}

object PoPowHeader {

  import io.circe.syntax._

  implicit val hf: HF = Blake2b256
  val powScheme: AutolykosPowScheme = new AutolykosPowScheme(32, 26)
  val nipopowAlgos: NipopowAlgos = new NipopowAlgos(powScheme)

  /**
    * Validates interlinks merkle root against provided proof
    */
  def checkInterlinksProof(interlinks: Seq[ModifierId], proof: BatchMerkleProof[Digest32]): Boolean = {
    val fields = nipopowAlgos.packInterlinks(interlinks)
    val tree = merkleTree(fields)
    proof.valid(tree.rootHash)
  }

  /**
    * Create PoPowHeader from a given block
    */
  def fromBlock(b: ErgoFullBlock): Try[PoPowHeader] = {
    val proof = nipopowAlgos.proofForInterlinkVector(b.extension).get
    NipopowAlgos.unpackInterlinks(b.extension.fields).map { interlinkVector =>
      PoPowHeader(b.header, interlinkVector, proof)
    }
  }

  implicit val interlinksEncoder: Encoder[Seq[ModifierId]] = { interlinksVector: Seq[ModifierId] =>
    interlinksVector.map(id => id: String).asJson
  }

  implicit val batchMerkleProofEncoder: Encoder[BatchMerkleProof[Digest32]] = { proof: BatchMerkleProof[Digest32] =>

    val indicesAsJson = proof.indices.map(i => Json.obj(fields =
      "index" -> i._1.asJson,
      "digest" -> i._2.toArray.asJson
    ))
    val proofsAsJson = proof.proofs.map(p => Json.obj(fields =
      "digest" -> p._1.toArray.asJson,
      "side" -> p._2.toByte.asJson
    ))
    Json.obj(fields =
      "indices" -> Json.fromValues(indicesAsJson),
      "proofs" -> Json.fromValues(proofsAsJson)
    )
  }

  implicit val batchMerkleProofDecoder: Decoder[BatchMerkleProof[Digest32]] = { p =>

    for {
      indicesJson <- p.downField("indices").as[List[Json]]
      indices <- Traverse[List].traverse(indicesJson)(
        indexJson => indexJson.hcursor.downField("index").as[Int]
      )
      indexDigests <- Traverse[List].traverse(indicesJson)(
        indexJson => indexJson.hcursor.downField("digest").as[Array[Byte]]
      )
      proofsJson <- p.downField("proofs").as[List[Json]]
      proofBytes <- Traverse[List].traverse(proofsJson)(
        proofsJson => proofsJson.hcursor.downField("digest").as[Array[Byte]]
      )
      proofSides <- Traverse[List].traverse(proofsJson)(
        proofsJson => proofsJson.hcursor.downField("side").as[Byte]
      )
    } yield BatchMerkleProof(
      indices zip indexDigests.map(i => Digest32 @@ i),
      proofBytes.map(p => Digest32 @@ p) zip proofSides.map(s => Side @@ s))
  }

  implicit val popowHeaderJsonEncoder: Encoder[PoPowHeader] = { p: PoPowHeader =>
    Map(
      "header" -> p.header.asJson,
      //order in JSON array is preserved according to RFC 7159
      "interlinks" -> p.interlinks.asJson,
      "interlinksProof" -> p.interlinksProof.asJson
    ).asJson
  }

  implicit val popowHeaderJsonDecoder: Decoder[PoPowHeader] = { c =>
    for {
      header <- c.downField("header").as[Header]
      interlinks <- c.downField("interlinks").as[Seq[String]]
      proof <- c.downField("interlinksProof").as[BatchMerkleProof[Digest32]]
    } yield PoPowHeader(header, interlinks.map(s => ModifierId @@ s), proof)
  }
}

/**
  * Binary serializer for PoPowHeader,
  */
object PoPowHeaderSerializer extends ScorexSerializer[PoPowHeader] {
  import org.ergoplatform.wallet.Constants.ModifierIdLength

  implicit val hf: HF = Algos.hash
  val merkleProofSerializer = new BatchMerkleProofSerializer[Digest32, HF]

  override def serialize(obj: PoPowHeader, w: Writer): Unit = {
    val headerBytes = obj.header.bytes
    w.putUInt(headerBytes.length)
    w.putBytes(headerBytes)
    w.putUInt(obj.interlinks.size)
    obj.interlinks.foreach(x => w.putBytes(idToBytes(x)))
    val proofBytes = merkleProofSerializer.serialize(obj.interlinksProof)
    w.putUInt(proofBytes.length)
    w.putBytes(proofBytes)
  }

  override def parse(r: Reader): PoPowHeader = {
    val headerSize = r.getUInt().toIntExact
    val header = HeaderSerializer.parseBytes(r.getBytes(headerSize))
    val linksQty = r.getUInt().toIntExact
    val interlinks = (0 until linksQty).map(_ => bytesToId(r.getBytes(ModifierIdLength)))
    val interlinksProofSize = r.getUInt().toIntExact
    val interlinksProof = merkleProofSerializer.deserialize(r.getBytes(interlinksProofSize)).get
    PoPowHeader(header, interlinks, interlinksProof)
  }

}
