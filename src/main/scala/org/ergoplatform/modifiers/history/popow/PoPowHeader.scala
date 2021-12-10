package org.ergoplatform.modifiers.history.popow

import io.circe.{Decoder, Encoder, Json}
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.header.{Header, HeaderSerializer}
import org.ergoplatform.settings.Algos
import org.ergoplatform.settings.Algos.HF
import scorex.core.serialization.{BytesSerializable, ScorexSerializer}
import scorex.crypto.authds.{LeafData, Side}
import scorex.crypto.authds.merkle.{BatchMerkleProof, MerkleTree}
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
case class PoPowHeader(header: Header, interlinks: Seq[ModifierId], proof: BatchMerkleProof[Digest32]) extends BytesSerializable {

  override type M = PoPowHeader

  implicit val hf = Blake2b256
  val powScheme: AutolykosPowScheme = new AutolykosPowScheme(32, 26)
  val nipopowAlgos: NipopowAlgos = new NipopowAlgos(powScheme)

  override def serializer: ScorexSerializer[M] = PoPowHeaderSerializer

  def id: ModifierId = header.id

  def height: Int = header.height

  def checkProof(): Boolean = {
    val packed = nipopowAlgos.packInterlinks(interlinks)
    val leafData = packed.map(_._2).map(LeafData @@ _)
    val treeRoot = MerkleTree(leafData)(hf).rootHash
    proof.valid(treeRoot)
  }
}

object PoPowHeader {

  import io.circe.syntax._

  implicit val hf = Blake2b256
  val powScheme: AutolykosPowScheme = new AutolykosPowScheme(32, 26)
  val nipopowAlgos: NipopowAlgos = new NipopowAlgos(powScheme)

  def fromBlock(b: ErgoFullBlock): Try[PoPowHeader] = {
    val proof = nipopowAlgos.proofForInterlinkVector(b.extension).get
    NipopowAlgos.unpackInterlinks(b.extension.fields).map { interlinkVector =>
      PoPowHeader(b.header, interlinkVector, proof)
    }
  }

  implicit val interlinksEncoder: Encoder[Seq[ModifierId]] = { interlinksVector: Seq[ModifierId] =>
    interlinksVector.map(id => id: String).asJson
  }

  implicit val proofEncoder: Encoder[BatchMerkleProof[Digest32]] = { proof: BatchMerkleProof[Digest32] =>

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

  implicit val proofDecoder: Decoder[BatchMerkleProof[Digest32]] = { p =>

    for {
      indices <- p.downField("indices").as[Seq[(Int, Array[Byte])]]
      proofs <- p.downField("proofs").as[Seq[(Array[Byte], Byte)]]
    } yield BatchMerkleProof(
        indices.map(i => (i._1, Digest32 @@ i._2)),
        proofs.map(p => (Digest32 @@ p._1, Side @@ p._2)))
  }

  implicit val popowHeaderJsonEncoder: Encoder[PoPowHeader] = { p: PoPowHeader =>
    Map(
      "header" -> p.header.asJson,
      //order in JSON array is preserved according to RFC 7159
      "interlinks" -> p.interlinks.asJson,
      "proof" -> p.proof.asJson
    ).asJson
  }

  implicit val popowHeaderJsonDecoder: Decoder[PoPowHeader] = { c =>
    for {
      header <- c.downField("header").as[Header]
      interlinks <- c.downField("interlinks").as[Seq[String]]
      proof <- c.downField("proof").as[BatchMerkleProof[Digest32]]
    } yield PoPowHeader(header, interlinks.map(s => ModifierId @@ s), proof)
  }
}

object PoPowHeaderSerializer extends ScorexSerializer[PoPowHeader] {
  import org.ergoplatform.wallet.Constants.ModifierIdLength

  implicit val hf: HF = Algos.hash
  val merkleSerializer = new BatchMerkleProofSerializer[Digest32, HF]

  override def serialize(obj: PoPowHeader, w: Writer): Unit = {
    val headerBytes = obj.header.bytes
    w.putUInt(headerBytes.length)
    w.putBytes(headerBytes)
    w.putUInt(obj.interlinks.size)
    obj.interlinks.foreach(x => w.putBytes(idToBytes(x)))
    val proofBytes = merkleSerializer.serialize(obj.proof)
    w.putUInt(proofBytes.length)
    w.putBytes(proofBytes)
  }

  override def parse(r: Reader): PoPowHeader = {
    val headerSize = r.getUInt().toIntExact
    val header = HeaderSerializer.parseBytes(r.getBytes(headerSize))
    val linksQty = r.getUInt().toIntExact
    val interlinks = (0 until linksQty).map(_ => bytesToId(r.getBytes(ModifierIdLength)))
    val proofSize = r.getUInt().toIntExact
    val proof = merkleSerializer.deserialize(r.getBytes(proofSize)).get
    PoPowHeader(header, interlinks, proof)
  }

}
