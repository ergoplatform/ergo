package org.ergoplatform.modifiers.history

import cats.syntax.either._
import sigmastate.utils.Helpers._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.modifiers.{NetworkObjectTypeId, NonHeaderBlockSection, ProofsTypeId}
import org.ergoplatform.modifiers.state._
import org.ergoplatform.settings.Algos.HF
import org.ergoplatform.settings.{Algos, Constants}
import org.ergoplatform.serialization.ErgoSerializer
import scorex.crypto.authds.avltree.batch.{Lookup => _, _}
import scorex.crypto.authds.{ADDigest, ADValue, SerializedAdProof}
import scorex.crypto.hash.Digest32
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, bytesToId, idToBytes}
import scorex.util.Extensions._

import scala.util.{Failure, Success, Try}

case class ADProofs(headerId: ModifierId,
                    proofBytes: SerializedAdProof,
                    override val sizeOpt: Option[Int] = None) extends NonHeaderBlockSection {

  override def digest: Digest32 = ADProofs.proofDigest(proofBytes)

  override val modifierTypeId: NetworkObjectTypeId.Value = ADProofs.modifierTypeId

  override type M = ADProofs

  override lazy val serializer: ErgoSerializer[ADProofs] = ADProofsSerializer

  override def toString: String = s"ADProofs(Id:$id,HeaderId:$headerId)"

  /**
    * Verify a set of box(outputs) operations on authenticated UTXO set by using the proof (this class wraps).
    *
    * @param changes      - ordered sequence of box operations(remove/insert) to check against a tree with known
    * @param previousHash - hash(from previous block) to apply the proof to.
    * @param expectedHash - expected (declared by miner) hash. A hash after applying proof must be the same.
    * @return Success, if verification passed
    */
  def verify(changes: StateChanges,
             previousHash: ADDigest,
             expectedHash: ADDigest): Try[Seq[ADValue]] = {

    def applyChanges(verifier: BatchAVLVerifier[Digest32, HF],
                     changes: StateChanges): Try[Seq[ADValue]] = Try {
      changes.operations.flatMap(o => verifier.performOneOperation(o).get)
    }

    val verifier = new BatchAVLVerifier[Digest32, HF](previousHash, proofBytes, ADProofs.KL,
      None, maxNumOperations = Some(changes.operations.size))

    applyChanges(verifier, changes).flatMap { oldValues =>
      verifier.digest match {
        case Some(digest) =>
          if (java.util.Arrays.equals(digest, expectedHash)) {
            Success(oldValues)
          } else {
            val msg = s"Unexpected result digest: ${Algos.encode(digest)} != ${Algos.encode(expectedHash)}"
            Failure(new IllegalArgumentException(msg))
          }
        case None =>
          Failure(new IllegalStateException("Digest is undefined"))
      }
    }
  }
}

object ADProofs extends ApiCodecs {

  val modifierTypeId: NetworkObjectTypeId.Value = ProofsTypeId.value

  val KL = 32

  def proofDigest(proofBytes: SerializedAdProof): Digest32 = Algos.hash(proofBytes)

  implicit val jsonEncoder: Encoder[ADProofs] = Encoder.instance { proof: ADProofs =>
    Map(
      "headerId" -> Algos.encode(proof.headerId).asJson,
      "proofBytes" -> Algos.encode(proof.proofBytes).asJson,
      "digest" -> Algos.encode(proof.digest).asJson,
      "size" -> proof.size.asJson
    ).asJson
  }

  implicit val jsonDecoder: Decoder[ADProofs] = Decoder.instance { c: HCursor =>
    for {
      headerId <- c.downField("headerId").as[ModifierId]
      proofBytes <- c.downField("proofBytes").as[Array[Byte]]
      size <- c.downField("size").as[Option[Int]]
    } yield ADProofs(headerId, SerializedAdProof @@ proofBytes, size)
  }
}

object ADProofsSerializer extends ErgoSerializer[ADProofs] {

  override def serialize(obj: ADProofs, w: Writer): Unit = {
    w.putBytes(idToBytes(obj.headerId))
    w.putUInt(obj.proofBytes.size.toLong)
    w.putBytes(obj.proofBytes)
  }

  override def parse(r: Reader): ADProofs = {
    val startPos = r.position
    val headerId = bytesToId(r.getBytes(Constants.ModifierIdSize))
    val size = r.getUInt().toIntExact
    val proofBytes = SerializedAdProof @@ r.getBytes(size)
    val endPos = r.position
    ADProofs(headerId, proofBytes, Some(endPos - startPos))
  }

}
