package org.ergoplatform.modifiers.history

import com.google.common.primitives.Bytes
import io.circe.Encoder
import io.circe.syntax._
import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.{ErgoPersistentModifier, ModifierWithDigest}
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.settings.Algos.HF
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.serialization.Serializer
import scorex.core.transaction.state.{BoxStateChangeOperation, BoxStateChanges, Insertion, Removal}
import scorex.core.{ModifierId, ModifierTypeId}
import scorex.crypto.authds.avltree.batch.{BatchAVLVerifier, Insert, Modification, Remove}
import scorex.crypto.authds.{ADDigest, ADValue, SerializedAdProof}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.{Blake2b256, Digest32}

import scala.util.{Failure, Success, Try}

case class ADProofs(headerId: ModifierId, proofBytes: SerializedAdProof) extends ErgoPersistentModifier
  with ModifierWithDigest {

  override def digest: Digest32 = ADProofs.proofDigest(proofBytes)

  override val modifierTypeId: ModifierTypeId = ADProofs.modifierTypeId

  override type M = ADProofs

  override lazy val serializer: Serializer[ADProofs] = ADProofSerializer

  override def toString: String = {
    val sId = Algos.encode(id)
    val sHeaderId = Algos.encode(headerId)

    /**
      * Sometimes proofBytes could have length about 350 000 elements, it's useless to convert them into string.
      * So decisin here is to not render them in toString method.
      */
    //val sProofBytes = Algos.encode(proofBytes)
    s"ADProofs(Id:$sId,HeaderId:$sHeaderId)"
  }

  /**
    * Verify a set of box(outputs) operations on authenticated UTXO set by using the proof (this class wraps).
    *
    * @param changes      - ordered sequence of box operations(remove/insert) to check against a tree with known
    * @param previousHash - hash(from previous block) to apply the proof to.
    * @param expectedHash - expected (declared by miner) hash. A hash after applying proof must be the same.
    * @return Success, if verification passed
    */
  def verify(changes: BoxStateChanges[AnyoneCanSpendProposition.type, ErgoBox],
             previousHash: ADDigest,
             expectedHash: ADDigest): Try[Unit] = {

    def applyChanges(verifier: BatchAVLVerifier[Digest32, HF],
                     changes: BoxStateChanges[AnyoneCanSpendProposition.type, ErgoBox]) =
      changes.operations.foldLeft[Try[Option[ADValue]]](Success(None)) { case (t, o) =>
        t.flatMap(_ => {
          verifier.performOneOperation(ADProofs.changeToMod(o))
        })
      }

    val verifier = new BatchAVLVerifier[Digest32, HF](previousHash, proofBytes, ADProofs.KL,
      None, maxNumOperations = Some(changes.operations.size))

    applyChanges(verifier, changes).flatMap { _ =>
      verifier.digest match {
        case Some(digest) =>
          if (digest sameElements expectedHash) {
            Success()
          } else {
            Failure(new IllegalArgumentException(s"Unexpected result digest: ${Base58.encode(digest)} != ${Base58.encode(expectedHash)}"))
          }
        case None =>
          Failure(new IllegalStateException("Digest is undefined"))
      }
    }
  }
}

object ADProofs {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (104: Byte)

  val KL = 32

  def proofDigest(proofBytes: SerializedAdProof): Digest32 = Algos.hash(proofBytes)

  /**
    * Convert operation over a box into an AVL+ tree modification
    *
    * @param change - operation over a box
    * @return AVL+ tree modification
    */
  def changeToMod(change: BoxStateChangeOperation[AnyoneCanSpendProposition.type, ErgoBox]): Modification =
    change match {
      case i: Insertion[AnyoneCanSpendProposition.type, ErgoBox] =>
        Insert(i.box.id, ADValue @@ i.box.bytes)
      case r: Removal[AnyoneCanSpendProposition.type, ErgoBox] =>
        Remove(r.boxId)
    }

  implicit val jsonEncoder: Encoder[ADProofs] = (proof: ADProofs) =>
    Map(
      "headerId" -> Base58.encode(proof.headerId).asJson,
      "proofBytes" -> Base58.encode(proof.proofBytes).asJson,
      "digest" -> Base58.encode(proof.digest).asJson
    ).asJson
}

object ADProofSerializer extends Serializer[ADProofs] {
  override def toBytes(obj: ADProofs): Array[Byte] = Bytes.concat(obj.headerId, obj.proofBytes)

  override def parseBytes(bytes: Array[Byte]): Try[ADProofs] = Try {
    ADProofs(
      ModifierId @@ bytes.take(Constants.ModifierIdSize),
      SerializedAdProof @@ bytes.slice(Constants.ModifierIdSize, bytes.length))
  }
}
