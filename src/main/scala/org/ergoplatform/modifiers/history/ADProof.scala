package org.ergoplatform.modifiers.history

import com.google.common.primitives.Bytes
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.modifiers.history.ADProof.ProofRepresentation
import org.ergoplatform.modifiers.mempool.proposition.{AnyoneCanSpendNoncedBox, AnyoneCanSpendProposition}
import org.ergoplatform.modifiers.{ErgoPersistentModifier, ModifierWithDigest}
import org.ergoplatform.nodeView.state.ErgoState.Digest
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer
import scorex.core.transaction.state.{BoxStateChangeOperation, BoxStateChanges, Insertion, Removal}
import scorex.crypto.authds.avltree.AVLValue
import scorex.crypto.authds.avltree.batch.{BatchAVLVerifier, Insert, Modification, Remove}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256Unsafe

import scala.util.{Failure, Success, Try}

case class ADProof(headerId: ModifierId, proofBytes: ProofRepresentation) extends ErgoPersistentModifier
  with ModifierWithDigest {

  override def digest: Array[ModifierTypeId] = ADProof.proofDigest(proofBytes)

  override val modifierTypeId: ModifierTypeId = ADProof.ModifierTypeId

  override type M = ADProof

  override lazy val serializer: Serializer[ADProof] = ADProofSerializer

  override lazy val json: Json = Map(
    "headerId" -> Base58.encode(headerId).asJson,
    "proofBytes" -> Base58.encode(proofBytes).asJson,
    "digest" -> Base58.encode(digest).asJson,
  ).asJson

  override def toString: String = s"ADProofs(${Base58.encode(id)},${Base58.encode(headerId)},${Base58.encode(proofBytes)})"

  /**
    * Verify a set of box(outputs) operations on authenticated UTXO set by using the proof (this class wraps).
    *
    * @param changes      - ordered sequence of box operations(remove/insert) to check against a tree with known
    * @param previousHash - hash(from previous block) to apply the proof to.
    * @param expectedHash - expected (declared by miner) hash. A hash after applying proof must be the same.
    * @return Success, if verification passed
    */
  def verify(changes: BoxStateChanges[AnyoneCanSpendProposition, AnyoneCanSpendNoncedBox],
             previousHash: Digest,
             expectedHash: Digest): Try[Unit] = {

    def applyChanges(verifier: BatchAVLVerifier[Blake2b256Unsafe],
                     changes: BoxStateChanges[AnyoneCanSpendProposition, AnyoneCanSpendNoncedBox]) =
      changes.operations.foldLeft[Try[Option[AVLValue]]](Success(None)) { case (t, o) =>
        t.flatMap(_ => {
          verifier.performOneOperation(ADProof.changeToMod(o))
        })
      }

    val verifier = new BatchAVLVerifier[Blake2b256Unsafe](previousHash, proofBytes, ADProof.KL, None, maxNumOperations = Some(changes.operations.size))

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

object ADProof {
  type ProofRepresentation = Array[Byte]

  val ModifierTypeId: Byte = 104: Byte

  val KL = 32

  def proofDigest(proofBytes: ProofRepresentation): Array[Byte] = Algos.hash(proofBytes)

  /**
    * Convert operation over a box into an AVL+ tree modification
    * @param change - operation over a box
    * @return AVL+ tree modification
    */
  def changeToMod(change: BoxStateChangeOperation[AnyoneCanSpendProposition, AnyoneCanSpendNoncedBox]): Modification =
    change match {
      case i: Insertion[AnyoneCanSpendProposition, AnyoneCanSpendNoncedBox] =>
        Insert(i.box.id, i.box.bytes)
      case r: Removal[AnyoneCanSpendProposition, AnyoneCanSpendNoncedBox] =>
        Remove(r.boxId)
    }
}

object ADProofSerializer extends Serializer[ADProof] {
  override def toBytes(obj: ADProof): Array[Byte] = Bytes.concat(obj.headerId, obj.proofBytes)

  override def parseBytes(bytes: Array[Byte]): Try[ADProof] = Try {
    ADProof(bytes.take(Constants.ModifierIdSize), bytes.slice(Constants.ModifierIdSize, bytes.length))
  }
}
