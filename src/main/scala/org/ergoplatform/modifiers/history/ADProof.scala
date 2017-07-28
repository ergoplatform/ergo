package org.ergoplatform.modifiers.history

import com.google.common.primitives.Bytes
import io.circe.Json
import org.ergoplatform.modifiers.history.ADProof.ProofRepresentation
import org.ergoplatform.modifiers.mempool.proposition.{AnyoneCanSpendNoncedBox, AnyoneCanSpendProposition}
import org.ergoplatform.modifiers.{ErgoPersistentModifier, ModifierWithDigest}
import org.ergoplatform.nodeView.state.ErgoState.Digest
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer
import scorex.core.transaction.state.BoxStateChanges
import scorex.crypto.encode.Base58

import scala.util.Try

case class ADProof(headerId: ModifierId, proofBytes: ProofRepresentation) extends ErgoPersistentModifier
  with ModifierWithDigest {

  override def digest: Array[ModifierTypeId] = ADProof.proofDigest(proofBytes)

  override val modifierTypeId: ModifierTypeId = ADProof.ModifierTypeId

  override type M = ADProof

  override lazy val serializer: Serializer[ADProof] = ADProofSerializer

  override lazy val json: Json = ???

  override def toString: String = s"ADProofs(${Base58.encode(id)},${Base58.encode(headerId)},${Base58.encode(proofBytes)})"

  //todo: for tolsi: implement
  /**
    * Verify a set of box(outputs) operations on authenticated UTXO set by using the proof (this class wraps).
    *
    * @param changes      - ordered sequence of box operations(remove/insert) to check against a tree with known
    * @param previousHash - hash(from previous block) to apply the proof to.
    * @param expectedHash - expected (declared by miner) hash. A hash after applying proof must be the same.
    * @return
    */
  def verify(changes: BoxStateChanges[AnyoneCanSpendProposition, AnyoneCanSpendNoncedBox],
             previousHash: Digest,
             expectedHash: Digest): Try[Unit] = ???
}

object ADProof {
  type ProofRepresentation = Array[Byte]

  val ModifierTypeId: Byte = 104: Byte

  def proofDigest(proofBytes: ProofRepresentation): Array[Byte] = Algos.hash(proofBytes)
}

object ADProofSerializer extends Serializer[ADProof] {
  override def toBytes(obj: ADProof): Array[Byte] = Bytes.concat(obj.headerId, obj.proofBytes)

  override def parseBytes(bytes: Array[Byte]): Try[ADProof] = Try {
    ADProof(bytes.take(Constants.ModifierIdSize), bytes.slice(Constants.ModifierIdSize, bytes.length))
  }
}
