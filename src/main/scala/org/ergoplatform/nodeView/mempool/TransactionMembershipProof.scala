package org.ergoplatform.nodeView.mempool

import io.circe.{Encoder, Json}
import org.ergoplatform.JsonCodecs
import org.ergoplatform.settings.Algos
import scorex.crypto.authds.merkle.MerkleProof
import scorex.crypto.hash.Digest32
import scorex.util.ModifierId
import io.circe.syntax._

/**
  * Container for Merkle proof for a transaction. This proof is to be checked against externally provided expected
  * Merkle tree digest (from a block header).
  *
  * @param txId - transaction identifier
  * @param proof - Merkle proof of transaction membership
  */
case class TransactionMembershipProof(txId: ModifierId, proof: MerkleProof[Digest32])

object TransactionMembershipProof extends JsonCodecs {

  implicit val merkleProofEncoder: Encoder[MerkleProof[Digest32]] = { mp: MerkleProof[Digest32] =>
    Json.obj(
      "leaf" -> Algos.encode(mp.leafData).asJson,
      "levels" -> mp.levels.map{case (digest, side) => Algos.encode(side +: digest)}.asJson
    )
  }

  implicit val txMembershipProofEncoder: Encoder[TransactionMembershipProof] = { tmp: TransactionMembershipProof =>
    tmp.proof.asJson
  }

}
