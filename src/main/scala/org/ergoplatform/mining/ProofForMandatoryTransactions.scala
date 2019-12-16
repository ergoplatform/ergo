package org.ergoplatform.mining

import io.circe.{Encoder, Json}
import org.ergoplatform.modifiers.history.{BlockTransactions, HeaderSerializer, HeaderWithoutPow}
import org.ergoplatform.nodeView.mempool.TransactionMembershipProof
import org.ergoplatform.settings.Algos
import io.circe.syntax._

/**
  * Proof of inclusion of certain transactions into a block with known (unproven) header
  * @param minHeader - (unproven or proven) header
  * @param txProofs - proofs of membership for transactions (against a transactions Merkle tree digest in the header)
  */
case class ProofForMandatoryTransactions(minHeader: HeaderWithoutPow, txProofs: Seq[TransactionMembershipProof]) {

  /**
    * Checks that all the proofs of membership are valid
    * @return true if all the transactions are valid, false otherwise
    */
  def check(): Boolean = txProofs.forall { tp =>
    BlockTransactions.proofValid(minHeader.transactionsRoot, tp)
  }
}

object ProofForMandatoryTransactions {

  import TransactionMembershipProof.txMembershipProofEncoder

  implicit val encoder: Encoder[ProofForMandatoryTransactions] = { p: ProofForMandatoryTransactions =>
    val preimageBytes = HeaderSerializer.bytesWithoutPow(p.minHeader)
    Json.obj(
      "msgPreimage" -> Algos.encode(preimageBytes).asJson,
      "txProofs" -> p.txProofs.asJson
    )
  }

}
