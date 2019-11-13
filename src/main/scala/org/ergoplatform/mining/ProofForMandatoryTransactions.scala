package org.ergoplatform.mining

import io.circe.{Encoder, Json}
import org.ergoplatform.modifiers.history.{BlockTransactions, HeaderSerializer, HeaderWithoutPow}
import org.ergoplatform.nodeView.mempool.TransactionMembershipProof
import org.ergoplatform.settings.Algos
import io.circe.syntax._


case class ProofForMandatoryTransactions(minHeader: HeaderWithoutPow, txProofs: Seq[TransactionMembershipProof]) {
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
