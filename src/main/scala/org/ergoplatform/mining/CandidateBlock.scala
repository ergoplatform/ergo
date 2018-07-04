package org.ergoplatform.mining

import io.circe.{Encoder, Json}
import io.circe.syntax._
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.JsonEncoders
import scorex.core.block.Block.Timestamp
import scorex.crypto.authds.{ADDigest, SerializedAdProof}
import scorex.crypto.hash.Digest32

case class CandidateBlock(parentOpt: Option[Header],
                          nBits: Long,
                          stateRoot: ADDigest,
                          adProofBytes: SerializedAdProof,
                          transactions: Seq[ErgoTransaction],
                          timestamp: Timestamp,
                          extensionHash: Digest32) {

  override def toString: String = s"CandidateBlock(${JsonEncoders.default.candidateBlockEncoder(this).noSpaces})"
}

class CandidateBlockEncoder(implicit transactionEncoder: Encoder[ErgoTransaction]) extends Encoder[CandidateBlock] {

  def apply(c: CandidateBlock): Json = {
    Map(
      "parentId" -> c.parentOpt.map(p => Algos.encode(p.id)).getOrElse("None").asJson,
      "nBits" -> c.nBits.asJson,
      "stateRoot" -> Algos.encode(c.stateRoot).asJson,
      "adProofBytes" -> Algos.encode(c.adProofBytes).asJson,
      "timestamp" -> c.timestamp.asJson,
      "transactions" -> c.transactions.map(_.asJson).asJson,
      "transactionsNumber" -> c.transactions.length.asJson,
      "extensionHash" -> Algos.encode(c.extensionHash).asJson
    ).asJson
  }

}
