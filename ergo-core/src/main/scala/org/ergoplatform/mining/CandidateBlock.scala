package org.ergoplatform.mining

import io.circe.Encoder
import io.circe.syntax._
import org.ergoplatform.modifiers.history.extension.ExtensionCandidate
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.settings.Algos
import scorex.crypto.authds.merkle.BatchMerkleProof
import scorex.crypto.authds.{ADDigest, SerializedAdProof}
import scorex.crypto.hash.Digest32

case class CandidateBlock(parentOpt: Option[Header],
                          version: Header.Version,
                          nBits: Long,
                          stateRoot: ADDigest,
                          adProofBytes: SerializedAdProof,
                          transactions: Seq[ErgoTransaction],
                          timestamp: Header.Timestamp,
                          extension: ExtensionCandidate,
                          votes: Array[Byte],
                          inputBlockFields: Seq[(Array[Byte], Array[Byte])],
                          inputBlockFieldsProof: BatchMerkleProof[Digest32],
                          inputBlockTransactions: Seq[ErgoTransaction]) {

  override def toString: String = s"CandidateBlock(${this.asJson})"

}

object CandidateBlock {

  implicit val jsonEncoder: Encoder[CandidateBlock] = Encoder.instance((c: CandidateBlock) =>
    Map(
      "parentId" -> c.parentOpt.map(p => Algos.encode(p.id)).getOrElse("None").asJson,
      "version" -> c.version.asJson,
      "nBits" -> c.nBits.asJson,
      "stateRoot" -> Algos.encode(c.stateRoot).asJson,
      "adProofBytes" -> Algos.encode(c.adProofBytes).asJson,
      "timestamp" -> c.timestamp.asJson,
      "transactions" -> c.transactions.map(_.asJson).asJson,
      "transactionsNumber" -> c.transactions.length.asJson,
      "votes" -> Algos.encode(c.votes).asJson,
      "extensionHash" -> Algos.encode(c.extension.digest).asJson,
      // todo: add input block related fields
    ).asJson)

}
