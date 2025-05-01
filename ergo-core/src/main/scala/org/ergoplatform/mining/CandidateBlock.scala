package org.ergoplatform.mining

import io.circe.Encoder
import io.circe.syntax._
import org.ergoplatform.modifiers.history.extension.Extension.{InputBlockTransactionsDigestKey, PrevInputBlockIdKey, PreviousInputBlockTransactionsDigestKey}
import org.ergoplatform.modifiers.history.extension.ExtensionCandidate
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.settings.Algos
import scorex.crypto.authds.merkle.BatchMerkleProof
import scorex.crypto.authds.{ADDigest, SerializedAdProof}
import scorex.crypto.hash.Digest32

/**
* @param prevInputBlockId - previous sub block id `subBlock` is following, if missed, sub-block is linked
*                         to a previous block
* @param transactionsDigest - digest of new transactions appeared in subblock
*
* @param inputBlockFieldsProof - batch Merkle proof for `prevSubBlockId`` and `subblockTransactionsDigest`
*                      (as they are coming from extension section, and committed in `subBlock` header via extension
*                      digest)
*/
class InputBlockFields(val prevInputBlockId: Option[Array[Byte]],
                       val transactionsDigest: Digest32,
                       val prevTransactionsDigest: Digest32,
                       val inputBlockFieldsProof: BatchMerkleProof[Digest32])

object InputBlockFields {
  def empty: InputBlockFields = {
    new InputBlockFields(
      None,
      Digest32 @@ Array.fill(32)(0.toByte),
      Digest32 @@ Array.fill(32)(0.toByte),
      BatchMerkleProof(Seq.empty, Seq.empty)(Algos.hash))
  }

  def toExtensionFields(prevInputBlockIdOpt: Option[Array[Byte]],
                        transactionsDigest: Digest32,
                        prevTransactionsDigest: Digest32): ExtensionCandidate = {
    val prevInput = prevInputBlockIdOpt.map { prevInputBlockId =>
      (PrevInputBlockIdKey, prevInputBlockId)
    }.toSeq

    // digest (Merkle tree root) of new first-class transactions since last input-block
    val txs = (InputBlockTransactionsDigestKey, transactionsDigest)

    // digest (Merkle tree root) first class transactions since ordering block till last input-block
    val prevTxs = (PreviousInputBlockTransactionsDigestKey, prevTransactionsDigest)

    ExtensionCandidate(prevInput ++ Seq(txs, prevTxs))
  }
}

case class CandidateBlock(parentOpt: Option[Header],
                          version: Header.Version,
                          nBits: Long,
                          stateRoot: ADDigest,
                          adProofBytes: SerializedAdProof,
                          transactions: Seq[ErgoTransaction],
                          timestamp: Header.Timestamp,
                          extension: ExtensionCandidate,
                          votes: Array[Byte],
                          inputBlockFields: InputBlockFields,
                          inputBlockTransactions: Seq[ErgoTransaction],
                          orderingBlockTransactions: Seq[ErgoTransaction]) {

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
