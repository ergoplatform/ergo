package org.ergoplatform.mining

import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.settings.Algos
import scorex.core.block.Block.Timestamp
import scorex.core.serialization.JsonSerializable
import scorex.crypto.authds.{ADDigest, SerializedAdProof}

case class CandidateBlock(parentOpt: Option[Header],
                          nBits: Long,
                          stateRoot: ADDigest,
                          adProofBytes: SerializedAdProof,
                          transactions: Seq[AnyoneCanSpendTransaction],
                          timestamp: Timestamp,
                          votes: Array[Byte]) extends JsonSerializable {
  override lazy val json: Json = Map(
    "parentId" -> parentOpt.map(p => Algos.encode(p.id)).getOrElse("None").asJson,
    "nBits" -> nBits.asJson,
    "stateRoot" -> Algos.encode(stateRoot).asJson,
    "adProofBytes" -> Algos.encode(adProofBytes).asJson,
    "timestamp" -> timestamp.asJson,
    "transactions" -> transactions.map(_.json).asJson,
    "transactionsNumber" -> transactions.length.asJson,
    "votes" -> Algos.encode(votes).asJson
  ).asJson

  override def toString = s"CandidateBlock(${json.noSpaces})"
}

