package org.ergoplatform.local

import io.circe.{Encoder, Json}
import io.circe.syntax._
import org.ergoplatform.mining.CandidateBlock

case class MiningStatus(isMining: Boolean, candidateBlock: Option[CandidateBlock])


class MiningStatusEncoder(implicit candidateBlockEncoder: Encoder[CandidateBlock])
  extends Encoder[MiningStatus] {

  def apply(r: MiningStatus): Json = {
    Map (
      "isMining" -> r.isMining.asJson,
      "candidateBlock" -> r.candidateBlock.asJson
    ).asJson
  }
}
