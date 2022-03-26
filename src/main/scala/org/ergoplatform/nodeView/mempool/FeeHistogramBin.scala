package org.ergoplatform.nodeView.mempool

import io.circe.{Encoder, Json}
import io.circe.syntax._

case class FeeHistogramBin(nTxns: Int, totalFee: Long)

object FeeHistogramBin {

  implicit val encodeHistogramBin: Encoder[FeeHistogramBin] = (bin: FeeHistogramBin) => Json.obj(
    ("nTxns", bin.nTxns.asJson),
    ("totalFee", bin.totalFee.asJson)
  )

}
