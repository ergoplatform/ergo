package org.ergoplatform.modifiers.history

import org.bouncycastle.math.ec.ECPoint
import org.ergoplatform.mining.AutolykosPowScheme
import scorex.core.block.Block._
import scorex.util._

/**
  * Only header fields which can be predicted by a miner
  */
trait PredictedHeader {
  val version: Version
  val parentId: ModifierId
  val timestamp: Timestamp
  val nBits: Long
  val height: Int
  val minerPk: ECPoint
}

object PredictedHeader {

  def apply(lastHeaderOpt: Option[Header],
            pk: ECPoint,
            ts: Long,
            nb: Long,
            powScheme: AutolykosPowScheme): PredictedHeader = {
    val (pId, v, _, h) = powScheme.derivedHeaderFields(lastHeaderOpt)
    new PredictedHeader {
      override val version: Version = v
      override val parentId: ModifierId = pId
      override val timestamp: Timestamp = ts
      override val nBits: Timestamp = nb
      override val height: Int = h
      override val minerPk: ECPoint = pk
    }
  }

}