package org.ergoplatform.modifiers.history

import org.ergoplatform.mining.AutolykosPowScheme
import scorex.core.block.Block._
import scorex.util._
import sigmastate.interpreter.CryptoConstants.EcPointType

/**
  * Only header fields that can be predicted by a miner
  */
trait PreHeader {
  val version: Version
  val parentId: ModifierId
  val timestamp: Timestamp
  val nBits: Long
  val height: Int
  val minerPk: EcPointType
}

object PreHeader {

  def apply(lastHeaderOpt: Option[Header],
            pk: EcPointType,
            ts: Long,
            nb: Long,
            powScheme: AutolykosPowScheme): PreHeader = {
    val (pId, v, _, h) = powScheme.derivedHeaderFields(lastHeaderOpt)
    new PreHeader {
      override val version: Version = v
      override val parentId: ModifierId = pId
      override val timestamp: Timestamp = ts
      override val nBits: Timestamp = nb
      override val height: Int = h
      override val minerPk: EcPointType = pk
    }
  }

}
