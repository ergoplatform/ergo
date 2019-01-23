package org.ergoplatform.modifiers.history

import org.ergoplatform.mining.AutolykosPowScheme
import scorex.core.block.Block._
import scorex.util._
import sigmastate.interpreter.CryptoConstants.EcPointType

/**
  * Only header fields that can be predicted by a miner
  */
//todo: add votes to the PreHeader?
trait PreHeader {
  val version: Version
  val parentId: ModifierId
  val timestamp: Timestamp
  val nBits: Long
  val height: Int
  val votes: Array[Byte]
  def minerPk: EcPointType
}

object PreHeader {

  def apply(lastHeaderOpt: Option[Header],
            blockVersion: Version,
            pk: EcPointType,
            ts: Long,
            nb: Long,
            v: Array[Byte],
            powScheme: AutolykosPowScheme): PreHeader = {
    val (pId, _, h) = powScheme.derivedHeaderFields(lastHeaderOpt)
    new PreHeader {
      override val version: Version = blockVersion
      override val parentId: ModifierId = pId
      override val timestamp: Timestamp = ts
      override val nBits: Timestamp = nb
      override val height: Int = h
      override val votes: Array[Byte] = v
      override val minerPk: EcPointType = pk
    }
  }

}
