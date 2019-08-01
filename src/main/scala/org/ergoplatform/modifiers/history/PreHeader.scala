package org.ergoplatform.modifiers.history

import org.ergoplatform.mining.AutolykosPowScheme
import scorex.core.block.Block._
import scorex.util._
import scorex.core.idToBytes
import sigmastate.eval.{CGroupElement, CPreHeader}
import sigmastate.eval.Extensions._
import sigmastate.interpreter.CryptoConstants.EcPointType
import special.collection.{Coll, CollOverArray}
import special.sigma.GroupElement

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

  def toSigma(preHeader: PreHeader): special.sigma.PreHeader =
    CPreHeader(
      version = preHeader.version,
      parentId = preHeader.parentId.toBytes.toColl,
      timestamp = preHeader.timestamp,
      nBits = preHeader.nBits,
      height = preHeader.height,
      minerPk = CGroupElement(preHeader.minerPk),
      votes = preHeader.votes.toColl
    )

  def apply(lastHeaderOpt: Option[Header],
            blockVersion: Version,
            pk: EcPointType,
            ts: Long,
            nb: Long,
            v: Array[Byte]): PreHeader = {
    val (pId, h) = AutolykosPowScheme.derivedHeaderFields(lastHeaderOpt)
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
