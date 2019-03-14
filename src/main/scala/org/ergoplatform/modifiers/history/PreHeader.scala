package org.ergoplatform.modifiers.history

import org.ergoplatform.mining.AutolykosPowScheme
import scorex.core.block.Block._
import scorex.util._
import scorex.core.idToBytes
import sigmastate.eval.CGroupElement
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

  def toSigma(preHeader: PreHeader): special.sigma.PreHeader = new special.sigma.PreHeader {
    override def version: Version = preHeader.version

    override def parentId: Coll[Byte] = new CollOverArray(idToBytes(preHeader.parentId))

    override def timestamp: Timestamp = preHeader.timestamp

    override def nBits: Long = preHeader.nBits

    override def height: Int = preHeader.height

    override def minerPk: GroupElement = CGroupElement(preHeader.minerPk)

    override def votes: Coll[Byte] = new CollOverArray(preHeader.votes)
  }

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
