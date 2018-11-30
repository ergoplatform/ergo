package org.ergoplatform.nodeView.state

import org.ergoplatform.modifiers.history.{Header, HeaderSerializer}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.settings.Constants
import scorex.core.serialization.ScorexSerializer
import scorex.core.utils.ScorexEncoding
import scorex.crypto.authds.ADDigest
import scorex.util.serialization.{Reader, Writer}

/**
  * Additional data required for transactions validation
  *
  * @param lastHeaders - fixed number of last headers
  * @param genesisStateDigest - fixed number of last headers
  */
case class ErgoStateContext(lastHeaders: Seq[Header], genesisStateDigest: ADDigest)
  extends ScorexEncoding {

  lazy val lastBlockMinerPk: Array[Byte] = lastHeaders.headOption.map(_.powSolution.encodedPk)
    .getOrElse(Array.fill(32)(0: Byte))

  // State root hash before the last block
  val previousStateDigest: ADDigest = if (lastHeaders.length >= 2) {
    lastHeaders(1).stateRoot
  } else {
    genesisStateDigest
  }

  def lastHeaderOpt: Option[Header] = lastHeaders.headOption

  val currentHeight: Int = ErgoHistory.heightOf(lastHeaderOpt)

  def appendHeader(header: Header): ErgoStateContext = {
    ErgoStateContext(header +: lastHeaders.takeRight(Constants.LastHeadersInContext - 1), genesisStateDigest)
  }

  override def toString: String = s"ErgoStateContext($currentHeight,${encoder.encode(previousStateDigest)}, $lastHeaders)"
}

object ErgoStateContext {

  def empty(genesisStateDigest: ADDigest): ErgoStateContext = {
    ErgoStateContext(Seq(), genesisStateDigest)
  }

  def apply(header: Header, genesisStateDigest: ADDigest): ErgoStateContext = {
    ErgoStateContext(Seq(header), genesisStateDigest)
  }
}

object ErgoStateContextSerializer extends ScorexSerializer[ErgoStateContext] {

  override def serialize(obj: ErgoStateContext, w: Writer): Unit = {
    w.putBytes(obj.genesisStateDigest)
    w.putInt(obj.lastHeaders.size)
    obj.lastHeaders.foreach(h => HeaderSerializer.serialize(h, w))
  }

  override def parse(r: Reader): ErgoStateContext = {
    val genesisDigest = ADDigest @@ r.getBytes(33)
    val length = r.getInt()
    val lastHeaders = (1 to length).map(_ => HeaderSerializer.parse(r))
    ErgoStateContext(lastHeaders, genesisDigest)
  }
}
