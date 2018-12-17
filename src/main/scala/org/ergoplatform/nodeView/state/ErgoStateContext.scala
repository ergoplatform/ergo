package org.ergoplatform.nodeView.state

import com.google.common.primitives.Bytes
import org.bouncycastle.math.ec.ECPoint
import org.ergoplatform.mining.{AutolykosPowScheme, pkToBytes}
import org.ergoplatform.modifiers.history.{Header, HeaderSerializer, PredictedHeader}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.settings.Constants
import scorex.core.serialization.{BytesSerializable, Serializer}
import scorex.core.utils.ScorexEncoding
import scorex.crypto.authds.ADDigest

import scala.util.Try

/**
  * State context with predicted header, that contains fields that are possible to predict
  */
class UpcomingStateContext(lastHeaders: Seq[Header],
                           predictedHeader: PredictedHeader,
                           genesisStateDigest: ADDigest) extends ErgoStateContext(lastHeaders, genesisStateDigest) {
  override val lastBlockMinerPk: Array[Byte] = pkToBytes(predictedHeader.minerPk)

  override val previousStateDigest: ADDigest = lastHeaders.lastOption.map(_.stateRoot).getOrElse(genesisStateDigest)

  override val currentHeight: Int = predictedHeader.height

  override def toString: String = s"UpcomingStateContext($predictedHeader, $lastHeaders)"
}

/**
  * Additional data required for transactions validation
  *
  * @param lastHeaders        - fixed number of last headers
  * @param genesisStateDigest - fixed number of last headers
  */
class ErgoStateContext(val lastHeaders: Seq[Header],
                       val genesisStateDigest: ADDigest)
  extends BytesSerializable with ScorexEncoding {

  val lastBlockMinerPk: Array[Byte] = lastHeaders.headOption.map(_.powSolution.encodedPk)
    .getOrElse(Array.fill(32)(0: Byte))

  // State root hash before the last block
  val previousStateDigest: ADDigest = if (lastHeaders.length >= 2) {
    lastHeaders(1).stateRoot
  } else {
    genesisStateDigest
  }

  def lastHeaderOpt: Option[Header] = lastHeaders.headOption

  val currentHeight: Int = ErgoHistory.heightOf(lastHeaderOpt)

  override type M = ErgoStateContext

  override def serializer: Serializer[M] = ErgoStateContextSerializer

  def rollback(heightTo: Int): ErgoStateContext = {
    val oldHeaders = lastHeaders.filter(_.height <= heightTo)
    ErgoStateContext(oldHeaders, genesisStateDigest)
  }

  def appendHeader(header: Header): ErgoStateContext = {
    ErgoStateContext(header +: lastHeaders.takeRight(Constants.LastHeadersInContext - 1), genesisStateDigest)
  }

  def upcoming(minerPk: ECPoint, timestamp: Long, nBits: Long, powScheme: AutolykosPowScheme): ErgoStateContext = {
    val upcomingHeader = PredictedHeader(lastHeaderOpt, minerPk, timestamp, nBits, powScheme)

    new UpcomingStateContext(lastHeaders, upcomingHeader, genesisStateDigest)
  }

  override def toString: String = s"ErgoStateContext($currentHeight,${encoder.encode(previousStateDigest)}, $lastHeaders)"
}

object ErgoStateContext {

  def empty(genesisStateDigest: ADDigest): ErgoStateContext = {
    new ErgoStateContext(Seq(), genesisStateDigest)
  }

  def apply(header: Header, genesisStateDigest: ADDigest): ErgoStateContext = {
    new ErgoStateContext(Seq(header), genesisStateDigest)
  }

  def apply(lastHeaders: Seq[Header],
            genesisStateDigest: ADDigest): ErgoStateContext = {
    new ErgoStateContext(lastHeaders, genesisStateDigest)
  }
}

object ErgoStateContextSerializer extends Serializer[ErgoStateContext] {

  override def toBytes(obj: ErgoStateContext): Array[Byte] = {
    Bytes.concat(obj.genesisStateDigest,
      scorex.core.utils.concatBytes(obj.lastHeaders.map(_.bytes)))
  }

  override def parseBytes(bytes: Array[Byte]): Try[ErgoStateContext] = Try {
    val genesisDigest = ADDigest @@ bytes.take(33)
    val length = bytes.length

    def loop(offset: Int, acc: Seq[Header]): Seq[Header] = if (offset < length) {
      val header = HeaderSerializer.parseBytes(bytes.slice(offset, bytes.length)).get
      loop(offset + header.bytes.length, header +: acc)
    } else {
      acc.reverse
    }

    ErgoStateContext(loop(offset = 33, Seq.empty), genesisDigest)
  }
}
