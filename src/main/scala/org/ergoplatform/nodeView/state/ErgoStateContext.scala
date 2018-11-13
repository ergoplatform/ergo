package org.ergoplatform.nodeView.state

import com.google.common.primitives.Ints
import org.ergoplatform.settings.{LaunchParameters, Parameters, ParametersSerializer}
import com.google.common.primitives.Bytes
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{Header, HeaderSerializer}
import org.ergoplatform.settings.Constants
import scorex.core.serialization.{BytesSerializable, Serializer}
import scorex.core.utils.ScorexEncoding
import scorex.crypto.authds.ADDigest

import scala.util.{Success, Try}

case class VotingResults(results: Seq[(Byte, Int)]){
  def update(voteFor: Byte): VotingResults = {
    VotingResults(results.map{case (id, votes) =>
      if(id == voteFor) id -> (votes + 1) else id -> votes
    })
  }
}

object VotingResults {
  val empty = VotingResults(Seq())
}

/**
  * Additional data required for transactions validation
  *
  * @param lastHeaders - fixed number of last headers
  * @param genesisStateDigest - fixed number of last headers
  */
case class ErgoStateContext(lastHeaders: Seq[Header],
                            genesisStateDigest: ADDigest,
                            currentParameters: Parameters,
                            currentVoting: VotingResults)
  extends BytesSerializable with ScorexEncoding {

  // State root hash before the last block
  val previousStateDigest: ADDigest = if (lastHeaders.length >= 2) {
    lastHeaders(1).stateRoot
  } else {
    genesisStateDigest
  }

  def lastHeaderOpt: Option[Header] = lastHeaders.headOption

  // TODO it should be -1 by default, see https://github.com/ergoplatform/ergo/issues/546
  val currentHeight: Int = lastHeaderOpt.map(_.height).getOrElse(0)

  override type M = ErgoStateContext

  override def serializer: Serializer[M] = ErgoStateContextSerializer

  def appendFullBlock(fullBlock: ErgoFullBlock, votingStart: Boolean): Try[ErgoStateContext] = {
    val header = fullBlock.header
    val newHeaders = header +: lastHeaders.takeRight(Constants.LastHeadersInContext - 1)

    if(votingStart) {
      val extension = fullBlock.extension

      val newVoting = VotingResults(
        header.votes.filter(_ != Parameters.NoParameter).map(id => id -> 1)
      )

      Parameters.parseExtension(extension.height, extension).map { params =>
        ErgoStateContext(newHeaders, genesisStateDigest, params, newVoting)
      }
    } else {
      val newVotes = header.votes.filter(_ != Parameters.NoParameter).foldLeft(currentVoting){case (v, id) =>
        v.update(id)
      }
      Success(ErgoStateContext(newHeaders, genesisStateDigest, currentParameters, newVotes))
    }
  }

  override def toString: String = s"ErgoStateContext($currentHeight,${encoder.encode(previousStateDigest)}, $lastHeaders, $currentParameters)"
}

object ErgoStateContext {
  def empty(genesisStateDigest: ADDigest): ErgoStateContext = {
    ErgoStateContext(Seq(), genesisStateDigest, LaunchParameters, VotingResults(Seq()))
  }
}

object ErgoStateContextSerializer extends Serializer[ErgoStateContext] {

  override def toBytes(ctx: ErgoStateContext): Array[Byte] = {
    val lastHeaderBytes = scorex.core.utils.concatBytes(ctx.lastHeaders.map(_.bytes))

    Bytes.concat(
      ctx.genesisStateDigest,
      Ints.toByteArray(lastHeaderBytes.length),
      lastHeaderBytes,
      ParametersSerializer.toBytes(ctx.currentParameters))
  }

  override def parseBytes(bytes: Array[Byte]): Try[ErgoStateContext] = Try {
    val genesisDigest = ADDigest @@ bytes.take(33)
    val length =  Ints.fromByteArray(bytes.slice(33, 37))

    def loop(startPos: Int, finishPos: Int, acc: Seq[Header]): Seq[Header] = if (startPos < length) {
      // todo use only required bytes when header size will be fixed after https://github.com/ergoplatform/ergo/issues/452
      val header = HeaderSerializer.parseBytes(bytes.slice(startPos, finishPos)).get
      loop(startPos + header.bytes.length, finishPos, header +: acc)
    } else {
      acc.reverse
    }

    ParametersSerializer.parseBytes(bytes.slice(37 + length, bytes.length)).map{params =>
      //todo: fix
      ErgoStateContext(loop(startPos = 37, 37 + length, Seq.empty), genesisDigest, params, VotingResults.empty)
    }
  }.flatten
}
