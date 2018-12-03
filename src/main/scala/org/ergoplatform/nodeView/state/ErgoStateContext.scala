package org.ergoplatform.nodeView.state

import com.google.common.primitives.Ints
import org.ergoplatform.settings.{LaunchParameters, Parameters, ParametersSerializer}
import com.google.common.primitives.Bytes
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{Extension, Header, HeaderSerializer}
import org.ergoplatform.settings.Constants
import scorex.core.serialization.{BytesSerializable, Serializer}
import scorex.core.utils.ScorexEncoding
import scorex.crypto.authds.ADDigest

import scala.util.{Success, Try}

case class VotingResults(results: Array[(Byte, Int)]) {
  def update(voteFor: Byte): VotingResults = {
    VotingResults(results.map { case (id, votes) =>
      if (id == voteFor) id -> (votes + 1) else id -> votes
    })
  }
}

object VotingResults {
  val empty = VotingResults(Array.empty)
}

/**
  * Additional data required for transactions validation
  *
  * @param lastHeaders        - fixed number of last headers
  * @param genesisStateDigest - genesis state digest (before the very first block)
  * @param currentParameters  - parameters at the beginning of the current voting epoch
  * @param currentVoting      - votes for parameters change within the current voting epoch
  */
case class ErgoStateContext(lastHeaders: Seq[Header],
                            genesisStateDigest: ADDigest,
                            currentParameters: Parameters,
                            currentVoting: VotingResults
                            //,softForkVotingStartingHeight: Int,
                            //softForkVotesCollected: Int,
                            //activationHeight: Int
                           )
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


  def processExtension(extension: Extension,
                       headerVotes: Array[Byte],
                       height: Int,
                       votingEpochLength: Int): Try[ErgoStateContext] = Try {
    def votingStarts(height: Int) = height % votingEpochLength == 0 && height > 0

    //Check that votes extracted from block header are correct
    def checkVotes(votes: Array[Byte]): Unit = {
      if (votes.distinct.length != votes.length) throw new Error(s"Double vote in ${votes.mkString}")
      if (votes.count(_ != Parameters.SoftFork) > Parameters.ParamVotesCount) throw new Error("Too many votes")
      //votes with id = 121..127 are prohibited
      if (votes.exists(_ > Parameters.SoftFork)) throw new Error(s"Invalid vote in $votes")
    }

    //Check that calculated parameters are matching ones written in the extension section of the block
    def matchParameters(p1: Parameters, p2: Parameters, softForkStarts: Boolean): Unit = {
      if (p1.parametersTable.size != p2.parametersTable.size) {
        throw new Error("Calculated and received parameters differ in size")
      }
      p1.parametersTable.foreach { case (k, v) =>
        if (p2.parametersTable(k) != v) throw new Error(s"Calculated and received parameters differ in parameter $k")
      }
    }

    //genesis block does not contain votes
    //todo: this rule may be reconsidered when moving interlink vector to extension section
    if (height == 0 && extension.mandatoryFields.nonEmpty) {
      throw new Error("Mandatory fields in genesis block")
    }

    val votes = headerVotes.filter(_ != Parameters.NoParameter)

    checkVotes(votes)

    if (votingStarts(height)) {
      val proposedVotes = votes.map(id => id -> 1)
      val newVoting = VotingResults(proposedVotes)

      val softForkStarts = votes.contains(Parameters.SoftFork)

      Parameters.parseExtension(height, extension).flatMap { parsedParams =>
        val calculatedParams = currentParameters.update(height, currentVoting.results, votingEpochLength)
        Try(matchParameters(parsedParams, calculatedParams, softForkStarts)).map(_ => calculatedParams)
      }.map { params =>
        ErgoStateContext(lastHeaders, genesisStateDigest, params, newVoting)
      }
    } else {
      val newVotes = votes
      val newVotingResults = newVotes.foldLeft(currentVoting) { case (v, id) => v.update(id) }
      Success(ErgoStateContext(lastHeaders, genesisStateDigest, currentParameters, newVotingResults))
    }
  }.flatten

  /**
    * This function verifies whether a full block is valid against the ErgoStateContext instance, and modifies
    * the latter according to the former.
    *
    * @param fullBlock         - full block (transactions, extension section, maybe state transformation proofs)
    * @param votingEpochLength - length of voting epoch (system constant)
    * @return
    */
  def appendFullBlock(fullBlock: ErgoFullBlock, votingEpochLength: Int): Try[ErgoStateContext] = Try {
    val header = fullBlock.header
    val height = header.height

    processExtension(fullBlock.extension, header.votes, height, votingEpochLength).map { sc =>
      val newHeaders = header +: lastHeaders.takeRight(Constants.LastHeadersInContext - 1)
      sc.copy(lastHeaders = newHeaders)
    }
  }.flatten

  override def toString: String = s"ErgoStateContext($currentHeight,${encoder.encode(previousStateDigest)}, $lastHeaders, $currentParameters)"
}

object ErgoStateContext {
  def empty(genesisStateDigest: ADDigest): ErgoStateContext = {
    ErgoStateContext(Seq.empty, genesisStateDigest, LaunchParameters, VotingResults.empty)
  }
}

object ErgoStateContextSerializer extends Serializer[ErgoStateContext] {

  override def toBytes(ctx: ErgoStateContext): Array[Byte] = {
    val lastHeaderBytes = scorex.core.utils.concatBytes(ctx.lastHeaders.map(_.bytes))
    val votesCount = ctx.currentVoting.results.length.toByte

    val votesBytes = if (votesCount > 0) {
      ctx.currentVoting.results.map { case (id, cnt) =>
        id +: Ints.toByteArray(cnt)
      }.reduce(_ ++ _)
    } else {
      Array.emptyByteArray
    }

    Bytes.concat(
      ctx.genesisStateDigest,
      Ints.toByteArray(lastHeaderBytes.length),
      lastHeaderBytes,
      Array(votesCount),
      votesBytes,
      ParametersSerializer.toBytes(ctx.currentParameters))
  }

  override def parseBytes(bytes: Array[Byte]): Try[ErgoStateContext] = Try {
    val genesisDigest = ADDigest @@ bytes.take(33)
    val length = Ints.fromByteArray(bytes.slice(33, 37))

    def loop(startPos: Int, finishPos: Int, acc: Seq[Header]): Seq[Header] = if (startPos < length) {
      // todo use only required bytes when header size will be fixed after https://github.com/ergoplatform/ergo/issues/452
      val header = HeaderSerializer.parseBytes(bytes.slice(startPos, finishPos)).get
      loop(startPos + header.bytes.length, finishPos, header +: acc)
    } else {
      acc.reverse
    }

    val votesCount = bytes(37 + length)

    val (votes: VotingResults, votesLength: Int) = if (votesCount > 0) {
      val vl = votesCount * 5
      val votesBytes = bytes.slice(37 + length + 1, 37 + length + 1 + vl)
      VotingResults(votesBytes.grouped(5).map { bs =>
        bs.head -> Ints.fromByteArray(bs.tail)
      }.toArray) -> vl
    } else {
      VotingResults.empty -> 0
    }

    ParametersSerializer.parseBytes(bytes.slice(37 + length + 1 + votesLength, bytes.length)).map { params =>
      //todo: fix
      val lastHeaders = loop(startPos = 37, 37 + length, Seq.empty)
      ErgoStateContext(lastHeaders, genesisDigest, params, votes)
    }
  }.flatten
}
