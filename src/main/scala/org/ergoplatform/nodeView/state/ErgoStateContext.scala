package org.ergoplatform.nodeView.state

import com.google.common.primitives.{Bytes, Ints}
import org.ergoplatform.settings._
import org.ergoplatform.mining.{AutolykosPowScheme, pkToBytes}
import org.ergoplatform.modifiers.history.PreHeader
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.settings.Constants
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{Extension, Header, HeaderSerializer}
import org.ergoplatform.nodeView.history.ErgoHistory
import scorex.core.serialization.{BytesSerializable, Serializer}
import scorex.core.utils.ScorexEncoding
import scorex.crypto.authds.ADDigest
import sigmastate.interpreter.CryptoConstants.EcPointType

import scala.collection.mutable
import scala.util.{Success, Try}

case class VotingData(epochVotes: Array[(Byte, Int)]) {
  def update(voteFor: Byte): VotingData = {
    this.copy(epochVotes = epochVotes.map { case (id, votes) =>
      if (id == voteFor) id -> (votes + 1) else id -> votes
    })
  }
}

object VotingDataSerializer extends Serializer[VotingData] {
  override def toBytes(obj: VotingData): Array[Byte] = {
    val votesCount = obj.epochVotes.length.toByte

    val epochVotesBytes =
      if (votesCount > 0) {
        obj.epochVotes.map { case (id, cnt) =>
          id +: Ints.toByteArray(cnt)
        }.reduce(_ ++ _)
      } else {
        Array.emptyByteArray
      }

    votesCount +: epochVotesBytes
  }

  override def parseBytes(bytes: Array[Byte]): Try[VotingData] = Try {
    val votesCount = bytes.head
    val epochVotesBytes = bytes.tail.take(votesCount * 5)
    val epochVotes = epochVotesBytes.grouped(5).toArray.map(bs => bs.head -> Ints.fromByteArray(bs.tail))

    VotingData(epochVotes)
  }
}

object VotingData {
  val empty = VotingData(Array.empty)
}

/**
  * State context with predicted header.
  * The predicted header only contains fields that can be predicted.
  */
class UpcomingStateContext(lastHeaders: Seq[Header],
                           predictedHeader: PreHeader,
                           genesisStateDigest: ADDigest,
                           currentParameters: Parameters,
                           votingData: VotingData)(implicit votingSettings: VotingSettings)
  extends ErgoStateContext(lastHeaders, genesisStateDigest, currentParameters, votingData)(votingSettings) {

  override val lastBlockMinerPk: Array[Byte] = pkToBytes(predictedHeader.minerPk)

  override val previousStateDigest: ADDigest = lastHeaders.lastOption.map(_.stateRoot).getOrElse(genesisStateDigest)

  override val currentHeight: Int = predictedHeader.height

  override def toString: String = s"UpcomingStateContext($predictedHeader, $lastHeaders)"
}

/**
  * Additional data required for transactions validation
  *
  * @param lastHeaders        - fixed number of last headers
  * @param genesisStateDigest - genesis state digest (before the very first block)
  * @param currentParameters  - parameters at the beginning of the current voting epoch
  * @param votingData         - votes for parameters change within the current voting epoch
  */
class ErgoStateContext(val lastHeaders: Seq[Header],
                       val genesisStateDigest: ADDigest,
                       val currentParameters: Parameters,
                       val votingData: VotingData)
                      (implicit val votingSettings: VotingSettings)
  extends BytesSerializable with ScorexEncoding {

  lazy val votingEpochLength: Int = votingSettings.votingLength

  def votingStarts(height: Int): Boolean = height % votingEpochLength == 0 && height > 0

  val lastBlockMinerPk: Array[Byte] = lastHeaders.headOption.map(_.powSolution.encodedPk).getOrElse(Array.fill(32)(0: Byte))

  // State root hash before the last block
  val previousStateDigest: ADDigest = if (lastHeaders.length >= 2) {
    lastHeaders(1).stateRoot
  } else {
    genesisStateDigest
  }

  def lastHeaderOpt: Option[Header] = lastHeaders.headOption

  val currentHeight: Int = ErgoHistory.heightOf(lastHeaderOpt)

  override type M = ErgoStateContext

  override def serializer: Serializer[M] = ErgoStateContextSerializer(votingSettings)

  //Check that non-zero votes extracted from block header are correct
  protected def checkVotes(votes: Array[Byte], epochStarts: Boolean): Unit = {
    if (votes.count(_ != Parameters.SoftFork) > Parameters.ParamVotesCount) throw new Error("Too many votes")

    val prevVotes = mutable.Buffer[Byte]()
    votes.foreach { v =>
      if (prevVotes.contains(v)) throw new Error(s"Double vote in ${votes.mkString}")
      if (prevVotes.contains((-v).toByte)) throw new Error(s"Contradictory votes in ${votes.mkString}")
      if (epochStarts && !Parameters.parametersDescs.contains(v)) throw new Error("Incorrect vote proposed")
      prevVotes += v
    }
  }

  def upcoming(minerPk: EcPointType, timestamp: Long, nBits: Long, powScheme: AutolykosPowScheme): ErgoStateContext = {
    val upcomingHeader = PreHeader(lastHeaderOpt, minerPk, timestamp, nBits, powScheme)
    new UpcomingStateContext(lastHeaders, upcomingHeader, genesisStateDigest, currentParameters, votingData)
  }

  //Check that calculated parameters are matching ones written in the extension section of the block
  private def matchParameters(p1: Parameters, p2: Parameters): Unit = {
    if (p1.parametersTable.size != p2.parametersTable.size) {
      throw new Error(s"Parameters differ in size, p1 = $p1, p2 = $p2")
    }
    p1.parametersTable.foreach { case (k, v) =>
      val v2 = p2.parametersTable(k)
      if (v2 != v) {
        throw new Error(s"Calculated and received parameters differ in parameter $k ($v != $v2)")
      }
    }
  }

  protected def checkForkVote(height: Height): Unit = {
    if (currentParameters.softForkStartingHeight.nonEmpty) {
      val startingHeight = currentParameters.softForkStartingHeight.get
      val finishingHeight = startingHeight + votingSettings.votingLength * votingSettings.softForkEpochs
      val afterActivationHeight = finishingHeight + votingSettings.votingLength * (votingSettings.activationEpochs + 1)
      val votesCollected = currentParameters.softForkVotesCollected.get

      if ((height >= finishingHeight && height < finishingHeight + votingEpochLength && votesCollected <= (finishingHeight - startingHeight) * 9 / 10) ||
        (height >= finishingHeight && height < afterActivationHeight && votesCollected > (finishingHeight - startingHeight) * 9 / 10)) {
        throw new Error(s"Voting for fork is prohibited at height $height")
      }
    }
  }

  def processExtension(extension: Extension,
                       header: Header): Try[ErgoStateContext] = Try {

    val headerVotes: Array[Byte] = header.votes
    val height = header.height

    val votes = headerVotes.filter(_ != Parameters.NoParameter)

    val epochStarts = votingStarts(height)

    checkVotes(votes, epochStarts)

    val forkVote = votes.contains(Parameters.SoftFork)

    if (forkVote) checkForkVote(height)

    if (epochStarts) {
      Parameters.parseExtension(height, extension).flatMap { parsedParams =>
        val calculatedParams = currentParameters.update(height, forkVote, votingData.epochVotes, votingSettings)

        if (calculatedParams.blockVersion != header.version) {
          throw new Error("Versions in header and parameters section are different")
        }

        Try(matchParameters(parsedParams, calculatedParams)).map(_ => calculatedParams)
      }.map { params =>
        val proposedVotes = votes.map(id => id -> 1)
        val newVoting = VotingData(proposedVotes)
        new ErgoStateContext(lastHeaders, genesisStateDigest, params, newVoting)(votingSettings)
      }
    } else {
      val newVotes = votes
      val newVotingResults = newVotes.foldLeft(votingData) { case (v, id) => v.update(id) }
      Success(new ErgoStateContext(lastHeaders, genesisStateDigest, currentParameters, newVotingResults)(votingSettings))
    }
  }.flatten

  /**
    * This function verifies whether a full block is valid against the ErgoStateContext instance, and modifies
    * the latter according to the former.
    *
    * @param fullBlock      - full block (transactions, extension section, maybe state transformation proofs)
    * @param votingSettings - chain-wide voting settings
    * @return
    */
  def appendFullBlock(fullBlock: ErgoFullBlock, votingSettings: VotingSettings): Try[ErgoStateContext] = Try {
    val header = fullBlock.header
    val height = header.height

    if (height != lastHeaderOpt.map(_.height + 1).getOrElse(ErgoHistory.GenesisHeight)) {
      throw new Error(s"Improper block applied: $fullBlock to state context $this")
    }

    processExtension(fullBlock.extension, header).map { sc =>
      val newHeaders = header +: lastHeaders.takeRight(Constants.LastHeadersInContext - 1)
      sc.updateHeaders(newHeaders)
    }
  }.flatten

  def updateHeaders(newHeaders: Seq[Header]): ErgoStateContext = {
    new ErgoStateContext(newHeaders, genesisStateDigest, currentParameters, votingData)(votingSettings)
  }

  override def toString: String = s"ErgoStateContext($currentHeight,${encoder.encode(previousStateDigest)}, $lastHeaders, $currentParameters)"
}

object ErgoStateContext {
  def empty(constants: StateConstants): ErgoStateContext = {
    implicit val votingSettings: VotingSettings = constants.votingSettings
    new ErgoStateContext(Seq.empty, constants.genesisStateDigest, LaunchParameters, VotingData.empty)
  }

  def empty(genesisStateDigest: ADDigest, votingSettings: VotingSettings): ErgoStateContext = {
    new ErgoStateContext(Seq.empty, genesisStateDigest, LaunchParameters, VotingData.empty)(votingSettings)
  }
}

case class ErgoStateContextSerializer(votingSettings: VotingSettings) extends Serializer[ErgoStateContext] {

  override def toBytes(ctx: ErgoStateContext): Array[Byte] = {
    val lastHeaderBytes = scorex.core.utils.concatBytes(ctx.lastHeaders.map(_.bytes))

    val votingDataBytes = VotingDataSerializer.toBytes(ctx.votingData)
    val votingDataSize = Ints.toByteArray(votingDataBytes.length)

    Bytes.concat(
      ctx.genesisStateDigest,
      Ints.toByteArray(lastHeaderBytes.length),
      lastHeaderBytes,
      votingDataSize,
      votingDataBytes,
      ParametersSerializer.toBytes(ctx.currentParameters))
  }

  override def parseBytes(bytes: Array[Byte]): Try[ErgoStateContext] = Try {
    val genesisDigest = ADDigest @@ bytes.take(33)
    val lastHeaderBytesLength = Ints.fromByteArray(bytes.slice(33, 37))

    def loop(bytes: Array[Byte], offset: Int, acc: Seq[Header]): Seq[Header] =
      if (offset < lastHeaderBytesLength) {
        val header = HeaderSerializer.parseBytes(bytes.slice(offset, bytes.length)).get
        loop(bytes, offset + header.bytes.length, header +: acc)
      } else {
        acc.reverse
      }

    val afterHeaders = 37 + lastHeaderBytesLength
    val lastHeaderBytes = bytes.slice(37, afterHeaders)
    val lastHeaders = loop(lastHeaderBytes, 0, Seq.empty)

    val votingDataSize = Ints.fromByteArray(bytes.slice(afterHeaders, afterHeaders + 4))

    val afterVoting = afterHeaders + 4 + votingDataSize
    VotingDataSerializer.parseBytes(bytes.slice(afterHeaders + 4, afterVoting)).flatMap { votingData =>
      ParametersSerializer.parseBytes(bytes.slice(afterVoting, bytes.length)).map { params =>
        new ErgoStateContext(lastHeaders, genesisDigest, params, votingData)(votingSettings)
      }
    }
  }.flatten

}
