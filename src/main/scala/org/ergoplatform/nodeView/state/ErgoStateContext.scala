package org.ergoplatform.nodeView.state

import com.google.common.primitives.Ints
import org.ergoplatform.settings._
import com.google.common.primitives.Bytes
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.settings.Constants
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{Extension, Header, HeaderSerializer}
import org.ergoplatform.nodeView.history.ErgoHistory
import scorex.core.serialization.{BytesSerializable, Serializer}
import scorex.core.utils.ScorexEncoding
import scorex.crypto.authds.ADDigest

import scala.collection.mutable
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

case class VotingData(currentVoting: VotingResults,
                      softForkVotingStartingHeight: Int = 0,
                      softForkVotesCollected: Int = 0,
                      activationHeight: Int = 0)

object VotingData {
  val empty = VotingData(VotingResults.empty)
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

  lazy val lastBlockMinerPk: Array[Byte] = lastHeaders.headOption.map(_.powSolution.encodedPk)
    .getOrElse(Array.fill(32)(0: Byte))

  lazy val currentVoting: VotingResults = votingData.currentVoting

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

  def processExtension(extension: Extension,
                       header: Header): Try[ErgoStateContext] = Try {

    val headerVotes: Array[Byte] = header.votes
    val height = header.height

    //genesis block does not contain votes
    //todo: this rule may be reconsidered when moving interlink vector to extension section
    if (height == 0 && extension.mandatoryFields.nonEmpty) {
      throw new Error("Mandatory fields in genesis block")
    }

    val votes = headerVotes.filter(_ != Parameters.NoParameter)

    val epochStarts = votingStarts(height)

    checkVotes(votes, epochStarts)

    def checkForkStart(height: Height): Unit = {
      if (currentParameters.softForkStartingHeight.nonEmpty) {
        throw new Error("Previous fork has not been activated yet")
      }
    }

    if (epochStarts) {
      val proposedVotes = votes.map(id => id -> 1)
      val newVoting = VotingData(VotingResults(proposedVotes))

      val softForkStarts = votes.contains(Parameters.SoftFork)

      //todo: fix
//      if (softForkStarts) checkForkStart(height)

      Parameters.parseExtension(height, extension).flatMap { parsedParams =>
        val calculatedParams = currentParameters.update(height, softForkStarts, currentVoting.results, votingSettings)

        if (currentParameters.blockVersion != header.version) {
          throw new Error("Versions in header and parameters section are different")
        }

        Try(matchParameters(parsedParams, calculatedParams)).map(_ => calculatedParams)
      }.map { params =>
        new ErgoStateContext(lastHeaders, genesisStateDigest, params, newVoting)(votingSettings)
      }
    } else {
      val newVotes = votes
      val newVotingResults = newVotes.foldLeft(currentVoting) { case (v, id) => v.update(id) }
      Success(new ErgoStateContext(lastHeaders, genesisStateDigest, currentParameters, VotingData(newVotingResults))(votingSettings))
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
    new ErgoStateContext(Seq.empty, constants.genesisStateDigest, LaunchParameters, VotingData(VotingResults.empty))
  }

  def empty(genesisStateDigest: ADDigest, votingSettings: VotingSettings): ErgoStateContext = {
    new ErgoStateContext(Seq.empty, genesisStateDigest, LaunchParameters, VotingData(VotingResults.empty))(votingSettings)
  }
}

case class ErgoStateContextSerializer(votingSettings: VotingSettings) extends Serializer[ErgoStateContext] {

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

    def loop(offset: Int, acc: Seq[Header]): Seq[Header] = if (offset < length) {
      val header = HeaderSerializer.parseBytes(bytes.slice(offset, bytes.length)).get
      loop(offset + header.bytes.length, header +: acc)
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
      val lastHeaders = loop(offset = 37, Seq.empty)
      new ErgoStateContext(lastHeaders, genesisDigest, params, VotingData(votes))(votingSettings)
    }
  }.flatten

}
