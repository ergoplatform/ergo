package org.ergoplatform.nodeView.state

import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{Extension, Header, HeaderSerializer, PreHeader}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.settings.{Constants, _}
import scorex.core.serialization.{BytesSerializable, ScorexSerializer}
import scorex.core.utils.ScorexEncoding
import scorex.crypto.authds.ADDigest
import scorex.util.serialization.{Reader, Writer}
import sigmastate.interpreter.CryptoConstants.EcPointType
import special.collection.{Coll, CollOverArray}

import scala.util.{Failure, Success, Try}

/**
  * State context with predicted header.
  * The predicted header only contains fields that can be predicted.
  */
class UpcomingStateContext(lastHeaders: Seq[Header],
                           val predictedHeader: PreHeader,
                           genesisStateDigest: ADDigest,
                           currentParameters: Parameters,
                           votingData: VotingData)(implicit votingSettings: VotingSettings)
  extends ErgoStateContext(lastHeaders, genesisStateDigest, currentParameters, votingData)(votingSettings) {

  override def sigmaPreHeader: special.sigma.PreHeader = PreHeader.toSigma(predictedHeader)

  override def sigmaLastHeaders: Coll[special.sigma.Header] = new CollOverArray(lastHeaders.map(h => Header.toSigma(h)).toArray)

  override def toString: String = s"UpcomingStateContext($predictedHeader, $lastHeaders)"

}

/**
  * Additional data required for transactions validation.
  * Script validation requires, that it contain at least a preHeader, so it can only be used
  * for transaction validation if lastHeaders is not empty or in `upcoming` version.
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

  override type M = ErgoStateContext

  def sigmaPreHeader: special.sigma.PreHeader = PreHeader.toSigma(lastHeaders.head)

  def sigmaLastHeaders: Coll[special.sigma.Header] = new CollOverArray(lastHeaders.tail.map(h => Header.toSigma(h)).toArray)

  // todo remove from ErgoLikeContext and from ErgoStateContext
  def lastBlockMinerPk: Array[Byte] = sigmaPreHeader.minerPk.getEncoded.toArray

  // todo remove from ErgoLikeContext and from ErgoStateContext
  // State root hash before the last block
  def previousStateDigest: ADDigest = if (sigmaLastHeaders.toArray.nonEmpty) {
    ADDigest @@ sigmaLastHeaders.toArray.head.stateRoot.digest.toArray
  } else {
    genesisStateDigest
  }

  // todo remove from ErgoLikeContext and from ErgoStateContext
  def currentHeight: Int = Try(sigmaPreHeader.height).getOrElse(ErgoHistory.EmptyHistoryHeight)

  private def votingEpochLength: Int = votingSettings.votingLength

  def lastHeaderOpt: Option[Header] = lastHeaders.headOption

  override def serializer: ScorexSerializer[M] = ErgoStateContextSerializer(votingSettings)

  def upcoming(minerPk: EcPointType,
               timestamp: Long,
               nBits: Long,
               votes: Array[Byte],
               version: Byte): ErgoStateContext = {
    val upcomingHeader = PreHeader(lastHeaderOpt, version, minerPk, timestamp, nBits, votes)
    val forkVote = votes.contains(Parameters.SoftFork)
    val height = ErgoHistory.heightOf(lastHeaderOpt)
    val calculatedParams = currentParameters.update(height, forkVote, votingData.epochVotes, votingSettings)
    new UpcomingStateContext(lastHeaders, upcomingHeader, genesisStateDigest, calculatedParams, votingData)
  }

  protected def checkForkVote(height: Height): Unit = {
    if (currentParameters.softForkStartingHeight.nonEmpty) {
      val startingHeight = currentParameters.softForkStartingHeight.get
      val finishingHeight = startingHeight + votingSettings.votingLength * votingSettings.softForkEpochs
      val afterActivationHeight = finishingHeight + votingSettings.votingLength * (votingSettings.activationEpochs + 1)
      val votesCollected = currentParameters.softForkVotesCollected.get

      if ((height >= finishingHeight && height < finishingHeight + votingEpochLength && !votingSettings.softForkApproved(votesCollected)) ||
        (height >= finishingHeight && height < afterActivationHeight && votingSettings.softForkApproved(votesCollected))) {
        throw new Exception(s"Voting for fork is prohibited at height $height")
      }
    }
  }

  /**
    * Extracts parameters from [[Extension]] and compares them to locally calculated once.
    */
  def processExtension(extension: Extension, header: Header, forkVote: Boolean): Try[Parameters] = {
    val height = header.height

    Parameters.parseExtension(height, extension).flatMap { parsedParams =>
      val calculatedParams = currentParameters.update(height, forkVote, votingData.epochVotes, votingSettings)

      if (calculatedParams.blockVersion != header.version) {
        throw new Exception("Versions in header and parameters section are different")
      }

      Parameters.matchParameters(parsedParams, calculatedParams)
    }
  }

  def process(header: Header, extension: Extension): Try[ErgoStateContext] = process(header, Some(extension))

  def process(header: Header, extensionOpt: Option[Extension]): Try[ErgoStateContext] = Try {

    val headerVotes: Array[Byte] = header.votes
    val height = header.height

    val votes = headerVotes.filter(_ != Parameters.NoParameter)

    val epochStarts = header.votingStarts(votingEpochLength)

    val forkVote = votes.contains(Parameters.SoftFork)

    if (forkVote) checkForkVote(height)

    if (epochStarts) {
      val extractedParams = extensionOpt
        .map(processExtension(_, header, forkVote))
        .getOrElse(Success(currentParameters))

      extractedParams.map { params =>
        val proposedVotes = votes.map(_ -> 1)
        val newVoting = VotingData(proposedVotes)
        new ErgoStateContext(lastHeaders, genesisStateDigest, params, newVoting)(votingSettings)
      }
    } else {
      val newVotes = votes
      val newVotingResults = newVotes.foldLeft(votingData) { case (v, id) => v.update(id) }
      Success(new ErgoStateContext(lastHeaders, genesisStateDigest, currentParameters, newVotingResults)(votingSettings))
    }
  }.flatten


  def appendFullBlock(fullBlock: ErgoFullBlock, votingSettings: VotingSettings): Try[ErgoStateContext] =
    appendBlock(fullBlock.header, Some(fullBlock.extension), votingSettings)

  /**
    * This function verifies whether a full block is valid against the ErgoStateContext instance, and modifies
    * the latter according to the former.
    *
    * @param header         - header of a block
    * @param extensionOpt   - extension section of a block (could be missed then only header data being used for update)
    * @param votingSettings - chain-wide voting settings
    * @return updated state context or error
    */
  def appendBlock(header: Header,
                  extensionOpt: Option[Extension],
                  votingSettings: VotingSettings): Try[ErgoStateContext] = Try {
    val height = header.height
    val expectedHeight = lastHeaderOpt.map(_.height + 1).getOrElse(ErgoHistory.GenesisHeight)

    if (height != expectedHeight) {
      throw new Error(s"Incorrect header ${header.id} height: $height != $expectedHeight")
    }

    process(header, extensionOpt).map { sc =>
      val newHeaders = header +: lastHeaders.take(Constants.LastHeadersInContext - 1)
      sc.updateHeaders(newHeaders)
    }
  }.flatten

  def updateHeaders(newHeaders: Seq[Header]): ErgoStateContext = {
    new ErgoStateContext(newHeaders, genesisStateDigest, currentParameters, votingData)(votingSettings)
  }

  override def toString: String =
    s"ErgoStateContext($currentHeight, ${encoder.encode(previousStateDigest)}, $lastHeaders, $currentParameters)"
}

object ErgoStateContext {

  def empty(constants: StateConstants): ErgoStateContext = {
    empty(constants.settings.chainSettings.genesisStateDigest, constants.settings)
  }

  /**
    * Initialize empty state context with fake PreHeader
    */
  def empty(genesisStateDigest: ADDigest, settings: ErgoSettings): ErgoStateContext = {
    new ErgoStateContext(Seq.empty, genesisStateDigest, LaunchParameters, VotingData.empty)(settings.chainSettings.voting)
      .upcoming(org.ergoplatform.mining.group.generator, 0L, settings.chainSettings.initialNBits, Array.fill(3)(0.toByte), 0.toByte)
  }

  /**
    * Recovers state context at the beginning of the voting epoch.
    */
  def recover(genesisStateDigest: ADDigest,
              extension: Extension,
              lastHeaders: Seq[Header])
             (vs: VotingSettings): Try[ErgoStateContext] = {
    if (lastHeaders.lastOption.exists(_.height % vs.votingLength == 0)) {
      val currentHeader = lastHeaders.last
      Parameters.parseExtension(currentHeader.height, extension).map { params =>
        new ErgoStateContext(lastHeaders.reverse, genesisStateDigest, params, VotingData.empty)(vs)
      }
    } else {
      Failure(new Exception("Context could only be recovered at the start of the voting epoch"))
    }
  }

}

case class ErgoStateContextSerializer(votingSettings: VotingSettings) extends ScorexSerializer[ErgoStateContext] {

  override def serialize(obj: ErgoStateContext, w: Writer): Unit = {
    w.putBytes(obj.genesisStateDigest)
    w.putUByte(obj.lastHeaders.size)
    obj.lastHeaders.foreach(h => HeaderSerializer.serialize(h, w))
    VotingDataSerializer.serialize(obj.votingData, w)
    ParametersSerializer.serialize(obj.currentParameters, w)
  }

  override def parse(r: Reader): ErgoStateContext = {
    val genesisDigest = ADDigest @@ r.getBytes(33)
    val length = r.getUByte()
    val lastHeaders = (1 to length).map(_ => HeaderSerializer.parse(r))
    val votingData = VotingDataSerializer.parse(r)
    val params = ParametersSerializer.parse(r)
    new ErgoStateContext(lastHeaders, genesisDigest, params, votingData)(votingSettings)
  }

}
