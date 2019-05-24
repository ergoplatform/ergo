package org.ergoplatform.nodeView.state

import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history._
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.storage.modifierprocessors.ExtensionValidator
import org.ergoplatform.settings.ValidationRules._
import org.ergoplatform.settings.{Constants, _}
import org.ergoplatform.wallet.protocol.context.ErgoLikeStateContext
import scorex.core.serialization.{BytesSerializable, ScorexSerializer}
import scorex.core.utils.ScorexEncoding
import scorex.core.validation.{ModifierValidator, ValidationState}
import scorex.crypto.authds.ADDigest
import scorex.util.serialization.{Reader, Writer}
import sigmastate.interpreter.CryptoConstants.EcPointType
import special.collection.{Coll, CollOverArray}

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

/**
  * State context with predicted header.
  * The predicted header only contains fields that can be predicted.
  */
class UpcomingStateContext(lastHeaders: Seq[Header],
                           lastExtensionOpt: Option[Extension],
                           val predictedHeader: PreHeader,
                           genesisStateDigest: ADDigest,
                           currentParameters: Parameters,
                           validationSettings: ErgoValidationSettings,
                           votingData: VotingData)(implicit votingSettings: VotingSettings)
  extends ErgoStateContext(lastHeaders, lastExtensionOpt, genesisStateDigest, currentParameters, validationSettings, votingData)(votingSettings) {

  override def sigmaPreHeader: special.sigma.PreHeader = PreHeader.toSigma(predictedHeader)

  override def sigmaLastHeaders: Coll[special.sigma.Header] = new CollOverArray(lastHeaders.map(h => Header.toSigma(h)).toArray)

  override def toString: String = s"UpcomingStateContext($predictedHeader, $lastHeaders)"

}

/**
  * Additional data required for transactions validation.
  * Script validation requires, that it contain at least a preHeader, so it can only be used
  * for transaction validation if lastHeaders is not empty or in `upcoming` version.
  *
  * @param lastHeaders        - fixed number (10) of last headers
  * @param lastExtensionOpt   - last block extension
  * @param genesisStateDigest - genesis state digest (before the very first block)
  * @param currentParameters  - parameters at the beginning of the current voting epoch
  * @param votingData         - votes for parameters change within the current voting epoch
  */
class ErgoStateContext(val lastHeaders: Seq[Header],
                       val lastExtensionOpt: Option[Extension],
                       val genesisStateDigest: ADDigest,
                       val currentParameters: Parameters,
                       val validationSettings: ErgoValidationSettings,
                       val votingData: VotingData)
                      (implicit val votingSettings: VotingSettings)
  extends ErgoLikeStateContext
    with BytesSerializable
    with ScorexEncoding {

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
               rulesToDisable: Seq[Short],
               version: Byte): ErgoStateContext = {
    val upcomingHeader = PreHeader(lastHeaderOpt, version, minerPk, timestamp, nBits, votes)
    val forkVote = votes.contains(Parameters.SoftFork)
    val height = ErgoHistory.heightOf(lastHeaderOpt)
    val (calculatedParams, disabled) = currentParameters.update(height, forkVote, votingData.epochVotes, rulesToDisable, votingSettings)
    val calculatedValidationSettings = validationSettings.disable(disabled)
    new UpcomingStateContext(lastHeaders, lastExtensionOpt, upcomingHeader, genesisStateDigest, calculatedParams, calculatedValidationSettings, votingData)
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
    * Called at the beginning of the epoch.
    * Extracts parameters and validationSettings from [[Extension]] and compares them to locally calculated once.
    */
  private def processExtension(extension: Extension, header: Header, forkVote: Boolean)
                              (validationState: ValidationState[Unit]): Try[(Parameters, ErgoValidationSettings)] = {
    val height = header.height
    val parsedParamsTry = Parameters.parseExtension(height, extension)
    val parsedValidationTry = ErgoValidationSettings.parseExtension(extension)

    validationState
      .validateNoFailure(exParseParameters, parsedParamsTry)
      .validateNoFailure(exParseValidationSettings, parsedValidationTry)
      .validateTry(parsedParamsTry, e => ModifierValidator.fatal("Failed to parse parameters", e)) {
        case (currentValidationState, parsedParams) =>

          val (calculatedParams, disabled) = currentParameters
            .update(height, forkVote, votingData.epochVotes, parsedParams.rulesToDisable, votingSettings)
          val calculatedSettings = validationSettings.disable(disabled)

          currentValidationState
            .validate(exBlockVersion, calculatedParams.blockVersion == header.version, s"${calculatedParams.blockVersion} == ${header.version}")
            .validateNoFailure(exMatchParameters, Parameters.matchParameters(parsedParams, calculatedParams))
            .validateTry(parsedValidationTry, e => ModifierValidator.fatal("Failed to parse validation settings", e)) {
              case (vs, parsedSettings) =>
                vs.validateNoFailure(exMatchValidationSettings, ErgoValidationSettings.matchSettings(parsedSettings, calculatedSettings))
            }
      }.result.toTry
      .flatMap(_ => parsedParamsTry.flatMap(p => parsedValidationTry.map(vs => (p, vs))))

  }

  def process(header: Header, extension: Extension): Try[ErgoStateContext] = {
    def newHeaders: Seq[Header] = header +: lastHeaders.take(Constants.LastHeadersInContext - 1)


    Try {
      val headerVotes: Array[Byte] = header.votes
      val height = header.height
      val votes = headerVotes.filter(_ != Parameters.NoParameter)
      val epochStarts = header.votingStarts(votingEpochLength)
      val forkVote = votes.contains(Parameters.SoftFork)

      val state = ModifierValidator(validationSettings)
        .validateNoThrow(exCheckForkVote, checkForkVote(height))

      if (epochStarts) {

        processExtension(extension, header, forkVote)(state).map { processed =>
          val params = processed._1
          val extractedValidationSettings = processed._2
          val proposedVotes = votes.map(_ -> 1)
          val newVoting = VotingData(proposedVotes)
          new ErgoStateContext(newHeaders, Some(extension), genesisStateDigest, params, extractedValidationSettings, newVoting)(votingSettings)
        }
      } else {
        val newVotes = votes
        val newVotingResults = newVotes.foldLeft(votingData) { case (v, id) => v.update(id) }
        Success(new ErgoStateContext(newHeaders, Some(extension), genesisStateDigest, currentParameters, validationSettings, newVotingResults)(votingSettings))
      }
    }.flatten
  }


  /**
    * This function verifies whether a full block is valid against the ErgoStateContext instance, and modifies
    * the latter according to the former.
    *
    * @param fb             - block to apply
    * @param votingSettings - chain-wide voting settings
    * @return updated state context or error
    */
  def appendFullBlock(fb: ErgoFullBlock, votingSettings: VotingSettings): Try[ErgoStateContext] = Try {
    new ExtensionValidator(validationSettings)
      .validateExtension(fb.extension, fb.header, lastExtensionOpt, lastHeaderOpt)
      .validateNoThrow(hdrVotes, checkVotes(fb.header))
      .validate(hdrHeight,
        lastHeaderOpt.map(_.height + 1).getOrElse(ErgoHistory.GenesisHeight) == fb.header.height,
        s"${fb.id} => ${lastHeaderOpt.map(_.height)} == ${fb.header.height}")
      .validate(bsBlockTransactionsSize,
        fb.blockTransactions.size <= currentParameters.maxBlockSize,
        s"${fb.id} => ${fb.blockTransactions.size} == ${currentParameters.maxBlockSize}")
      .validate(exSize,
        fb.extension.size <= currentParameters.maxExtensionSize,
        s"${fb.id} => ${fb.extension.size} == ${currentParameters.maxExtensionSize}")
      .result
      .toTry
      .flatMap { _ =>
        process(fb.header, fb.extension)
      }
  }.flatten

  override def toString: String =
    s"ErgoStateContext($currentHeight, ${encoder.encode(previousStateDigest)}, $lastHeaders, $currentParameters)"

  /**
    * Check that non-zero votes extracted from block header are correct
    */
  private def checkVotes(header: Header): Unit = {
    val votes: Array[Byte] = header.votes.filter(_ != Parameters.NoParameter)
    val epochStarts = header.votingStarts(votingSettings.votingLength)
    val votesCount = votes.count(_ != Parameters.SoftFork)
    if (votesCount > Parameters.ParamVotesCount) throw new Error(s"Too many votes $votesCount")

    val prevVotes = mutable.Buffer[Byte]()
    votes.foreach { v =>
      if (prevVotes.contains(v)) throw new Error(s"Double vote in ${votes.mkString}")
      if (prevVotes.contains((-v).toByte)) throw new Error(s"Contradictory votes in ${votes.mkString}")
      if (epochStarts && !Parameters.parametersDescs.contains(v)) throw new Error("Incorrect vote proposed")
      prevVotes += v
    }
  }
}

object ErgoStateContext {

  def empty(constants: StateConstants): ErgoStateContext = {
    empty(constants.settings.chainSettings.genesisStateDigest, constants.settings)
  }

  /**
    * Initialize empty state context with fake PreHeader
    */
  def empty(genesisStateDigest: ADDigest, settings: ErgoSettings): ErgoStateContext = {
    new ErgoStateContext(Seq.empty, None, genesisStateDigest, LaunchParameters, ErgoValidationSettings.initial, VotingData.empty)(settings.chainSettings.voting)
      .upcoming(org.ergoplatform.mining.group.generator, 0L, settings.chainSettings.initialNBits, Array.fill(3)(0.toByte), Seq(), 0.toByte)
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
      Parameters.parseExtension(currentHeader.height, extension).flatMap { params =>
        ErgoValidationSettings.parseExtension(extension).map { validationSettings =>
          new ErgoStateContext(lastHeaders.reverse, Some(extension), genesisStateDigest, params, validationSettings, VotingData.empty)(vs)
        }
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
    ErgoValidationSettingsSerializer.serialize(obj.validationSettings, w)
    w.putUByte(obj.lastExtensionOpt.size)
    obj.lastExtensionOpt.foreach(e => ExtensionSerializer.serialize(e.toExtension(Header.GenesisParentId), w))
  }

  override def parse(r: Reader): ErgoStateContext = {
    val genesisDigest = ADDigest @@ r.getBytes(33)
    val headersLength = r.getUByte()
    val lastHeaders = (1 to headersLength).map(_ => HeaderSerializer.parse(r))
    val votingData = VotingDataSerializer.parse(r)
    val params = ParametersSerializer.parse(r)
    val validationSettings = ErgoValidationSettingsSerializer.parse(r)
    val extensionLength = r.getUByte()
    val lastExtension = (1 to extensionLength).map(_ => ExtensionSerializer.parse(r)).headOption
    new ErgoStateContext(lastHeaders, lastExtension, genesisDigest, params, validationSettings, votingData)(votingSettings)
  }

}
