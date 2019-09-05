package org.ergoplatform.nodeView.state

import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history._
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.components.ExtensionValidator
import org.ergoplatform.settings.ValidationRules._
import org.ergoplatform.settings.{Constants, _}
import org.ergoplatform.wallet.protocol.context.ErgoLikeStateContext
import scorex.core.serialization.{BytesSerializable, ScorexSerializer}
import scorex.core.utils.ScorexEncoding
import scorex.core.validation.{ModifierValidator, TaggedValidationState, ValidationState}
import scorex.crypto.authds.ADDigest
import scorex.util.serialization.{Reader, Writer}
import sigmastate.interpreter.CryptoConstants.EcPointType
import special.collection.{Coll, CollOverArray}

import scala.util.{Failure, Try}

/**
  * State context with predicted header.
  * The predicted header only contains fields that can be predicted.
  */
case class UpcomingStateContext(override val lastHeaders: Seq[Header],
                                override val lastExtensionOpt: Option[Extension],
                                predictedHeader: PreHeader,
                                override val genesisStateDigest: ADDigest,
                                override val currentParameters: Parameters,
                                override val validationSettings: ErgoValidationRules,
                                override val votingData: VotingData)(implicit votingSettings: VotingSettings)
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
                       val validationSettings: ErgoValidationRules,
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
               proposedUpdate: ErgoValidationSettingsUpdate,
               version: Byte): UpcomingStateContext = {
    val upcomingHeader = PreHeader(lastHeaderOpt, version, minerPk, timestamp, nBits, votes)
    val forkVote = votes.contains(Parameters.SoftFork)
    val height = ErgoHistory.heightOf(lastHeaderOpt) + 1
    val (calculatedParams, updated) = currentParameters.update(height, forkVote, votingData.epochVotes, proposedUpdate, votingSettings)
    val calculatedValidationSettings = validationSettings.updated(updated)
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
    * Extracts parameters and validationSettings from `Extension` and compares them to locally calculated once.
    */
  private def processExtension(extension: Extension, header: Header, forkVote: Boolean)
                              (validationState: TaggedValidationState[Unit]): Try[(Parameters, ErgoValidationRules)] = {
    val height = header.height
    val parsedParamsTry = Parameters.parseExtension(height, extension)
    val parsedValidationTry = ErgoValidationRules.parseExtension(extension)

    validationState
      .validateNoFailure(exParseParameters, parsedParamsTry)
      .validateNoFailure(exParseValidationSettings, parsedValidationTry)
      .validateTry(parsedParamsTry, e => ModifierValidator.fatal("Failed to parse parameters", e)) {
        case (currentValidationState, parsedParams) =>

          val (calculatedParams, disabled) = currentParameters
            .update(height, forkVote, votingData.epochVotes, parsedParams.proposedUpdate, votingSettings)
          val calculatedSettings = validationSettings.updated(disabled)

          currentValidationState
            .validate(exBlockVersion, calculatedParams.blockVersion == header.version, s"${calculatedParams.blockVersion} == ${header.version}")
            .validateNoFailure(exMatchParameters, Parameters.matchParameters(parsedParams, calculatedParams))
            .validateTry(parsedValidationTry, e => ModifierValidator.fatal("Failed to parse validation settings", e)) {
              case (vs, parsedSettings) =>
                vs.validate(exMatchValidationSettings, parsedSettings == calculatedSettings, s"$parsedSettings vs $calculatedSettings")
            }
      }.result.toTry
      .flatMap(_ => parsedParamsTry.flatMap(p => parsedValidationTry.map(vs => (p, vs))))

  }

  def process(header: Header, extensionOpt: Option[Extension]): Try[ErgoStateContext] = {
    def newHeaders: Seq[Header] = header +: lastHeaders.take(Constants.LastHeadersInContext - 1)

    Try {
      val headerVotes: Array[Byte] = header.votes
      val height = header.height
      val votes = headerVotes.filter(_ != Parameters.NoParameter)
      val epochStarts = header.votingStarts(votingEpochLength)
      val forkVote = votes.contains(Parameters.SoftFork)

      val state = ModifierValidator.failFastTagged(validationSettings)
        .validateNoThrow(exCheckForkVote, if (forkVote) checkForkVote(height))

      extensionOpt match {
        case Some(extension) if epochStarts =>
          processExtension(extension, header, forkVote)(state).map { processed =>
            val params = processed._1
            val extractedValidationSettings = processed._2
            val proposedVotes = votes.map(_ -> 1)
            val newVoting = VotingData(proposedVotes)
            new ErgoStateContext(newHeaders, extensionOpt, genesisStateDigest, params, extractedValidationSettings, newVoting)(votingSettings)
          }
        case _ =>
          val newVotes = votes
          val newVotingResults = newVotes.foldLeft(votingData) { case (v, id) => v.update(id) }
          state.result.toTry.map { _ =>
            new ErgoStateContext(newHeaders, extensionOpt, genesisStateDigest, currentParameters, validationSettings, newVotingResults)(votingSettings)
          }
      }
    }.flatten
  }

  def appendHeader(header: Header, votingSettings: VotingSettings): Try[ErgoStateContext] = {
    validateVotes(header)
      .validate(hdrHeight,
        lastHeaderOpt.map(_.height + 1).getOrElse(ErgoHistory.GenesisHeight) == header.height,
        s"${header.id} => ${lastHeaderOpt.map(_.height)} == ${header.height}")
      .result
      .toTry
      .flatMap { _ =>
        process(header, None)
      }
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
    val votesValidationState = validateVotes(fb.header)
    new ExtensionValidator(votesValidationState)
      .validateExtension(fb.extension, fb.header, lastExtensionOpt, lastHeaderOpt)
      .validate(hdrHeight,
        lastHeaderOpt.map(_.height + 1).getOrElse(ErgoHistory.GenesisHeight) == fb.header.height,
        s"${fb.id} => ${lastHeaderOpt.map(_.height)} == ${fb.header.height}")
      .validate(bsBlockTransactionsSize,
        fb.blockTransactions.size <= currentParameters.maxBlockSize,
        s"${fb.id} => ${fb.blockTransactions.size} == ${currentParameters.maxBlockSize}")
      .validate(exSize,
        fb.extension.size <= Constants.MaxExtensionSize,
        s"${fb.id} => ${fb.extension.size} == ${Constants.MaxExtensionSize}")
      .result
      .toTry
      .flatMap { _ =>
        process(fb.header, Some(fb.extension))
      }
  }.flatten

  override def toString: String =
    s"ErgoStateContext($currentHeight, ${encoder.encode(previousStateDigest)}, $lastHeaders, $currentParameters)"


  /**
    * Check that non-zero votes extracted from block header are correct
    */
  private def validateVotes(header: Header): TaggedValidationState[ErgoStateContext] = {
    val votes: Array[Byte] = header.votes.filter(_ != Parameters.NoParameter)
    val epochStarts = header.votingStarts(votingSettings.votingLength)
    val votesCount = votes.count(_ != Parameters.SoftFork)
    val reverseVotes: Array[Byte] = votes.map(v => (-v).toByte)

    @inline def vs: String = votes.mkString("")

    ModifierValidator.failFastTagged(validationSettings)
      .payload(this)
      .validate(hdrVotesNumber, votesCount <= Parameters.ParamVotesCount, s"votesCount=$votesCount")
      .validateSeq(votes) { case (validationState, v) =>
        validationState
          .validate(hdrVotesDuplicates, votes.count(_ == v) == 1, s"Double vote in $vs")
          .validate(hdrVotesContradictory, !reverseVotes.contains(v), s"Contradictory votes in $vs")
          .validate(hdrVotesUnknown, !(epochStarts && !Parameters.parametersDescs.contains(v)), s"Incorrect vote proposed in $vs")
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
    new ErgoStateContext(Seq.empty, None, genesisStateDigest, LaunchParameters, ErgoValidationRules.initial, VotingData.empty)(settings.chainSettings.voting)
      .upcoming(org.ergoplatform.mining.group.generator, 0L, settings.chainSettings.initialNBits, Array.fill(3)(0.toByte),
        ErgoValidationSettingsUpdate.empty, 0.toByte)
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
        ErgoValidationRules.parseExtension(extension).map { validationSettings =>
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
