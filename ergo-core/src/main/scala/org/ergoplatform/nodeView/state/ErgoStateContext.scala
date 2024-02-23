package org.ergoplatform.nodeView.state

import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.core.BytesSerializable
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.history.extension.{Extension, ExtensionSerializer}
import org.ergoplatform.modifiers.history.header.{Header, HeaderSerializer}
import org.ergoplatform.modifiers.history.popow.NipopowAlgos
import org.ergoplatform.nodeView.history.ErgoHistoryUtils
import org.ergoplatform.nodeView.history.storage.modifierprocessors.ExtensionValidator
import org.ergoplatform.sdk.wallet.protocol.context.BlockchainStateContext
import org.ergoplatform.settings.ValidationRules._
import org.ergoplatform.settings._
import org.ergoplatform.utils.ScorexEncoding
import org.ergoplatform.serialization.ErgoSerializer
import org.ergoplatform.validation.{InvalidModifier, ModifierValidator, ValidationState}
import scorex.crypto.authds.ADDigest
import scorex.util.ScorexLogging
import scorex.util.serialization.{Reader, Writer}
import sigmastate.crypto.CryptoConstants.EcPointType
import sigmastate.eval.Extensions.ArrayOps
import sigmastate.eval.SigmaDsl
import sigma.Coll

import scala.collection.compat.immutable.ArraySeq
import scala.util.{Failure, Success, Try}

/**
  * State context with predicted header.
  * The predicted header only contains fields that can be predicted.
  */
case class UpcomingStateContext(override val lastHeaders: Seq[Header],
                                override val lastExtensionOpt: Option[Extension],
                                predictedHeader: PreHeader,
                                override val genesisStateDigest: ADDigest,
                                override val currentParameters: Parameters,
                                override val validationSettings: ErgoValidationSettings,
                                override val votingData: VotingData)(implicit chainSettings: ChainSettings)
  extends ErgoStateContext(lastHeaders, lastExtensionOpt, genesisStateDigest, currentParameters,
                            validationSettings, votingData)(chainSettings) {

  override def sigmaPreHeader: sigma.PreHeader = PreHeader.toSigma(predictedHeader)

  override def sigmaLastHeaders: Coll[sigma.Header] = {
    SigmaDsl.Colls.fromArray(lastHeaders.map(h => Header.toSigma(h)).toArray)
  }

  override def toString: String = s"UpcomingStateContext($predictedHeader, $lastHeaders)"

}

/**
  * Additional data required for transactions validation.
  * Script validation requires that it contain at least a preHeader, so it can only be used
  * for transaction validation if lastHeaders not empty or in `upcoming` version.
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
                      (implicit val chainSettings: ChainSettings)
  extends BlockchainStateContext
    with BytesSerializable
    with ScorexEncoding
    with ScorexLogging {

  override type M = ErgoStateContext

  private val votingSettings = chainSettings.voting
  private val popowAlgos = new NipopowAlgos(chainSettings)

  override def sigmaPreHeader: sigma.PreHeader =
    PreHeader.toSigma(lastHeaders.headOption.getOrElse(PreHeader.fake))

  override def sigmaLastHeaders: Coll[sigma.Header] =
    SigmaDsl.Colls.fromArray(lastHeaders.drop(1).map(h => Header.toSigma(h)).toArray)

  // todo remove from ErgoLikeContext and from ErgoStateContext
  // State root hash before the last block
  override def previousStateDigest: Coll[Byte] = if (sigmaLastHeaders.toArray.nonEmpty) {
    sigmaLastHeaders.toArray.head.stateRoot.digest
  } else {
    genesisStateDigest.toColl
  }

  /* NOHF PROOF:
  Changed: removed returning ErgoHistory.EmptyHistoryHeight if sigmaPreHeader is null.
  Motivation: sigmaPreHeader no longer can be null, returning PreHeader.fake.
  Safety: In PreHeader.fake.height = ErgoHistory.EmptyHistoryHeight.
  */
  // todo remove from ErgoLikeContext and from ErgoStateContext
  def currentHeight: Int = sigmaPreHeader.height

  /**
    * @return block version of the protocol
    */
  def blockVersion: Header.Version = currentParameters.blockVersion

  private def votingEpochLength: Int = votingSettings.votingLength

  def lastHeaderOpt: Option[Header] = lastHeaders.headOption

  override def serializer: ErgoSerializer[M] = ErgoStateContextSerializer(chainSettings)

  /**
    * @return state context corresponding to a block after last known one with fields provided
    */
  def upcoming(minerPk: EcPointType,
               timestamp: Long,
               nBits: Long,
               votes: Array[Byte],
               proposedUpdate: ErgoValidationSettingsUpdate,
               version: Byte): UpcomingStateContext = {
    val upcomingHeader = PreHeader(lastHeaderOpt, version, minerPk, timestamp, nBits, votes)
    val forkVote = votes.contains(Parameters.SoftFork)
    val height = ErgoHistoryUtils.heightOf(lastHeaderOpt) + 1
    val (calculatedParams, updated) =
      currentParameters.update(height, forkVote, ArraySeq.unsafeWrapArray(votingData.epochVotes), proposedUpdate, votingSettings)
    val calculatedValidationSettings = validationSettings.updated(updated)
    UpcomingStateContext(lastHeaders, lastExtensionOpt, upcomingHeader, genesisStateDigest, calculatedParams,
                          calculatedValidationSettings, votingData)
  }

  /**
    * @return state context corresponding to a block after last known one
    *         with fields filled with (kind of) default values
    */
  def simplifiedUpcoming(): UpcomingStateContext = {
    val minerPk = org.ergoplatform.mining.group.generator
    val version = lastHeaderOpt.map(_.version).getOrElse(Header.InitialVersion)
    val nBits = lastHeaderOpt.map(_.nBits).getOrElse(chainSettings.initialNBits)
    val timestamp = lastHeaderOpt.map(_.timestamp + 1).getOrElse(System.currentTimeMillis())
    val votes = Array.emptyByteArray
    val proposedUpdate = ErgoValidationSettingsUpdate.empty
    val upcomingHeader = PreHeader(lastHeaderOpt, version, minerPk, timestamp, nBits, votes)
    val height = ErgoHistoryUtils.heightOf(lastHeaderOpt) + 1
    val (calculatedParams, updated) =
      currentParameters.update(height, forkVote = false, ArraySeq.unsafeWrapArray(votingData.epochVotes), proposedUpdate, votingSettings)
    val calculatedValidationSettings = validationSettings.updated(updated)
    UpcomingStateContext(lastHeaders, lastExtensionOpt, upcomingHeader, genesisStateDigest, calculatedParams,
      calculatedValidationSettings, votingData)
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
                              (validationState: ValidationState[Unit]): Try[(Parameters, ErgoValidationSettings)] = {
    val height = header.height
    val parsedParamsTry = Parameters.parseExtension(height, extension)
    val parsedValidationSettingsTry = ErgoValidationSettings.parseExtension(extension)

    validationState
      .validateNoFailure(exParseParameters, parsedParamsTry, extension.id, extension.modifierTypeId)
      .validateNoFailure(exParseValidationSettings, parsedValidationSettingsTry, extension.id, extension.modifierTypeId)
      .validateTry(parsedParamsTry, e => ModifierValidator.fatal("Failed to parse parameters", extension.id, extension.modifierTypeId, e)) {
        case (vs, parsedParams) =>
          vs.validateTry(parsedValidationSettingsTry,
            e => ModifierValidator.fatal("Failed to parse validation settings", extension.id, extension.modifierTypeId, e)) {
            case (currentValidationState, parsedSettings) =>

              /*
               Calculating blockchain parameters and validation rules based on the locally stored blockchain,
               to compare them then with announced ones.
               If current parameters height is equal to 0, it means that whether it is first epoch after genesis,
               or node is starting in light regime with validating suffix only. For the former case, we assume that
               parameters and validation settings were not changed, which is true for both Ergo mainnet and testnet.
               For the latter case, the light fullnode is just relied on PoW majority about parameters and validation
               settings.
               */
              val (calculatedParams, calculatedSettings) = if (currentParameters.height == 0) {
                parsedParams -> parsedSettings
              } else {
                val (params, settingsUpdates) = currentParameters
                  .update(height, forkVote, ArraySeq.unsafeWrapArray(votingData.epochVotes), parsedParams.proposedUpdate, votingSettings)
                val settings = validationSettings.updated(settingsUpdates)
                params -> settings
              }

              currentValidationState
                .validate(exBlockVersion, calculatedParams.blockVersion == header.version, InvalidModifier(s"${calculatedParams.blockVersion} == ${header.version}", extension.id, extension.modifierTypeId))
                .validateNoFailure(exMatchParameters, Parameters.matchParameters(parsedParams, calculatedParams), extension.id, extension.modifierTypeId)
                .validate(exMatchValidationSettings, parsedSettings == calculatedSettings, InvalidModifier(s"$parsedSettings vs $calculatedSettings", extension.id, extension.modifierTypeId))
          }.result
      }.result
      .toTry
      .flatMap(_ => parsedParamsTry.flatMap(p => parsedValidationSettingsTry.map(vs => (p, vs))))
  }

  def process(header: Header, extensionOpt: Option[Extension]): Try[ErgoStateContext] = {
    def newHeaders: Seq[Header] = header +: lastHeaders.take(Constants.LastHeadersInContext - 1)

    Try {
      val headerVotes: Array[Byte] = header.votes
      val height = header.height
      val votes = headerVotes.filter(_ != Parameters.NoParameter)
      val epochStarts = header.votingStarts(votingEpochLength)
      val forkVote = votes.contains(Parameters.SoftFork)

      val state = ModifierValidator(validationSettings)
        .validateNoThrow(exCheckForkVote, if (forkVote) checkForkVote(height), header.id, header.modifierTypeId)

      extensionOpt match {
        case Some(extension) if epochStarts =>
          processExtension(extension, header, forkVote)(state).map { processed =>
            val params = processed._1
            val extractedValidationSettings = processed._2
            val proposedVotes = votes.map(_ -> 1)
            val newVoting = VotingData(proposedVotes)
            new ErgoStateContext(newHeaders, extensionOpt, genesisStateDigest, params,
              extractedValidationSettings, newVoting)(chainSettings)
          }
        case _ =>
          val newVotes = votes
          val newVotingResults = newVotes.foldLeft(votingData) { case (v, id) => v.update(id) }
          state.result.toTry.map { _ =>
            new ErgoStateContext(newHeaders, extensionOpt, genesisStateDigest, currentParameters, validationSettings,
              newVotingResults)(chainSettings)
          }
      }
    }.flatten
  }

  /**
    * Checks that header is coming in order.
    *
    * If no last headers collected yet, header is always applicable. It could be genesis header if no pruning, or
    * header at non-genesis height if pruning.
    *
    * @param header - header to be applied to this state context
    * @return Success if header is coming in order, Failure otherwise
    */
  private def checkHeaderHeight(header: Header): Try[Unit] = {
    if (lastHeaders.isEmpty) {
      log.info(s"Last headers are empty, starting assembling state context with $header")
    }
    if (lastHeaders.isEmpty || lastHeaders.head.height + 1 == header.height) {
      Success(())
    } else {
      Failure(new Exception(s"Improper application of $header (last applied header is $lastHeaderOpt: " +
        s"${header.id} => ${lastHeaderOpt.map(_.height + 1)} != ${header.height}"))
    }
  }

  def appendHeader(header: Header): Try[ErgoStateContext] = {
    validateVotes(header)
      .result
      .toTry
      .flatMap(_ => checkHeaderHeight(header))
      .flatMap { _ =>
        process(header, None)
      }
  }

  /**
    * This function verifies whether a full block is valid against the ErgoStateContext instance, and modifies
    * the latter according to the former.
    *
    * @param fb             - block to apply
    * @return updated state context or error
    */
  def appendFullBlock(fb: ErgoFullBlock): Try[ErgoStateContext] = Try {
    val votesValidationState = validateVotes(fb.header)
    new ExtensionValidator(votesValidationState, popowAlgos)
      .validateExtension(fb.extension, fb.header, lastExtensionOpt, lastHeaderOpt)
      .validate(bsBlockTransactionsSize,
        fb.blockTransactions.size <= currentParameters.maxBlockSize,
        InvalidModifier(s"${fb.id} => ${fb.blockTransactions.size} == ${currentParameters.maxBlockSize}", fb.id, fb.modifierTypeId))
      .validate(exSize,
        fb.extension.size <= Constants.MaxExtensionSize,
        InvalidModifier(s"${fb.id} => ${fb.extension.size} == ${Constants.MaxExtensionSize}", fb.extension.id, fb.extension.modifierTypeId)) // id/type of Block here?
      .result
      .toTry
      .flatMap(_ => checkHeaderHeight(fb.header))
      .flatMap { _ =>
        process(fb.header, Some(fb.extension))
      }
  }.flatten

  override def toString: String =
    s"ErgoStateContext($currentHeight, ${encoder.encode(previousStateDigest.toArray)}, $lastHeaders, $currentParameters)"


  /**
    * Check that non-zero votes extracted from block header are correct
    */
  private def validateVotes(header: Header): ValidationState[ErgoStateContext] = {
    val votes: Array[Byte] = header.votes.filter(_ != Parameters.NoParameter)
    val epochStarts = header.votingStarts(votingSettings.votingLength)
    val votesCount = votes.count(_ != Parameters.SoftFork)
    val reverseVotes: Array[Byte] = votes.map(v => (-v).toByte)

    @inline def vs: String = votes.mkString("")

    ModifierValidator(validationSettings)
      .payload(this)
      .validate(hdrVotesNumber, votesCount <= Parameters.ParamVotesCount, InvalidModifier(s"votesCount=$votesCount", header.id, header.modifierTypeId))
      .validateSeq(votes) { case (validationState, v) =>
        validationState
          .validate(hdrVotesDuplicates, votes.count(_ == v) == 1, InvalidModifier(s"Double vote in $vs", header.id, header.modifierTypeId))
          .validate(hdrVotesContradictory, !reverseVotes.contains(v), InvalidModifier(s"Contradictory votes in $vs", header.id, header.modifierTypeId))
          .validate(hdrVotesUnknown, !(epochStarts && !Parameters.parametersDescs.contains(v)), InvalidModifier(s"Incorrect vote proposed in $vs", header.id, header.modifierTypeId))
      }
  }

}

object ErgoStateContext {

  /**
    * Parameter to vote for to support EIP-27 soft-fork.
    * Also used for output cost.
    */
  val eip27Vote: Byte = 8

  def empty(chainSettings: ChainSettings, parameters: Parameters): ErgoStateContext = {
    empty(chainSettings.genesisStateDigest, chainSettings, parameters)
  }

  /**
    * Initialize empty state context
    */
  def empty(genesisStateDigest: ADDigest, chainSettings: ChainSettings, parameters: Parameters): ErgoStateContext = {
    new ErgoStateContext(Seq.empty, None, genesisStateDigest, parameters, ErgoValidationSettings.initial,
      VotingData.empty)(chainSettings)
  }

  /**
    * Recovers state context at the beginning of the voting epoch.
    *
    * Used in the digest mode only.
    */
  def recover(genesisStateDigest: ADDigest,
              extension: Extension,
              lastHeaders: Seq[Header])
             (chainSettings: ChainSettings): Try[ErgoStateContext] = {
    val vs = chainSettings.voting
    if (lastHeaders.lastOption.exists(_.height % vs.votingLength == 0)) {
      val currentHeader = lastHeaders.last
      Parameters.parseExtension(currentHeader.height, extension).flatMap { params =>
        ErgoValidationSettings.parseExtension(extension).map { validationSettings =>
          new ErgoStateContext(lastHeaders.reverse, Some(extension), genesisStateDigest, params,
            validationSettings, VotingData.empty)(chainSettings)
        }
      }
    } else {
      Failure(new Exception("Context could only be recovered at the start of the voting epoch"))
    }
  }

}

case class ErgoStateContextSerializer(chainSettings: ChainSettings) extends ErgoSerializer[ErgoStateContext] {

  private val Eip27SupportValue = 100 // see comment in serialize()

  override def serialize(esc: ErgoStateContext, w: Writer): Unit = {
    /* NOHF PROOF:
    Changed: added assert to not let `UpcomingStateContext` get serialized.
    Motivation: only `ErgoStateContext` is supported in `parse`.
    Safety: `UpcomingStateContext` is used only in `ErgoMiner.createCandidate` and does not get serialized.
  */
    assert(!esc.isInstanceOf[UpcomingStateContext], "UpcomingStateContext serialization is not supported")
    w.putBytes(esc.genesisStateDigest)
    w.putUByte(esc.lastHeaders.size)
    esc.lastHeaders.foreach(h => HeaderSerializer.serialize(h, w))
    VotingDataSerializer.serialize(esc.votingData, w)
    ParametersSerializer.serialize(esc.currentParameters, w)
    ErgoValidationSettingsSerializer.serialize(esc.validationSettings, w)

    val lastExtensionSize = esc.lastExtensionOpt.size // 0 or 1

    w.putUByte(lastExtensionSize)
    esc.lastExtensionOpt.foreach(e => ExtensionSerializer.serialize(e.toExtension(Header.GenesisParentId), w))
  }

  override def parse(r: Reader): ErgoStateContext = {
    val genesisDigest = ADDigest @@ r.getBytes(33)
    val headersLength = r.getUByte()
    val lastHeaders = (1 to headersLength).map(_ => HeaderSerializer.parse(r))
    val votingData = VotingDataSerializer.parse(r)
    val params = ParametersSerializer.parse(r)
    val validationSettings = ErgoValidationSettingsSerializer.parse(r)

    var lastExtensionOpt: Option[Extension] = None
    var eip27AndExtensionSize = r.getUByte()

    if (eip27AndExtensionSize >= Eip27SupportValue) {
      // we do not store EIP-27 flag into db anymore, but we could read old db with it
      eip27AndExtensionSize = eip27AndExtensionSize - Eip27SupportValue
    }
    if (eip27AndExtensionSize == 1) {
      lastExtensionOpt = Some(ExtensionSerializer.parse(r))
    }

    new ErgoStateContext(lastHeaders, lastExtensionOpt, genesisDigest, params, validationSettings,
      votingData)(chainSettings)
  }

}
