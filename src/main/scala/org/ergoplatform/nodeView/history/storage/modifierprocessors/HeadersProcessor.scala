package org.ergoplatform.nodeView.history.storage.modifierprocessors

import com.google.common.primitives.Ints
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoApp
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.mining.difficulty.LinearDifficultyControl
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history._
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.ErgoHistory.{Difficulty, GenesisHeight}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.settings.Constants.HashLength
import org.ergoplatform.settings.ValidationRules._
import org.ergoplatform.settings._
import scorex.core.consensus.History.ProgressInfo
import scorex.core.consensus.ModifierSemanticValidity
import scorex.core.utils.ScorexEncoding
import scorex.core.validation.{ModifierValidator, ValidationResult, ValidationState}
import scorex.util._

import scala.annotation.tailrec
import scala.util.Try

/**
  * Contains all functions required by History to process Headers.
  */
trait HeadersProcessor extends ToDownloadProcessor with ScorexLogging with ScorexEncoding {

  protected val historyStorage: HistoryStorage

  protected val settings: ErgoSettings

  val powScheme: AutolykosPowScheme

  //Maximum time in future block header may contain
  protected lazy val MaxTimeDrift: Long = 10 * chainSettings.blockInterval.toMillis

  lazy val difficultyCalculator = new LinearDifficultyControl(chainSettings)

  def realDifficulty(h: Header): Difficulty = powScheme.realDifficulty(h)

  def isSemanticallyValid(modifierId: ModifierId): ModifierSemanticValidity

  // todo for performance reasons we may just use key like s"score$id" but this will require to redownload blockchain
  protected def headerScoreKey(id: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(Algos.hash("score".getBytes(ErgoHistory.CharsetName) ++ idToBytes(id)))

  protected def headerHeightKey(id: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(Algos.hash("height".getBytes(ErgoHistory.CharsetName) ++ idToBytes(id)))

  protected[history] def validityKey(id: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(Algos.hash("validity".getBytes(ErgoHistory.CharsetName) ++ idToBytes(id)))

  protected def bestHeaderIdOpt: Option[ModifierId] = historyStorage.getIndex(BestHeaderKey).map(w => bytesToId(w.data))

  /**
    * Id of best header with transactions and proofs. None in regime that do not process transactions
    */
  def bestFullBlockIdOpt: Option[ModifierId] = None

  /**
    * @return height of best header
    */
  def headersHeight: Int = bestHeaderIdOpt.flatMap(id => heightOf(id)).getOrElse(ErgoHistory.EmptyHistoryHeight)

  /**
    * @return height of best header with all block sections
    */
  def fullBlockHeight: Int = bestFullBlockIdOpt.flatMap(id => heightOf(id)).getOrElse(ErgoHistory.EmptyHistoryHeight)

  /**
    * @param id - id of ErgoPersistentModifier
    * @return height of modifier with such id if is in History
    */
  def heightOf(id: ModifierId): Option[Int] = historyStorage.getIndex(headerHeightKey(id))
    .map(b => Ints.fromByteArray(b.data))

  def isInBestChain(id: ModifierId): Boolean = heightOf(id).flatMap(h => bestHeaderIdAtHeight(h)).contains(id)

  def isInBestChain(h: Header): Boolean = bestHeaderIdAtHeight(h.height).contains(h.id)

  private def bestHeaderIdAtHeight(h: Int): Option[ModifierId] = headerIdsAtHeight(h).headOption

  /**
    * @param h - header to process
    * @return ProgressInfo - info required for State to be consistent with History
    */
  protected def process(h: Header): ProgressInfo[ErgoPersistentModifier] = {
    val dataToInsert: (Seq[(ByteArrayWrapper, ByteArrayWrapper)], Seq[ErgoPersistentModifier]) = toInsert(h)

    historyStorage.insert(Algos.idToBAW(h.id), dataToInsert._1, dataToInsert._2)

    bestHeaderIdOpt match {
      case Some(bestHeaderId) =>
        // If we verify transactions, we don't need to send this header to state.
        // If we don't and this is the best header, we should send this header to state to update state root hash
        val toProcess = if (config.verifyTransactions || !(bestHeaderId == h.id)) Seq.empty else Seq(h)
        ProgressInfo(None, Seq.empty, toProcess, toDownload(h))
      case None =>
        log.error("Should always have best header after header application")
        ErgoApp.forceStopApplication()
    }
  }

  /**
    * Data, we should add and remove from the storage to process this modifier
    */
  private def toInsert(h: Header): (Seq[(ByteArrayWrapper, ByteArrayWrapper)], Seq[ErgoPersistentModifier]) = {
    val requiredDifficulty: Difficulty = h.requiredDifficulty
    val score = scoreOf(h.parentId).getOrElse(BigInt(0)) + requiredDifficulty
    val bestRow: Seq[(ByteArrayWrapper, ByteArrayWrapper)] =
      if (score > bestHeadersChainScore) Seq(BestHeaderKey -> Algos.idToBAW(h.id)) else Seq.empty
    val scoreRow = headerScoreKey(h.id) -> ByteArrayWrapper(score.toByteArray)
    val heightRow = headerHeightKey(h.id) -> ByteArrayWrapper(Ints.toByteArray(h.height))
    val headerIdsRow = if (score > bestHeadersChainScore) {
      if (h.isGenesis) log.info(s"Processing genesis header ${h.encodedId}")
      bestBlockHeaderIdsRow(h, score)
    } else {
      orphanedBlockHeaderIdsRow(h, score)
    }

    (Seq(scoreRow, heightRow) ++ bestRow ++ headerIdsRow, Seq(h))
  }

  /**
    * Row to storage, that put this orphaned block id to the end of header ids at this height
    */
  private def orphanedBlockHeaderIdsRow(h: Header, score: Difficulty) = {
    log.info(s"New orphaned header ${h.encodedId} at height ${h.height} with score $score")
    Seq(heightIdsKey(h.height) -> ByteArrayWrapper((headerIdsAtHeight(h.height) :+ h.id).flatMap(idToBytes).toArray))
  }

  /**
    * Update header ids to ensure, that this block id and ids of all parent blocks are in the first position of
    * header ids at this height
    */
  private def bestBlockHeaderIdsRow(h: Header, score: Difficulty) = {
    val prevHeight = headersHeight
    log.info(s"New best header ${h.encodedId} with score $score. New height ${h.height}, old height $prevHeight")
    val self: (ByteArrayWrapper, ByteArrayWrapper) =
      heightIdsKey(h.height) -> ByteArrayWrapper((Seq(h.id) ++ headerIdsAtHeight(h.height)).flatMap(idToBytes).toArray)
    val parentHeaderOpt: Option[Header] = typedModifierById[Header](h.parentId)
    val forkHeaders = parentHeaderOpt.toSeq
      .flatMap(parent => headerChainBack(h.height, parent, h => isInBestChain(h)).headers)
      .filter(h => !isInBestChain(h))
    val forkIds: Seq[(ByteArrayWrapper, ByteArrayWrapper)] = forkHeaders.map { header =>
      val otherIds = headerIdsAtHeight(header.height).filter(id => id != header.id)
      heightIdsKey(header.height) -> ByteArrayWrapper((Seq(header.id) ++ otherIds).flatMap(idToBytes).toArray)
    }
    forkIds :+ self
  }

  /** Validates given header
    *
    * @return Success() if header is valid, Failure(error) otherwise
    */
  protected def validate(header: Header): Try[Unit] = new HeaderValidator().validate(header).toTry

  protected val BestHeaderKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(HashLength)(Header.modifierTypeId))

  protected val BestFullBlockKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(HashLength)(-1))

  /**
    * @param id - header id
    * @return score of header with such id if is in History
    */
  def scoreOf(id: ModifierId): Option[BigInt] = historyStorage.getIndex(headerScoreKey(id))
    .map(b => BigInt(b.data))

  /**
    * @param height - block height
    * @return ids of headers on chosen height.
    *         Seq.empty we don't have any headers on this height (e.g. it is too big or we bootstrap in PoPoW regime)
    *         single id if no forks on this height
    *         multiple ids if there are forks at chosen height.
    *         First id is always from the best headers chain.
    */
  def headerIdsAtHeight(height: Int): Seq[ModifierId] =
    historyStorage.getIndex(heightIdsKey(height: Int))
      .map(w => w.data).getOrElse(Array()).grouped(32).map(bytesToId).toSeq

  /**
    * @param limit       - maximum length of resulting HeaderChain
    * @param startHeader - header to start
    * @param until       - stop condition
    * @return at most limit header back in history starting from startHeader and when condition until is not satisfied
    *         Note now it includes one header satisfying until condition!
    */
  def headerChainBack(limit: Int, startHeader: Header, until: Header => Boolean): HeaderChain = {
    @tailrec
    def loop(header: Header, acc: Seq[Header]): Seq[Header] = {
      if (acc.lengthCompare(limit) == 0 || until(header)) {
        acc
      } else {
        typedModifierById[Header](header.parentId) match {
          case Some(parent: Header) =>
            loop(parent, acc :+ parent)
          case None if acc.contains(header) =>
            acc
          case _ =>
            acc :+ header
        }
      }
    }

    if (bestHeaderIdOpt.isEmpty || (limit == 0)) {
      HeaderChain(Seq.empty)
    } else {
      HeaderChain(loop(startHeader, Seq(startHeader)).reverse)
    }
  }

  /**
    * Find first header with the best height <= `height` which id satisfies condition `p`
    *
    * @param height - start height
    * @param p      - condition to satisfy
    * @return found header
    */
  @tailrec
  protected final def loopHeightDown(height: Int, p: ModifierId => Boolean): Option[Header] = {
    headerIdsAtHeight(height).find(id => p(id)).flatMap(id => typedModifierById[Header](id)) match {
      case Some(header) => Some(header)
      case None if height > 0 => loopHeightDown(height - 1, p)
      case None => None
    }
  }

  private def bestHeadersChainScore: BigInt = bestHeaderIdOpt.flatMap(id => scoreOf(id)).getOrElse(0)

  private def heightIdsKey(height: Int): ByteArrayWrapper = ByteArrayWrapper(Algos.hash(Ints.toByteArray(height)))

  def requiredDifficultyAfter(parent: Header): Difficulty = {
    val fallbackRequired = timeProvider.time() - parent.timestamp >= Constants.DiffFallbackDuration.toMillis &&
      !settings.networkType.isMainNet
    if (fallbackRequired) {
      Constants.FallbackDiff
    } else {
      val parentHeight = parent.height
      val heights = difficultyCalculator.previousHeadersRequiredForRecalculation(parentHeight + 1)
        .ensuring(_.last == parentHeight)
      if (heights.lengthCompare(1) == 0) {
        difficultyCalculator.calculate(Seq(parent))
      } else {
        val chain = headerChainBack(heights.max - heights.min + 1, parent, (_: Header) => false)
        val headers = chain.headers.filter(p => heights.contains(p.height))
        difficultyCalculator.calculate(headers)
      }
    }
  }

  class HeaderValidator extends ScorexEncoding {

    private def validationState: ValidationState[Unit] = ModifierValidator(ErgoValidationSettings.initial)

    def validate(header: Header): ValidationResult[Unit] = {
      if (header.isGenesis) {
        validateGenesisBlockHeader(header)
      } else {
        val parentOpt = typedModifierById[Header](header.parentId)
        parentOpt map { parent =>
          validateChildBlockHeader(header, parent)
        } getOrElse {
          validationState.validate(hdrParent, condition = false, Algos.encode(header.parentId))
        }
      }
    }

    private def validateGenesisBlockHeader(header: Header): ValidationResult[Unit] = {
      validationState
        .validateEqualIds(hdrGenesisParent, header.parentId, Header.GenesisParentId)
        .validateOrSkipFlatten(hdrGenesisFromConfig, chainSettings.genesisId, (id: ModifierId) => id.equals(header.id))
        .validate(hdrGenesisHeight, header.height == GenesisHeight, header.toString)
        .validateNoFailure(hdrPoW, powScheme.validate(header))
        .validateEquals(hdrRequiredDifficulty, header.requiredDifficulty, chainSettings.initialDifficulty)
        .validateNot(alreadyApplied, historyStorage.contains(header.id), header.id.toString)
        .validate(hdrTooOld, fullBlockHeight < config.keepVersions, heightOf(header.parentId).toString)
        .validate(hdrFutureTimestamp, header.timestamp - timeProvider.time() <= MaxTimeDrift, s"${header.timestamp} vs ${timeProvider.time()}")
        .result
    }

    private def validateChildBlockHeader(header: Header, parent: Header): ValidationResult[Unit] = {
      validationState
        .validate(hdrNonIncreasingTimestamp, header.timestamp > parent.timestamp, s"${header.timestamp} > ${parent.timestamp}")
        .validate(hdrHeight, header.height == parent.height + 1, s"${header.height} vs ${parent.height}")
        .validateNoFailure(hdrPoW, powScheme.validate(header))
        .validateEquals(hdrRequiredDifficulty, header.requiredDifficulty, requiredDifficultyAfter(parent))
        .validate(hdrTooOld, heightOf(header.parentId).exists(h => fullBlockHeight - h < config.keepVersions), heightOf(header.parentId).toString)
        .validateSemantics(hdrParentSemantics, isSemanticallyValid(header.parentId))
        .validate(hdrFutureTimestamp, header.timestamp - timeProvider.time() <= MaxTimeDrift, s"${header.timestamp} vs ${timeProvider.time()}")
        .validateNot(alreadyApplied, historyStorage.contains(header.id), header.id.toString)
        .result
    }

  }

}
