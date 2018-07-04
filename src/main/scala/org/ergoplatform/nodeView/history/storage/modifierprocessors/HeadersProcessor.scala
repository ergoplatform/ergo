package org.ergoplatform.nodeView.history.storage.modifierprocessors

import com.google.common.primitives.Ints
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoApp
import org.ergoplatform.mining.PowScheme
import org.ergoplatform.mining.difficulty.LinearDifficultyControl
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history._
import org.ergoplatform.nodeView.history.ErgoHistory.{Difficulty, GenesisHeight}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.settings.Constants.hashLength
import org.ergoplatform.settings.{Algos, NodeConfigurationSettings}
import scorex.core._
import scorex.core.consensus.History.ProgressInfo
import scorex.core.consensus.ModifierSemanticValidity
import scorex.core.utils.{ScorexEncoding, ScorexLogging}
import scorex.core.validation.{ModifierValidator, ValidationResult}

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.util.Try

/**
  * Contains all functions required by History to process Headers.
  */
trait HeadersProcessor extends ToDownloadProcessor with ScorexLogging with ScorexEncoding {

  private val charsetName = "UTF-8"

  protected val historyStorage: HistoryStorage

  protected val config: NodeConfigurationSettings

  val powScheme: PowScheme

  //Maximum time in future block header may contain
  protected lazy val MaxTimeDrift: Long = 10 * chainSettings.blockInterval.toMillis

  lazy val difficultyCalculator = new LinearDifficultyControl(chainSettings.blockInterval,
    chainSettings.useLastEpochs, chainSettings.epochLength)


  def realDifficulty(h: Header): Difficulty = powScheme.realDifficulty(h)

  def isSemanticallyValid(modifierId: ModifierId): ModifierSemanticValidity

  protected def headerScoreKey(id: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(Algos.hash("score".getBytes(charsetName) ++ id))

  protected def headerHeightKey(id: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(Algos.hash("height".getBytes(charsetName) ++ id))

  protected def validityKey(id: Array[Byte]): ByteArrayWrapper =
    ByteArrayWrapper(Algos.hash("validity".getBytes(charsetName) ++ id))

  protected def bestHeaderIdOpt: Option[ModifierId] = historyStorage.getIndex(BestHeaderKey).map(ModifierId @@ _.data)

  /**
    * Id of best header with transactions and proofs. None in regime that do not process transactions
    */
  def bestFullBlockIdOpt: Option[ModifierId] = None

  /**
    * @return height of best header
    */
  def headersHeight: Int = bestHeaderIdOpt.flatMap(id => heightOf(id)).getOrElse(-1)

  /**
    * @return height of best header with transacions and proofs
    */
  def fullBlockHeight: Int = bestFullBlockIdOpt.flatMap(id => heightOf(id)).getOrElse(-1)

  /**
    * @param id - id of ErgoPersistentModifier
    * @return height of modifier with such id if is in History
    */
  def heightOf(id: ModifierId): Option[Int] = historyStorage.getIndex(headerHeightKey(id))
    .map(b => Ints.fromByteArray(b.data))

  def isInBestChain(id: ModifierId): Boolean = heightOf(id).flatMap(h => bestHeaderIdAtHeight(h))
    .exists(_ sameElements id)

  def isInBestChain(h: Header): Boolean = bestHeaderIdAtHeight(h.height).exists(_ sameElements h.id)

  private def bestHeaderIdAtHeight(h: Int): Option[ModifierId] = headerIdsAtHeight(h).headOption

  /**
    * @param h - header to process
    * @return ProgressInfo - info required for State to be consistent with History
    */
  protected def process(h: Header): ProgressInfo[ErgoPersistentModifier] = {
    val dataToInsert: (Seq[(ByteArrayWrapper, ByteArrayWrapper)], ErgoPersistentModifier) = toInsert(h)

    historyStorage.insert(ByteArrayWrapper(h.id), dataToInsert._1, Seq(dataToInsert._2))

    bestHeaderIdOpt match {
      case Some(bestHeaderId) =>
        // If we verify transactions, we don't need to send this header to state.
        // If we don't and this is the best header, we should send this header to state to update state root hash
        val toProcess = if (config.verifyTransactions || !(bestHeaderId sameElements h.id)) Seq.empty else Seq(h)
        ProgressInfo(None, Seq.empty, toProcess, toDownload(h))
      case None =>
        log.error("Should always have best header after header application")
        ErgoApp.forceStopApplication()
    }
  }

  /**
    * Data, we should add and remove from the storage to process this modifier
    */
  private def toInsert(h: Header): (Seq[(ByteArrayWrapper, ByteArrayWrapper)], ErgoPersistentModifier) = {
    val requiredDifficulty: Difficulty = h.requiredDifficulty
    if (h.isGenesis) {
      genesisToInsert(h, requiredDifficulty)
    } else {
      nonGenesisToInsert(h, requiredDifficulty)
    }
  }

  /**
    * Data to insert for regular block
    */
  private def nonGenesisToInsert(h: Header, requiredDifficulty: Difficulty) = {
    val score = scoreOf(h.parentId).get + requiredDifficulty
    val bestRow: Seq[(ByteArrayWrapper, ByteArrayWrapper)] =
      if (score > bestHeadersChainScore) Seq(BestHeaderKey -> ByteArrayWrapper(h.id)) else Seq.empty
    val scoreRow = headerScoreKey(h.id) -> ByteArrayWrapper(score.toByteArray)
    val heightRow = headerHeightKey(h.id) -> ByteArrayWrapper(Ints.toByteArray(h.height))
    val headerIdsRow = if (score > bestHeadersChainScore) {
      bestBlockHeaderIdsRow(h, score)
    } else {
      orphanedBlockHeaderIdsRow(h, score)
    }
    (Seq(scoreRow, heightRow) ++ bestRow ++ headerIdsRow, h)
  }

  /**
    * Data to insert for genesis block
    */
  private def genesisToInsert(h: Header, requiredDifficulty: Difficulty) = {
    log.info(s"Initialize header chain with genesis header ${h.encodedId}")
    (Seq(
      BestHeaderKey -> ByteArrayWrapper(h.id),
      heightIdsKey(GenesisHeight) -> ByteArrayWrapper(h.id),
      headerHeightKey(h.id) -> ByteArrayWrapper(Ints.toByteArray(GenesisHeight)),
      headerScoreKey(h.id) -> ByteArrayWrapper(requiredDifficulty.toByteArray)),
      h)
  }


  /**
    * Row to storage, that put this orphaned block id to the end of header ids at this height
    */
  private def orphanedBlockHeaderIdsRow(h: Header, score: Difficulty) = {
    log.info(s"New orphaned header ${h.encodedId} at height ${h.height} with score $score")
    Seq(heightIdsKey(h.height) -> ByteArrayWrapper((headerIdsAtHeight(h.height) :+ h.id).flatten.toArray))
  }


  /**
    * Update header ids to ensure, that this block id and ids of all parent blocks are in the first position of
    * header ids at this height
    */
  private def bestBlockHeaderIdsRow(h: Header, score: Difficulty) = {
    val prevHeight = headersHeight
    log.info(s"New best header ${h.encodedId} with score $score. Hew height ${h.height}, old height $prevHeight")
    val self: (ByteArrayWrapper, ByteArrayWrapper) =
      heightIdsKey(h.height) -> ByteArrayWrapper((Seq(h.id) ++ headerIdsAtHeight(h.height)).flatten.toArray)
    val parentHeaderOpt: Option[Header] = typedModifierById[Header](h.parentId)
    val forkHeaders = parentHeaderOpt.toSeq
      .flatMap(parent => headerChainBack(h.height, parent, h => isInBestChain(h)).headers)
      .filter(h => !isInBestChain(h))
    val forkIds: Seq[(ByteArrayWrapper, ByteArrayWrapper)] = forkHeaders.map { header =>
      val otherIds = headerIdsAtHeight(header.height).filter(id => !(id sameElements header.id))
      heightIdsKey(header.height) -> ByteArrayWrapper((Seq(header.id) ++ otherIds).flatten.toArray)
    }
    forkIds :+ self
  }

  /**
    *
    * @param header - header we're going to remove from history
    * @return ids to remove, new data to apply
    */
  protected def reportInvalid(header: Header): (Seq[ByteArrayWrapper], Seq[(ByteArrayWrapper, ByteArrayWrapper)]) = {
    val modifierId = header.id
    val payloadModifiers = Seq(header.transactionsId, header.ADProofsId).filter(id => historyStorage.contains(id))
      .map(id => ByteArrayWrapper(id))

    val toRemove = Seq(headerScoreKey(modifierId), ByteArrayWrapper(modifierId)) ++ payloadModifiers
    val bestHeaderKeyUpdate = if (bestHeaderIdOpt.exists(_ sameElements modifierId)) {
      Seq(BestHeaderKey -> ByteArrayWrapper(header.parentId))
    } else {
      Seq.empty
    }
    val bestFullBlockKeyUpdate = if (bestFullBlockIdOpt.exists(_ sameElements modifierId)) {
      Seq(BestFullBlockKey -> ByteArrayWrapper(header.parentId))
    } else {
      Seq.empty
    }
    (toRemove, bestFullBlockKeyUpdate ++ bestHeaderKeyUpdate)
  }

  /** Validates given header
    *
    * @return Success() if header is valid, Failure(error) otherwise
    */
  protected def validate(header: Header): Try[Unit] = new HeaderValidator().validate(header).toTry

  protected val BestHeaderKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(hashLength)(Header.modifierTypeId))

  protected val BestFullBlockKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(hashLength)(-1))

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
    ModifierId @@ historyStorage.getIndex(heightIdsKey(height: Int)).map(_.data).getOrElse(Array()).grouped(32).toSeq

  /**
    * @param limit       - maximum length of resulting HeaderChain
    * @param startHeader - header to start
    * @param until       - stop condition
    * @return at most limit header back in history starting from startHeader and when condition until is not satisfied
    *         Note now it includes one header satisfying until condition!
    */
  protected def headerChainBack(limit: Int, startHeader: Header, until: Header => Boolean): HeaderChain = {
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
    * Find first header with the best height <= $height which id satisfies condition $p
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

  class HeaderValidator extends ModifierValidator with ScorexEncoding {

    def validate(header: Header): ValidationResult = {
      if (header.isGenesis) {
        validateGenesisBlockHeader(header)
      } else {
        val parentOpt = typedModifierById[Header](header.parentId)
        parentOpt map { parent =>
          validateChildBlockHeader(header, parent)
        } getOrElse {
          error(s"Parent header with id ${Algos.encode(header.parentId)} is not defined")
        }
      }
    }

    private def validateGenesisBlockHeader(header: Header): ValidationResult = {
      accumulateErrors
        .validateEqualIds(header.parentId, Header.GenesisParentId) { detail =>
          fatal(s"Genesis block should have genesis parent id. $detail")
        }
        .validate(bestHeaderIdOpt.isEmpty) {
          fatal("Trying to append genesis block to non-empty history")
        }
        .validate(header.height == GenesisHeight) {
          fatal(s"Height of genesis block $header is incorrect")
        }
        .result
    }

    private def validateChildBlockHeader(header: Header, parent: Header): ValidationResult = {
      failFast
        .validate(header.timestamp - timeProvider.time() <= MaxTimeDrift) {
          error(s"Header timestamp ${header.timestamp} is too far in future from now ${timeProvider.time()}")
        }
        .validate(header.timestamp > parent.timestamp) {
          fatal(s"Header timestamp ${header.timestamp} is not greater than parents ${parent.timestamp}")
        }
        .validate(header.height == parent.height + 1) {
          fatal(s"Header height ${header.height} is not greater by 1 than parents ${parent.height}")
        }
        .validateNot(historyStorage.contains(header.id)) {
          fatal("Header is already in history")
        }
        .validate(realDifficulty(header) >= header.requiredDifficulty) {
          fatal(s"Block difficulty ${realDifficulty(header)} is less than required ${header.requiredDifficulty}")
        }
        .validateEquals(header.requiredDifficulty)(requiredDifficultyAfter(parent)) { detail =>
          fatal(s"Incorrect required difficulty. $detail")
        }
        .validate(heightOf(header.parentId).exists(h => headersHeight - h < config.keepVersions)) {
          fatal(s"Trying to apply too old header at height ${heightOf(header.parentId)}")
        }
        .validate(powScheme.verify(header)) {
          fatal(s"Wrong proof-of-work solution for $header")
        }
        .validateSemantics(isSemanticallyValid(header.parentId)) {
          fatal("Parent header is marked as semantically invalid")
        }
        .result
    }

  }

}
