package org.ergoplatform.nodeView.history.storage.modifierprocessors

import com.google.common.primitives.Ints
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.mining.difficulty.LinearDifficultyControl
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.ErgoHistory.{Difficulty, GenesisHeight}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.settings.Constants.hashLength
import org.ergoplatform.settings.{Algos, Constants, NodeConfigurationSettings, _}
import scorex.core._
import scorex.core.consensus.History.ProgressInfo
import scorex.core.consensus.ModifierSemanticValidity
import scorex.core.utils.{NetworkTimeProvider, ScorexLogging}

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/**
  * Contains all functions required by History to process Headers.
  */
trait HeadersProcessor extends ScorexLogging {

  protected val timeProvider: NetworkTimeProvider

  protected val historyStorage: HistoryStorage

  private val charsetName = "UTF-8"

  val powScheme: PoWScheme

  def realDifficulty(h: Header): Difficulty = powScheme.realDifficulty(h)

  protected val config: NodeConfigurationSettings
  protected val chainSettings: ChainSettings

  //TODO alternative DDoS protection
  protected lazy val MaxRollback: Long = 600.days.toMillis / chainSettings.blockInterval.toMillis

  //Maximum time in future block header main contain
  protected lazy val MaxTimeDrift: Long = 10 * chainSettings.blockInterval.toMillis

  lazy val difficultyCalculator = new LinearDifficultyControl(chainSettings.blockInterval,
    chainSettings.useLastEpochs, chainSettings.epochLength)

  def isSemanticallyValid(modifierId: ModifierId): ModifierSemanticValidity.Value

  def bestFullBlockOpt: Option[ErgoFullBlock]

  def contains(id: ModifierId): Boolean

  def typedModifierById[T <: ErgoPersistentModifier](id: ModifierId): Option[T]

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
    * @param header - header to process
    * @return ProgressInfo - info required for State to be consistent with History
    */
  protected def process(header: Header): ProgressInfo[ErgoPersistentModifier] = {
    val dataToInsert = toInsert(header)
    historyStorage.insert(ByteArrayWrapper(header.id), dataToInsert._1, Seq(dataToInsert._2))
    val score = scoreOf(header.id).getOrElse(-1)

    if (bestHeaderIdOpt.isEmpty) {
      log.info(s"Initialize header chain with genesis header ${Algos.encode(header.id)}")
      ProgressInfo(None, Seq(), Some(header), toDownload(header))
    } else if (bestHeaderIdOpt.get sameElements header.id) {
      log.info(s"New best header ${Algos.encode(header.id)} at height ${header.height} with score $score")
      ProgressInfo(None, Seq(), Some(header), toDownload(header))
    } else {
      log.info(s"New orphaned header ${header.encodedId} at height ${header.height} with score $score")
      ProgressInfo(None, Seq(), None, toDownload(header))
    }
  }

  private def toDownload(h: Header): Seq[(ModifierTypeId, ModifierId)] = {
    (config.verifyTransactions, config.ADState) match {
      case (true, true) =>
        Seq((BlockTransactions.modifierTypeId, h.transactionsId), (ADProofs.modifierTypeId, h.ADProofsId))
      case (true, false) =>
        Seq((BlockTransactions.modifierTypeId, h.transactionsId))
      case (false, _) => Seq()
    }
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
    } else Seq()
    val bestFullBlockKeyUpdate = if (bestFullBlockIdOpt.exists(_ sameElements modifierId)) {
      Seq(BestFullBlockKey -> ByteArrayWrapper(header.parentId))
    } else Seq()
    (toRemove, bestFullBlockKeyUpdate ++ bestHeaderKeyUpdate)

  }

  /**
    * @param header - header to validate
    * @return Success() if header is valid, Failure(error) otherwise
    */
  protected def validate(header: Header): Try[Unit] = {
    lazy val parentOpt = typedModifierById[Header](header.parentId)
    if (header.parentId sameElements Header.GenesisParentId) {
      if (bestHeaderIdOpt.nonEmpty) {
        Failure(new Error("Trying to append genesis block to non-empty history"))
      } else if (header.height != GenesisHeight) {
        Failure(new Error(s"Height of genesis block $header is incorrect"))
      } else {
        Success()
      }
    } else if (parentOpt.isEmpty) {
      Failure(new Error(s"Parent header with id ${Algos.encode(header.parentId)} not defined"))
    } else if (header.timestamp - timeProvider.time() > MaxTimeDrift) {
      Failure(new Error(s"Header timestamp ${header.timestamp} is too far in future from now ${timeProvider.time()}"))
    } else if (header.timestamp <= parentOpt.get.timestamp) {
      Failure(new Error(s"Header timestamp ${header.timestamp} is not greater than parents ${parentOpt.get.timestamp}"))
    } else if (header.height != parentOpt.get.height + 1) {
      Failure(new Error(s"Header height ${header.height} is not greater by 1 than parents ${parentOpt.get.height + 1}"))
    } else if (historyStorage.contains(header.id)) {
      Failure(new Error("Header is already in history"))
    } else if (realDifficulty(header) < header.requiredDifficulty) {
      Failure(new Error(s"Block difficulty ${realDifficulty(header)} is less than required ${header.requiredDifficulty}"))
    } else if (header.requiredDifficulty != requiredDifficultyAfter(parentOpt.get)) {
      Failure(new Error(s"Incorrect difficulty: ${header.requiredDifficulty} != ${requiredDifficultyAfter(parentOpt.get)}"))
    } else if (!heightOf(header.parentId).exists(h => headersHeight - h < MaxRollback)) {
      Failure(new Error(s"Trying to apply too old block difficulty at height ${heightOf(header.parentId)}"))
    } else if (!powScheme.verify(header)) {
      Failure(new Error(s"Wrong proof-of-work solution for $header"))
    } else if (isSemanticallyValid(header.parentId) == ModifierSemanticValidity.Invalid) {
      Failure(new Error(s"Parent header is marked as semantically invalid"))
    } else {
      Success()
    }.recoverWith { case thr =>
      log.warn("Validation error: ", thr)
      Failure(thr)
    }
  }

  protected val BestHeaderKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(hashLength)(Header.modifierTypeId))

  protected val BestFullBlockKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(hashLength)(-1))

  /**
    * @param id - header id
    * @return score of header with such id if is in History
    */
  protected def scoreOf(id: ModifierId): Option[BigInt] = historyStorage.getIndex(headerScoreKey(id))
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
    *         Note now it includes one header satisfying until condition! (TODO fix)
    */
  protected def headerChainBack(limit: Int, startHeader: Header, until: Header => Boolean): HeaderChain = {
    @tailrec
    def loop(header: Header, acc: Seq[Header]): Seq[Header] = {
      if (acc.length == limit || until(header)) {
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

    if (bestHeaderIdOpt.isEmpty || (limit == 0)) HeaderChain(Seq())
    else HeaderChain(loop(startHeader, Seq(startHeader)).reverse)
  }

  private def bestHeadersChainScore: BigInt = scoreOf(bestHeaderIdOpt.get).get

  private def toInsert(h: Header): (Seq[(ByteArrayWrapper, ByteArrayWrapper)], ErgoPersistentModifier) = {
    val requiredDifficulty: Difficulty = h.requiredDifficulty
    if (h.isGenesis) {
      (Seq(
        BestHeaderKey -> ByteArrayWrapper(h.id),
        heightIdsKey(GenesisHeight) -> ByteArrayWrapper(h.id),
        headerHeightKey(h.id) -> ByteArrayWrapper(Ints.toByteArray(GenesisHeight)),
        headerScoreKey(h.id) -> ByteArrayWrapper(requiredDifficulty.toByteArray)),
        h)
    } else {
      val blockScore = scoreOf(h.parentId).get + requiredDifficulty
      val bestRow: Seq[(ByteArrayWrapper, ByteArrayWrapper)] =
        if (blockScore > bestHeadersChainScore) Seq(BestHeaderKey -> ByteArrayWrapper(h.id)) else Seq()

      val scoreRow = headerScoreKey(h.id) -> ByteArrayWrapper(blockScore.toByteArray)
      val heightRow = headerHeightKey(h.id) -> ByteArrayWrapper(Ints.toByteArray(h.height))
      val headerIdsRow = if (blockScore > bestHeadersChainScore) {
        // Best block. All blocks back should have their id in the first position
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
      } else {
        // Orphaned block. Put id to the end
        Seq(heightIdsKey(h.height) -> ByteArrayWrapper((headerIdsAtHeight(h.height) :+ h.id).flatten.toArray))
      }
      (Seq(scoreRow, heightRow) ++ bestRow ++ headerIdsRow, h)
    }
  }

  private def heightIdsKey(height: Int): ByteArrayWrapper = ByteArrayWrapper(Algos.hash(Ints.toByteArray(height)))

  def requiredDifficulty: Difficulty =
    bestHeaderIdOpt
      .flatMap(id => typedModifierById[Header](id))
      .map(h => requiredDifficultyAfter(h))
      .getOrElse(Constants.InitialDifficulty)

  def requiredDifficultyAfter(parent: Header): Difficulty = {
    val parentId: ModifierId = parent.id
    val parentHeight = heightOf(parentId).get
    val heights = difficultyCalculator.previousHeadersRequiredForRecalculation(parentHeight + 1)
      .ensuring(_.last == parentHeight)
    if (heights.length == 1) {
      difficultyCalculator.calculate(Seq((parentHeight, parent)))
    } else {
      val chain = headerChainBack(heights.max - heights.min + 1, parent, (h: Header) => false)
        .ensuring(_.length == (heights.min to heights.max).length)
      val previousHeaders = (heights.min to heights.max).zip(chain.headers).filter(p => heights.contains(p._1))
        .ensuring(_.length == heights.length)
      difficultyCalculator.calculate(previousHeaders)
    }
  }
}