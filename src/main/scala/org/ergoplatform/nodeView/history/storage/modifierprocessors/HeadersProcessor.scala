package org.ergoplatform.nodeView.history.storage.modifierprocessors

import com.google.common.primitives.Ints
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.mining.difficulty.LinearDifficultyControl
import org.ergoplatform.modifiers.history.{Header, HeaderChain, HistoryModifierSerializer, PoWScheme}
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.ErgoHistory.{Difficulty, GenesisHeight}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.settings.Constants.hashLength
import org.ergoplatform.settings.{Algos, Constants, NodeConfigurationSettings, _}
import scorex.core._
import scorex.core.consensus.History.ProgressInfo
import scorex.core.consensus.ModifierSemanticValidity
import scorex.core.utils.{NetworkTime, ScorexLogging}

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/**
  * Contains all functions required by History to process Headers.
  */
trait HeadersProcessor extends ScorexLogging {

  protected val historyStorage: HistoryStorage

  val powScheme: PoWScheme

  def realDifficulty(h: Header): Difficulty = powScheme.realDifficulty(h)

  protected val config: NodeConfigurationSettings
  protected val chainSettings: ChainSettings

  //TODO alternative DDoS protection
  protected lazy val MaxRollback: Long = 600.days.toMillis / chainSettings.blockInterval.toMillis

  //Maximum time in future block header main contain
  protected lazy val MaxTimeDrift: Long = 10 * chainSettings.blockInterval.toMillis

  protected lazy val difficultyCalculator = new LinearDifficultyControl(chainSettings.blockInterval,
    chainSettings.epochLength)

  def isSemanticallyValid(modifierId: ModifierId): ModifierSemanticValidity.Value

  def bestFullBlockOpt: Option[ErgoFullBlock]

  def typedModifierById[T <: ErgoPersistentModifier](id: ModifierId): Option[T]

  protected def headerScoreKey(id: ModifierId): ByteArrayWrapper = ByteArrayWrapper(Algos.hash("score".getBytes ++ id))

  protected def headerHeightKey(id: ModifierId): ByteArrayWrapper = ByteArrayWrapper(Algos.hash("height".getBytes ++ id))

  protected def validityKey(id: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(Algos.hash("validity".getBytes ++ id))

  protected def bestHeaderIdOpt: Option[ModifierId] = historyStorage.db.get(BestHeaderKey).map(ModifierId @@ _.data)

  /**
    * Id of best header with transactions and proofs. None in regime that do not process transactions
    */
  def bestFullBlockIdOpt: Option[ModifierId] = None

  /**
    * @return height of best header
    */
  def height: Int = bestHeaderIdOpt.flatMap(id => heightOf(id)).getOrElse(-1)

  /**
    * @param id - id of ErgoPersistentModifier
    * @return height of modifier with such id if is in History
    */
  def heightOf(id: ModifierId): Option[Int] =
  historyStorage.db
    .get(headerHeightKey(id))
    .map(b => Ints.fromByteArray(b.data))

  /**
    * @param header - header to process
    * @return ProgressInfo - info required for State to be consistent with History
    */
  protected def process(header: Header): ProgressInfo[ErgoPersistentModifier] = {
    val dataToInsert = toInsert(header)
    historyStorage.insert(header.id, dataToInsert)

    //todo: why the first check?
    if (bestHeaderIdOpt.isEmpty || (bestHeaderIdOpt.get sameElements header.id)) {
      log.info(s"New best header ${Algos.encode(header.id)} with score ${scoreOf(header.id)}")
      //TODO Notify node view holder that it should download transactions ?
      ProgressInfo(None, Seq(), Seq(header), Seq())
    } else {
      log.info(s"New orphaned header ${header.encodedId} with score ${scoreOf(header.id)}")
      ProgressInfo(None, Seq(), Seq(), Seq())
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
    } else if (header.timestamp - NetworkTime.time() > MaxTimeDrift) {
      Failure(new Error(s"Header timestamp ${header.timestamp} is too far in future from now ${NetworkTime.time()}"))
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
    } else if (!heightOf(header.parentId).exists(h => height - h < MaxRollback)) {
      Failure(new Error(s"Trying to apply too old block difficulty at height ${heightOf(header.parentId)}"))
    } else if (!powScheme.verify(header)) {
      Failure(new Error(s"Wrong proof-of-work solution for $header"))
    } else if (isSemanticallyValid(header.parentId) == ModifierSemanticValidity.Invalid) {
      Failure(new Error(s"Parent header is marked as semantically invalid"))
    } else {
      Success()
    }.recoverWith{case thr =>
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
  protected def scoreOf(id: ModifierId): Option[BigInt] =
  historyStorage.db
    .get(headerScoreKey(id))
    .map(b => BigInt(b.data))

  /**
    * @param height - block height
    * @return ids of headers on chosen height.
    *         Seq.empty we don't have any headers on this height (e.g. it is too big or we bootstrap in PoPoW regime)
    *         single id if no forks on this height
    *         multiple ids if there are forks at chosen height
    */
  protected def headerIdsAtHeight(height: Int): Seq[ModifierId] =
  ModifierId @@ historyStorage.db.get(heightIdsKey(height: Int)).map(_.data).getOrElse(Array()).grouped(32).toSeq

  /**
    * @param limit       - maximum length of resulting HeaderChain
    * @param startHeader - header to start
    * @param until       - stop condition
    * @return at most limit header back in history starting from startHeader and when condition until is not satisfied
    */
  protected def headerChainBack(limit: Int, startHeader: Header, until: Header => Boolean): HeaderChain = {
    @tailrec
    def loop(block: Header, acc: Seq[Header]): Seq[Header] = {
      if (until(block) || (acc.length == limit)) {
        acc
      } else {
        typedModifierById[Header](block.parentId) match {
          case Some(parent: Header) =>
            loop(parent, acc :+ parent)
          case _ =>
            log.warn(s"No parent header in history for block $block")
            acc
        }
      }
    }

    if (bestHeaderIdOpt.isEmpty || (limit == 0)) HeaderChain(Seq())
    else HeaderChain(loop(startHeader, Seq(startHeader)).reverse)
  }

  private def bestHeadersChainScore: BigInt = scoreOf(bestHeaderIdOpt.get).get

  private def toInsert(h: Header): Seq[(ByteArrayWrapper, ByteArrayWrapper)] = {
    val requiredDifficulty: Difficulty = h.requiredDifficulty
    if (h.isGenesis) {
      Seq(
        ByteArrayWrapper(h.id) -> ByteArrayWrapper(HistoryModifierSerializer.toBytes(h)),
        BestHeaderKey -> ByteArrayWrapper(h.id),
        heightIdsKey(GenesisHeight) -> ByteArrayWrapper(h.id),
        headerHeightKey(h.id) -> ByteArrayWrapper(Ints.toByteArray(GenesisHeight)),
        headerScoreKey(h.id) -> ByteArrayWrapper(requiredDifficulty.toByteArray))
    } else {
      val blockScore = scoreOf(h.parentId).get + requiredDifficulty
      val bestRow: Seq[(ByteArrayWrapper, ByteArrayWrapper)] =
        if (blockScore > bestHeadersChainScore) Seq(BestHeaderKey -> ByteArrayWrapper(h.id)) else Seq()

      val scoreRow = headerScoreKey(h.id) -> ByteArrayWrapper(blockScore.toByteArray)
      val heightRow = headerHeightKey(h.id) -> ByteArrayWrapper(Ints.toByteArray(h.height))
      val headerIdsRow = heightIdsKey(h.height) -> ByteArrayWrapper((headerIdsAtHeight(h.height) :+ h.id).flatten.toArray)
      val modifierRow = ByteArrayWrapper(h.id) -> ByteArrayWrapper(HistoryModifierSerializer.toBytes(h))
      Seq(scoreRow, heightRow, modifierRow, headerIdsRow) ++ bestRow
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
    require(heights.last == parentHeight, s"${heights.last} == $parentHeight")
    if (heights.length == 1) {
      difficultyCalculator.calculate(Seq((parentHeight, parent)))
    } else {
      val chain = headerChainBack(heights.max - heights.min + 1, parent, (h: Header) => false)
      require(chain.length == (heights.min to heights.max).length, s"${chain.length} == ${(heights.min to heights.max).length}")
      val previousHeaders = (heights.min to heights.max).zip(chain.headers).filter(p => heights.contains(p._1))
      require(heights.length == previousHeaders.length, s"Missed headers: $heights != ${previousHeaders.map(_._1)}")
      difficultyCalculator.calculate(previousHeaders)
    }
  }
}