package org.ergoplatform.nodeView.history.storage.modifierprocessors

import com.google.common.primitives.Ints
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.{Header, HeaderChain, HistoryModifierSerializer}
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.HistoryConfig
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.settings.Algos
import org.ergoplatform.settings.Constants.hashLength
import scorex.core.NodeViewModifier._
import scorex.core.consensus.History.ProgressInfo
import scorex.core.utils.ScorexLogging

import scala.util.Try

trait HeadersProcessor extends ScorexLogging {

  protected val config: HistoryConfig

  def bestFullBlockOpt: Option[ErgoFullBlock]

  def typedModifierById[T <: ErgoPersistentModifier](id: ModifierId): Option[T]

  protected def commonBlockThenSuffixes(header1: Header, header2: Header): (HeaderChain, HeaderChain)

  protected def getFullBlock(h: Header): ErgoFullBlock

  /**
    * Id of best header with transactions and proofs. None in regime that do not process transactions
    */
  def bestFullBlockIdOpt: Option[ModifierId] = None

  protected val historyStorage: HistoryStorage

  protected val BestHeaderKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(hashLength)(Header.ModifierTypeId))

  protected val BestFullBlockKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(hashLength)(-1))

  def bestHeaderIdOpt: Option[ModifierId] = historyStorage.db.get(BestHeaderKey).map(_.data)

  protected def difficultyAt(id: ModifierId): Option[BigInt] = historyStorage.db.get(headerDiffKey(id)).map(b => BigInt(b.data))

  protected def scoreOf(id: ModifierId): Option[BigInt] = historyStorage.db.get(headerScoreKey(id)).map(b => BigInt(b.data))

  def height: Int = bestHeaderIdOpt.flatMap(id => heightOf(id)).getOrElse(0)

  def heightOf(id: ModifierId): Option[Int] = historyStorage.db.get(headerHeightKey(id))
    .map(b => Ints.fromByteArray(b.data))

  protected def headerIdsAtHeight(height: Int): Seq[ModifierId] = historyStorage.db.get(heightIdsKey(height: Int))
    .map(_.data).getOrElse(Array()).grouped(32).toSeq

  private def bestHeadersChainScore: BigInt = scoreOf(bestHeaderIdOpt.get).get

  protected def headerDiffKey(id: ModifierId): ByteArrayWrapper = ByteArrayWrapper(Algos.hash("diff".getBytes ++ id))

  protected def headerScoreKey(id: ModifierId): ByteArrayWrapper = ByteArrayWrapper(Algos.hash("score".getBytes ++ id))

  protected def headerHeightKey(id: ModifierId): ByteArrayWrapper = ByteArrayWrapper(Algos.hash("height".getBytes ++ id))

  private def heightIdsKey(height: Int): ByteArrayWrapper = ByteArrayWrapper(Algos.hash(Ints.toByteArray(height)))

  def process(m: Header): ProgressInfo[ErgoPersistentModifier] = {
    val dataToInsert = toInsert(m)
    historyStorage.insert(m.id, dataToInsert)
    if (bestHeaderIdOpt.isEmpty || (bestHeaderIdOpt.get sameElements m.id)) {
      log.info(s"New best header ${m.encodedId}")
      //TODO Notify node view holder that it should download transactions ?
      ProgressInfo(None, Seq(), Seq(m))
    } else {
      log.info(s"New orphaned header ${m.encodedId}, best: ${}")
      ProgressInfo(None, Seq(), Seq())
    }
  }

  def toInsert(h: Header): Seq[(ByteArrayWrapper, ByteArrayWrapper)] = {
    val requiredDifficulty: BigInt = expectedDifficulty(h)
    if (h.isGenesis) {
      Seq((ByteArrayWrapper(h.id), ByteArrayWrapper(HistoryModifierSerializer.toBytes(h))),
        (BestHeaderKey, ByteArrayWrapper(h.id)),
        (headerHeightKey(h.id), ByteArrayWrapper(Ints.toByteArray(1))),
        (headerScoreKey(h.id), ByteArrayWrapper(requiredDifficulty.toByteArray)),
        (headerDiffKey(h.id), ByteArrayWrapper(requiredDifficulty.toByteArray)))
    } else {
      val blockScore = scoreOf(h.parentId).get + requiredDifficulty
      val bestRow: Seq[(ByteArrayWrapper, ByteArrayWrapper)] = if (blockScore > bestHeadersChainScore) {
        Seq((BestHeaderKey, ByteArrayWrapper(h.id)))
      } else {
        Seq()
      }
      val scoreRow = (headerScoreKey(h.id), ByteArrayWrapper(blockScore.toByteArray))
      val height = heightOf(h.parentId).get + 1
      val heightRow = (headerHeightKey(h.id), ByteArrayWrapper(Ints.toByteArray(height)))
      val headerIdsRow: (ByteArrayWrapper, ByteArrayWrapper) = (heightIdsKey(height),
        ByteArrayWrapper((headerIdsAtHeight(height) :+ h.id).flatten.toArray))
      val diffRow = (headerDiffKey(h.id), ByteArrayWrapper(requiredDifficulty.toByteArray))
      val modifierRow = (ByteArrayWrapper(h.id), ByteArrayWrapper(HistoryModifierSerializer.toBytes(h)))
      Seq(diffRow, scoreRow, heightRow, modifierRow, headerIdsRow) ++ bestRow
    }
  }

  def toDrop(header: Header): (Seq[ByteArrayWrapper], Seq[(ByteArrayWrapper, ByteArrayWrapper)]) = {
    val modifierId = header.id
    val payloadModifiers = Seq(header.transactionsId, header.ADProofsId).filter(id => historyStorage.contains(id))
      .map(id => ByteArrayWrapper(id))

    val toRemove = Seq(headerDiffKey(modifierId),
      headerScoreKey(modifierId),
      ByteArrayWrapper(modifierId)) ++ payloadModifiers
    val bestHeaderKeyUpdate = if (bestHeaderIdOpt.exists(_ sameElements modifierId)) {
      Seq((BestHeaderKey, ByteArrayWrapper(header.parentId)))
    } else Seq()
    val bestFullBlockKeyUpdate = if (bestFullBlockIdOpt.exists(_ sameElements modifierId)) {
      Seq((BestFullBlockKey, ByteArrayWrapper(header.parentId)))
    } else Seq()
    (toRemove, bestFullBlockKeyUpdate ++ bestHeaderKeyUpdate)
  }

  //todo: avoid using require/ensuring, as they can be turned off by a compiler flag
  //todo: also, their intended semantics is different from what we're doing in this method
  def validate(m: Header): Try[Unit] = Try {
    if (m.isGenesis) {
      require(bestHeaderIdOpt.isEmpty, "Trying to append genesis block to non-empty history")
    } else {
      val parentOpt = historyStorage.modifierById(m.parentId)
      require(parentOpt.isDefined, "Parent header is no defined")
      require(!historyStorage.contains(m.id), "Header is already in history")
      require(Algos.blockIdDifficulty(m.powHash) >= expectedDifficulty(m),
        s"Block difficulty ${Algos.blockIdDifficulty(m.powHash)} is less than required ${expectedDifficulty(m)}")
      //TODO check timestamp
      //TODO check that block is not too old to prevent DDoS
    }
  }

  private val difficultyCalculator = new DifficultyCalculator

  def expectedDifficulty(h: Header): BigInt = {
    if (h.isGenesis) {
      BigInt(Algos.initialDifficulty)
    } else if (difficultyCalculator.recalculationRequired(heightOf(h.parentId).get)) {
      ???
    } else {
      difficultyAt(h.parentId).get
    }
  }
}
