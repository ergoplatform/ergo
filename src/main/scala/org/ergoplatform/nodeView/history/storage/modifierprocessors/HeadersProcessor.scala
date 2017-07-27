package org.ergoplatform.nodeView.history.storage.modifierprocessors

import com.google.common.primitives.Ints
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.{Header, HistoryModifierSerializer}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.settings.Algos
import org.ergoplatform.settings.Constants.hashLength
import scorex.core.NodeViewModifier._

import scala.util.Try

trait HeadersProcessor {

  /**
    * Id of best header with transactions and proofs. None in regime that do not process transactions
    */
  def bestFullBlockId: Option[ModifierId] = None

  protected val historyStorage: HistoryStorage

  private val BestHeaderKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(hashLength)(Header.ModifierTypeId))

  def bestHeaderIdOpt: Option[ModifierId] = historyStorage.db.get(BestHeaderKey).map(_.data)

  protected def difficultyAt(id: Array[Byte]): Option[BigInt] = historyStorage.db.get(headerDiffKey(id)).map(b => BigInt(b.data))

  protected def scoreOf(id: Array[Byte]): Option[BigInt] = historyStorage.db.get(headerScoreKey(id)).map(b => BigInt(b.data))

  def heightOf(id: Array[Byte]): Option[Int] = historyStorage.db.get(headerHeightKey(id))
    .map(b => Ints.fromByteArray(b.data))

  protected def headerIdsAtHeight(height: Int): Seq[ModifierId] = historyStorage.db.get(heightIdsKey(height: Int))
    .map(_.data).getOrElse(Array()).grouped(32).toSeq

  private def bestHeadersChainScore: BigInt = scoreOf(bestHeaderIdOpt.get).get

  private def headerDiffKey(id: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(Algos.hash("diff".getBytes ++ id))

  private def headerScoreKey(id: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(Algos.hash("score".getBytes ++ id))

  private def headerHeightKey(id: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(Algos.hash("height".getBytes ++ id))

  private def heightIdsKey(height: Int): ByteArrayWrapper = ByteArrayWrapper(Algos.hash(Ints.toByteArray(height)))

  def toInsert(h: Header): Seq[(ByteArrayWrapper, ByteArrayWrapper)] = {
    val requiredDifficulty: BigInt = calculateDifficulty(h)
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

  def toDrop(modifier: Header): Seq[ByteArrayWrapper] = {
    //TODO what if we're dropping best block id ??
    val modifierId = modifier.id
    Seq(headerDiffKey(modifierId), headerScoreKey(modifierId))
  }

  def validate(m: Header): Try[Unit] = Try {
    if (m.isGenesis) {
      require(bestHeaderIdOpt.isEmpty, "Trying to append genesis block to non-empty history")
    } else {
      val parentOpt = historyStorage.modifierById(m.parentId)
      require(parentOpt.isDefined, "Parent header is no defined")
      require(!historyStorage.contains(m.id), "Header is already in history")
      require(Algos.blockIdDifficulty(m.headerHash) >= calculateDifficulty(m),
        s"Block difficulty ${Algos.blockIdDifficulty(m.headerHash)} is less than required ${calculateDifficulty(m)}")
      //TODO check timestamp
      //TODO check that block is not too old to prevent DDoS
    }
  }

  private val difficultyCalculator = new DifficultyCalculator

  def calculateDifficulty(h: Header): BigInt = {
    if (h.isGenesis) {
      BigInt(1)
    } else if (difficultyCalculator.recalculationRequired(heightOf(h.parentId).get)) {
      ???
    } else {
      difficultyAt(h.parentId).get
    }
  }
}
