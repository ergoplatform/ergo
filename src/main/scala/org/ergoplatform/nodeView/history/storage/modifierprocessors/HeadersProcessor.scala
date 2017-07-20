package org.ergoplatform.nodeView.history.storage.modifierprocessors

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.{Header, HistoryModifierSerializer}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.settings.Algos

import scala.util.Try

trait HeadersProcessor {

  protected val historyStorage: HistoryStorage


//  //TODO .get.asInstanceOf ??
//  def bestHeader: Option[Header] = historyStorage.modifierById(historyStorage.bestHeaderId).get.asInstanceOf[Header]

  protected def difficultyAt(id: Array[Byte]): Option[BigInt] = historyStorage.db.get(headerDiffKey(id)).map(b => BigInt(b.data))

  protected def scoreOf(id: Array[Byte]): Option[BigInt] = historyStorage.db.get(headerScoreKey(id)).map(b => BigInt(b.data))

  protected def bestHeadersChainScore: BigInt = scoreOf(historyStorage.bestHeaderId.get).get

  protected def headerDiffKey(id: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(Algos.hash("diff".getBytes ++ id))

  protected def headerScoreKey(id: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(Algos.hash("score".getBytes ++ id))

  def toInsert(h: Header,
               env: ModifierProcessorEnvironment): Seq[(ByteArrayWrapper, ByteArrayWrapper)] = {
    val requiredDifficulty = env.requiredDifficulty
    if (h.isGenesis) {
      Seq((ByteArrayWrapper(h.id), ByteArrayWrapper(HistoryModifierSerializer.toBytes(h))),
        (historyStorage.BestHeaderKey, ByteArrayWrapper(h.id)),
        (headerScoreKey(h.id), ByteArrayWrapper(requiredDifficulty.toByteArray)),
        (headerDiffKey(h.id), ByteArrayWrapper(requiredDifficulty.toByteArray)))
    } else {
      val blockScore = scoreOf(h.parentId).get + requiredDifficulty
      val bestRow: Seq[(ByteArrayWrapper, ByteArrayWrapper)] = if (blockScore > bestHeadersChainScore) {
        Seq((historyStorage.BestHeaderKey, ByteArrayWrapper(h.id)))
      } else {
        Seq()
      }
      val scoreRow = Seq((headerScoreKey(h.id), ByteArrayWrapper(blockScore.toByteArray)))
      val diffRow = Seq((headerDiffKey(h.id), ByteArrayWrapper(requiredDifficulty.toByteArray)))
      Seq((ByteArrayWrapper(h.id), ByteArrayWrapper(HistoryModifierSerializer.toBytes(h)))) ++ bestRow ++ diffRow ++ scoreRow
    }
  }

  def toDrop(modifier: Header): Seq[ByteArrayWrapper] = {
    //TODO what if we're dropping best block id ??
    val modifierId = modifier.id
    Seq(headerDiffKey(modifierId), headerScoreKey(modifierId))
  }

  def validate(m: Header): Try[Unit] = Try {
    if (m.isGenesis) {
      require(historyStorage.bestHeaderId.isEmpty, "Trying to append genesis block to non-empty history")
    } else {
      val parentOpt = historyStorage.modifierById(m.parentId)
      require(parentOpt.isDefined, "Parent header is no defined")
      require(!historyStorage.contains(m.id), "Header is already in history")
      //TODO require(Algos.blockIdDifficulty(m.headerHash) >= difficulty, "Block difficulty is not enough")
      //TODO check timestamp
    }
  }
}
