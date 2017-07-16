package org.ergoplatform.nodeView.history

import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.ergoplatform.modifiers.history.{Header, HistoryModifier, HistoryModifierSerializer}
import org.ergoplatform.settings.Algos
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.utils.ScorexLogging
import scorex.crypto.hash.Blake2b256

import scala.util.{Failure, Success}

class HistoryStorage(storage: LSMStore) extends ScorexLogging {

  private val BestHeaderKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(32)(0.toByte))

  def bestHeaderId: Array[Byte] = storage.get(BestHeaderKey).get.data

  def difficultyAt(id: Array[Byte]): Option[BigInt] = storage.get(headerDiffKey(id)).map(b => BigInt(b.data))

  def scoreOf(id: Array[Byte]): Option[BigInt] = storage.get(headerScoreKey(id)).map(b => BigInt(b.data))

  def bestChainScore: BigInt = scoreOf(bestHeaderId).get

  def modifierById(id: ModifierId): Option[HistoryModifier] = storage.get(ByteArrayWrapper(id)).flatMap { bBytes =>
    HistoryModifierSerializer.parseBytes(bBytes.data) match {
      case Success(b) =>
        Some(b)
      case Failure(e) =>
        log.warn("Failed to parse block from db", e)
        None
    }
  }

  def insert(b: HistoryModifier): Unit = {
    //TODO calculate
    val requiredDifficulty: BigInt = 1
    val indexRows = indexes(b, requiredDifficulty)
    storage.update(
      ByteArrayWrapper(b.id),
      Seq(),
      indexRows :+ (ByteArrayWrapper(b.id) -> ByteArrayWrapper(HistoryModifierSerializer.toBytes(b))))
  }

  def drop(id: ModifierId): Unit = {
    storage.update(
      ByteArrayWrapper(Blake2b256(id ++ "drop".getBytes)),
      Seq(ByteArrayWrapper(id), headerScoreKey(id), headerDiffKey(id)),
      Seq())
  }

  private def headerDiffKey(id: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(Algos.hash("diff".getBytes ++ id))

  private def headerScoreKey(id: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(Algos.hash("score".getBytes ++ id))

  private def indexes(mod: HistoryModifier, requiredDifficulty: BigInt): Seq[(ByteArrayWrapper, ByteArrayWrapper)] = {
    mod match {
      case h: Header if h.isGenesis=>
        Seq((BestHeaderKey, ByteArrayWrapper(h.id)),
          (headerScoreKey(h.id), ByteArrayWrapper(requiredDifficulty.toByteArray)),
          (headerDiffKey(h.id), ByteArrayWrapper(requiredDifficulty.toByteArray)))

      case h: Header =>
        val blockScore = scoreOf(h.parentId).get + requiredDifficulty
        val bestRow: Seq[(ByteArrayWrapper, ByteArrayWrapper)] = if (blockScore > bestChainScore) {
          Seq((BestHeaderKey, ByteArrayWrapper(h.id)))
        } else {
          Seq()
        }
        val scoreRow = Seq((headerScoreKey(h.id), ByteArrayWrapper(blockScore.toByteArray)))
        val diffRow = Seq((headerDiffKey(h.id), ByteArrayWrapper(requiredDifficulty.toByteArray)))
        bestRow ++ diffRow ++ scoreRow
      case _ =>
        Seq()
    }
  }

}
