package org.ergoplatform.nodeView.history

import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.settings.Algos
import scorex.core.utils.ScorexLogging

import scala.util.Try

/**
  * Storage for node indexes and difficulties
  */
class IndexStorage(storage: LSMStore) extends ScorexLogging {
  private val BestHederKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(32)(0.toByte))

  def bestHeaderId: Array[Byte] = storage.get(BestHederKey).get.data

  def difficultyAt(id: Array[Byte]): Option[BigInt] = storage.get(headerDiffKey(id)).map(b => BigInt(b.data))

  def scoreOf(id: Array[Byte]): Option[BigInt] = storage.get(headerScoreKey(id)).map(b => BigInt(b.data))

  def bestChainScore: BigInt = scoreOf(bestHeaderId).get

  /**
    * Calculate and save indexes for header
    *
    * @return true if the best block
    */
  def insert(h: Header, requiredDifficulty: BigInt): Try[Unit] = Try {
    val headerScore: BigInt = requiredDifficulty
    val blockScore = scoreOf(h.parentId).get + headerScore
    val bestRow: Seq[(ByteArrayWrapper, ByteArrayWrapper)] = if (blockScore > bestChainScore) {
      Seq((BestHederKey, ByteArrayWrapper(h.id)))
    } else {
      Seq()
    }
    val scoreRow: Seq[(ByteArrayWrapper, ByteArrayWrapper)] = Seq((headerScoreKey(h.id), ByteArrayWrapper(headerScore.toByteArray)))
    val diffRow = Seq((headerDiffKey(h.id), ByteArrayWrapper(requiredDifficulty.toByteArray)))
    storage.update(ByteArrayWrapper(h.id),
      Seq(),
      bestRow ++ diffRow ++ scoreRow)
    bestRow.nonEmpty
  }

  def headerDiffKey(id: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(Algos.hash("diff".getBytes ++ id))

  def headerScoreKey(id: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(Algos.hash("score".getBytes ++ id))

}
