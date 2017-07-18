package org.ergoplatform.nodeView.history.storage

import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.settings.Algos

trait HeadersProcessor {

  protected val storage: LSMStore

  private val BestHeaderKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(32)(Header.ModifierTypeId))

  def bestHeaderId: Array[Byte] = storage.get(BestHeaderKey).get.data

  protected def difficultyAt(id: Array[Byte]): Option[BigInt] = storage.get(headerDiffKey(id)).map(b => BigInt(b.data))

  protected def scoreOf(id: Array[Byte]): Option[BigInt] = storage.get(headerScoreKey(id)).map(b => BigInt(b.data))

  protected def bestHeadersChainScore: BigInt = scoreOf(bestHeaderId).get

  protected def headerDiffKey(id: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(Algos.hash("diff".getBytes ++ id))

  protected def headerScoreKey(id: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(Algos.hash("score".getBytes ++ id))

  def indexes(h: Header,
                       env: ModifierProcessorEnvironment): Seq[(ByteArrayWrapper, ByteArrayWrapper)] = {
    val requiredDifficulty = env.requiredDifficulty
    if (h.isGenesis) {
      Seq((BestHeaderKey, ByteArrayWrapper(h.id)),
        (headerScoreKey(h.id), ByteArrayWrapper(requiredDifficulty.toByteArray)),
        (headerDiffKey(h.id), ByteArrayWrapper(requiredDifficulty.toByteArray)))
    } else {
      val blockScore = scoreOf(h.parentId).get + requiredDifficulty
      val bestRow: Seq[(ByteArrayWrapper, ByteArrayWrapper)] = if (blockScore > bestHeadersChainScore) {
        Seq((BestHeaderKey, ByteArrayWrapper(h.id)))
      } else {
        Seq()
      }
      val scoreRow = Seq((headerScoreKey(h.id), ByteArrayWrapper(blockScore.toByteArray)))
      val diffRow = Seq((headerDiffKey(h.id), ByteArrayWrapper(requiredDifficulty.toByteArray)))
      bestRow ++ diffRow ++ scoreRow
    }
  }

  def idsToDrop(modifier: Header): Seq[ByteArrayWrapper] = {
    //TODO what if we're dropping best block id ??
    val modifierId = modifier.id
    Seq(headerDiffKey(modifierId), headerScoreKey(modifierId))
  }
}
