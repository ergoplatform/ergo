package org.ergoplatform.nodeView.history.storage

import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.ergoplatform.modifiers.history.{Header, HistoryModifier}
import org.ergoplatform.settings.Algos

class HeadersProcessor(protected val storage: LSMStore) extends ModifiersProcessor {

  private val BestHeaderKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(32)(Header.ModifierTypeId))

  def bestHeaderId: Array[Byte] = storage.get(BestHeaderKey).get.data

  protected def difficultyAt(id: Array[Byte]): Option[BigInt] = storage.get(headerDiffKey(id)).map(b => BigInt(b.data))

  protected def scoreOf(id: Array[Byte]): Option[BigInt] = storage.get(headerScoreKey(id)).map(b => BigInt(b.data))

  protected def bestHeadersChainScore: BigInt = scoreOf(bestHeaderId).get

  protected def headerDiffKey(id: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(Algos.hash("diff".getBytes ++ id))

  protected def headerScoreKey(id: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(Algos.hash("score".getBytes ++ id))

  override def indexes(m: HistoryModifier,
                       env: ModifierProcessorEnvironment): Seq[(ByteArrayWrapper, ByteArrayWrapper)] = {
    val requiredDifficulty = env.requiredDifficulty
    m match {
      case h: Header if h.isGenesis =>
        Seq((BestHeaderKey, ByteArrayWrapper(h.id)),
          (headerScoreKey(h.id), ByteArrayWrapper(requiredDifficulty.toByteArray)),
          (headerDiffKey(h.id), ByteArrayWrapper(requiredDifficulty.toByteArray)))
      case h: Header =>
        val blockScore = scoreOf(h.parentId).get + requiredDifficulty
        val bestRow: Seq[(ByteArrayWrapper, ByteArrayWrapper)] = if (blockScore > bestHeadersChainScore) {
          Seq((BestHeaderKey, ByteArrayWrapper(h.id)))
        } else {
          Seq()
        }
        val scoreRow = Seq((headerScoreKey(h.id), ByteArrayWrapper(blockScore.toByteArray)))
        val diffRow = Seq((headerDiffKey(h.id), ByteArrayWrapper(requiredDifficulty.toByteArray)))
        bestRow ++ diffRow ++ scoreRow
      case _ => Seq()
    }
  }

  override def idsToDrop(modifierId: Array[Byte]): Seq[ByteArrayWrapper] = {
    //TODO what if we're dropping best block id ??
    Seq(headerDiffKey(modifierId), headerScoreKey(modifierId))
  }
}
