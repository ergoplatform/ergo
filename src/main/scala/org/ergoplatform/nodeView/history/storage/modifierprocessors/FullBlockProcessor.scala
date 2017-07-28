package org.ergoplatform.nodeView.history.storage.modifierprocessors

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.HistoryConfig
import org.ergoplatform.settings.Algos
import org.ergoplatform.settings.Constants.hashLength
import scorex.core.NodeViewModifier._
import scorex.core.consensus.History.ProgressInfo
import scorex.core.utils.ScorexLogging

import scala.util.Try

trait FullBlockProcessor extends HeadersProcessor with ScorexLogging {

  val BestFullBlockKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(hashLength)(-1))

  override def bestFullBlockId: Option[ModifierId] = historyStorage.db.get(BestFullBlockKey).map(_.data)

  protected val config: HistoryConfig

  def bestFullBlockOpt: Option[ErgoFullBlock]

  def typedModifierById[T <: ErgoPersistentModifier](id: ModifierId): Option[T]

  def commonBlockThenSuffixes(header1: Header, header2: Header): (HeaderChain, HeaderChain)

  protected def getFullBlock(h: Header): ErgoFullBlock

  protected def processFullBlock(header: Header,
                                 txs: BlockTransactions,
                                 adProofsOpt: Option[ADProof],
                                 txsAreNew: Boolean): ProgressInfo[ErgoPersistentModifier] = {
    assert(adProofsOpt.isDefined || txsAreNew, "Only transactions can be new when proofs are empty")
    val newModRow = if (txsAreNew) {
      (ByteArrayWrapper(txs.id), ByteArrayWrapper(HistoryModifierSerializer.toBytes(txs)))
    } else {
      (ByteArrayWrapper(adProofsOpt.get.id), ByteArrayWrapper(HistoryModifierSerializer.toBytes(adProofsOpt.get)))
    }
    val storageVersion = if (txsAreNew) txs.id else adProofsOpt.get.id
    val fullBlock = ErgoFullBlock(header, txs, adProofsOpt)
    (bestFullBlockOpt, bestFullBlockId.flatMap(scoreOf), scoreOf(header.id)) match {
      case (Some(pevBest), _, _) if header.parentId sameElements pevBest.header.id =>
        log.info(s"New best header ${header.encodedId} with transactions and proofs at the end of the chain")
        if (config.blocksToKeep >= 0) pruneOnNewBestBlock(header)
        bestBlockToTheEnd(newModRow, storageVersion, fullBlock)
      case (Some(pevBest), Some(prevBestScore), Some(curentScore)) if curentScore >= prevBestScore =>
        log.info(s"Process fork for new best header ${header.encodedId} with transactions and proofs")
        historyStorage.insert(storageVersion, Seq(newModRow, (BestFullBlockKey, ByteArrayWrapper(fullBlock.header.id))))
        val (prevChain, newChain) = commonBlockThenSuffixes(pevBest.header, header)
        assert(prevChain.head == newChain.head)
        val toRemove: Seq[ErgoFullBlock] = prevChain.tail.headers.map(getFullBlock)
        val toApply: Seq[ErgoFullBlock] = newChain.tail.headers.map(getFullBlock)
        assert(toRemove.nonEmpty)
        assert(toApply.nonEmpty)
        if (config.blocksToKeep >= 0) {
          val bestHeight: Int = heightOf(toApply.last.header.id).get
          lazy val toClean = (bestHeight - config.blocksToKeep - toApply.length) until (bestHeight - config.blocksToKeep)
          if (bestHeight > config.blocksToKeep) pruneBlockDataAt(toClean)
        }
        ProgressInfo(Some(prevChain.head.id), toRemove, toApply)
      case (None, _, _) =>
        log.info(s"Initialize full chain with new best header ${header.encodedId} with transactions and proofs")
        bestBlockToTheEnd(newModRow, storageVersion, fullBlock)
      case _ =>
        log.info(s"Got transactions and proofs for non-best header ${header.encodedId}")
        historyStorage.insert(storageVersion, Seq(newModRow))
        ProgressInfo(None, Seq(), Seq())
    }
  }

  private def pruneOnNewBestBlock(header: Header): Unit = heightOf(header.id).filter(h => h > config.blocksToKeep)
    .foreach(h => pruneBlockDataAt(Seq(h - config.blocksToKeep)))

  private def pruneBlockDataAt(heights: Seq[Int]): Try[Unit] = Try {
    val id: ModifierId = Algos.hash(heights.flatMap(_.toString.getBytes).toArray)
    val toRemove: Seq[ByteArrayWrapper] = heights.flatMap(h => headerIdsAtHeight(h))
      .flatMap { id => typedModifierById[Header](id) }
      .flatMap { h =>
        Seq(ByteArrayWrapper(h.ADProofsId), ByteArrayWrapper(h.transactionsId))
      }
    historyStorage.remove(id, toRemove)
  }

  private def bestBlockToTheEnd(newModRow: (ByteArrayWrapper, ByteArrayWrapper),
                                storageVersion: ModifierId,
                                fullBlock: ErgoFullBlock): ProgressInfo[ErgoPersistentModifier] = {
    historyStorage.insert(storageVersion, Seq(newModRow, (BestFullBlockKey, ByteArrayWrapper(fullBlock.header.id))))
    ProgressInfo(None, Seq(), Seq(fullBlock))
  }
}
