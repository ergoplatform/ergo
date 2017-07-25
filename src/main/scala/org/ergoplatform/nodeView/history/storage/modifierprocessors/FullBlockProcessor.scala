package org.ergoplatform.nodeView.history.storage.modifierprocessors

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.settings.Constants.hashLength
import scorex.core.NodeViewModifier._
import scorex.core.consensus.History.ProgressInfo
import scorex.core.utils.ScorexLogging

trait FullBlockProcessor extends HeadersProcessor with ScorexLogging {

  val BestFullBlockKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(hashLength)(-1))

  override def bestFullBlockId: Option[ModifierId] = historyStorage.db.get(BestFullBlockKey).map(_.data)

  def bestFullBlockOpt: Option[ErgoFullBlock]

  def commonBlockThenSuffixes(header1: Header, header2: Header): (HeaderChain, HeaderChain)

  protected def getFullBlock(h: Header): ErgoFullBlock

  protected def processFullBlock(header: Header,
                                 txs: BlockTransactions,
                                 adProofs: ADProof,
                                 txsAreNew: Boolean): ProgressInfo[ErgoPersistentModifier] = {
    val newModRow = if (txsAreNew) {
      (ByteArrayWrapper(txs.id), ByteArrayWrapper(HistoryModifierSerializer.toBytes(txs)))
    } else {
      (ByteArrayWrapper(adProofs.id), ByteArrayWrapper(HistoryModifierSerializer.toBytes(adProofs)))
    }
    val storageVersion = if (txsAreNew) txs.id else adProofs.id
    val fullBlock = ErgoFullBlock(header, txs, adProofs)
    (bestFullBlockOpt, bestFullBlockId.flatMap(scoreOf), scoreOf(header.id)) match {
      case (Some(pevBest), _, _) if header.parentId sameElements pevBest.header.id =>
        log.info(s"New best header ${header.encodedId} with transactions and proofs at the end of the chain")
        bestBlockToTheEnd(newModRow, storageVersion, fullBlock)
      case (Some(pevBest), Some(prevBestScore), Some(curentScore)) if curentScore >= prevBestScore =>
        log.info(s"Process fork for new best header ${header.encodedId} with transactions and proofs")
        historyStorage.insert(storageVersion, Seq(newModRow, (BestFullBlockKey, ByteArrayWrapper(fullBlock.header.id))))
        val (prevChain, newChain) = commonBlockThenSuffixes(pevBest.header, header)
        assert(prevChain.head == newChain.head)
        val toRemove: Seq[ErgoFullBlock] = prevChain.tail.headers.map(getFullBlock)
        val toApply: Seq[ErgoFullBlock] = newChain.tail.headers.map(getFullBlock)
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

  private def bestBlockToTheEnd(newModRow: (ByteArrayWrapper, ByteArrayWrapper),
                                storageVersion: ModifierId,
                                fullBlock: ErgoFullBlock): ProgressInfo[ErgoPersistentModifier] = {
    historyStorage.insert(storageVersion, Seq(newModRow, (BestFullBlockKey, ByteArrayWrapper(fullBlock.header.id))))
    ProgressInfo(None, Seq(), Seq(fullBlock))
  }
}
