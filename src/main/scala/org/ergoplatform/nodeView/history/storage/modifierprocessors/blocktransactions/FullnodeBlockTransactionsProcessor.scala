package org.ergoplatform.nodeView.history.storage.modifierprocessors.blocktransactions

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header, HistoryModifierSerializer}
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.history.storage.modifierprocessors.FullBlockProcessor
import scorex.core.consensus.History.ProgressInfo
import scorex.crypto.encode.Base58

import scala.util.Try

/**
  * BlockTransactions processor for settings with verifyTransactions=true
  */
trait FullnodeBlockTransactionsProcessor extends BlockTransactionsProcessor with FullBlockProcessor {
  protected val historyStorage: HistoryStorage

  protected val adState: Boolean

  override protected def process(txs: BlockTransactions): ProgressInfo[ErgoPersistentModifier] = {
    historyStorage.modifierById(txs.headerId) match {
      case Some(header: Header) =>
        historyStorage.modifierById(header.ADProofsId) match {
          case Some(adProof: ADProofs) =>
            processFullBlock(ErgoFullBlock(header, txs, Some(adProof)), txsAreNew = true)
          case None if !adState =>
            processFullBlock(ErgoFullBlock(header, txs, None), txsAreNew = true)
          case _ =>
            val modifierRow = Seq((ByteArrayWrapper(txs.id), ByteArrayWrapper(HistoryModifierSerializer.toBytes(txs))))
            historyStorage.insert(txs.id, modifierRow)
            ProgressInfo(None, Seq(), Seq(), Seq())
        }
      case _ =>
        throw new Error(s"Header for modifier $txs is no defined")
    }
  }

  override protected def validate(m: BlockTransactions): Try[Unit] = Try {
    require(!historyStorage.contains(m.id), s"Modifier $m is already in history")
    historyStorage.modifierById(m.headerId) match {
      case Some(header: Header) =>
        require(header.transactionsRoot sameElements m.digest,
          s"Header transactions root ${Base58.encode(header.transactionsRoot)} differs from block transactions $m digest")

        bestFullBlockIdOpt match {
          case None if config.blocksToKeep < 0 =>
            require(header.isGenesis, "Trying to apply non-genesis block to empty history in fullnode mode")
          case None if config.blocksToKeep >= 0 =>
            //TODO State should be at this version!
          case Some(id) =>
            val parentFull = typedModifierById[Header](header.parentId).flatMap(h => getFullBlock(h))
            require(parentFull.isDefined, "Trying to apply transactions for header, which parent transactions are empty")
        }

      case _ =>
        throw new Error(s"Header for modifier $m is no defined")
    }
  }
}
