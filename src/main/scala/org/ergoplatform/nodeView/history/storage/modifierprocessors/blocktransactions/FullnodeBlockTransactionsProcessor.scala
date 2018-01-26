package org.ergoplatform.nodeView.history.storage.modifierprocessors.blocktransactions

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header, HistoryModifierSerializer}
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.history.storage.modifierprocessors.FullBlockProcessor
import scorex.core.consensus.History.ProgressInfo
import scorex.crypto.encode.Base58

import scala.util.{Failure, Success, Try}

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
          case _ if !header.isGenesis && bestFullBlockIdOpt.isEmpty =>
            //TODO light mode when start from different block ?
            justPutToHistory(txs)
          case Some(adProof: ADProofs) =>
            processFullBlock(ErgoFullBlock(header, txs, Some(adProof)), txsAreNew = true)
          case None if !adState =>
            processFullBlock(ErgoFullBlock(header, txs, None), txsAreNew = true)
          case _ =>
            justPutToHistory(txs)
        }
      case _ =>
        throw new Error(s"Header for modifier $txs is no defined")
    }
  }

  private def justPutToHistory(txs: BlockTransactions):ProgressInfo[ErgoPersistentModifier] = {
    historyStorage.insert(ByteArrayWrapper(txs.id), Seq(), Seq(txs))
    ProgressInfo(None, Seq(), None, Seq())
  }

  override protected def validate(m: BlockTransactions): Try[Unit] = {
    if(historyStorage.contains(m.id)) {
      Failure(new Error(s"Modifier $m is already in history"))
    } else {
      historyStorage.modifierById(m.headerId) match {
        case None =>
          Failure(new Error(s"Header for modifier $m is no defined"))
        case Some(header: Header) if !(header.transactionsRoot sameElements m.digest) =>
          Failure(new Error(s"Header transactions root ${Base58.encode(header.ADProofsRoot)} differs from $m digest"))
        case Some(header: Header) =>
          Success()
      }
    }
  }
}
