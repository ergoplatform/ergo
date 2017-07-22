package org.ergoplatform.nodeView.history.storage.modifierprocessors

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.{ADProof, BlockTransactions, Header, HistoryModifierSerializer}
import scorex.core.consensus.History.ProgressInfo
import scorex.crypto.encode.Base58

import scala.util.Try

/**
  * ADProof processor for fullnode regime
  */
trait FullnodeADProofsProcessor extends ADProofsProcessor with FullBlockProcessor {


  def process(m: ADProof): ProgressInfo[ErgoPersistentModifier] = {
    historyStorage.modifierById(m.headerId) match {
      case Some(header: Header) =>
        historyStorage.modifierById(header.transactionsId) match {
          case Some(txs: BlockTransactions) =>
            processFullBlock(header, txs, m, txsAreNew = false)
          case _ =>
            val modifierRow = Seq((ByteArrayWrapper(m.id), ByteArrayWrapper(HistoryModifierSerializer.toBytes(m))))
            historyStorage.insert(m.id, modifierRow)
            ProgressInfo(None, Seq(), Seq())
        }
      case _ =>
        throw new Error(s"Header for modifier $m is no defined")
    }
  }


  override def toDrop(modifier: ADProof): Seq[ByteArrayWrapper] = Seq(ByteArrayWrapper(modifier.id))

  override def validate(m: ADProof): Try[Unit] = Try {
    require(!historyStorage.contains(m.id), s"Modifier $m is already in history")
    historyStorage.modifierById(m.headerId) match {
      case Some(h: Header) =>
        require(h.ADProofsRoot sameElements m.digest,
          s"Header ADProofs root ${Base58.encode(h.ADProofsRoot)} differs from $m digest")
      case _ =>
        throw new Error(s"Header for modifier $m is no defined")
    }
  }
}
