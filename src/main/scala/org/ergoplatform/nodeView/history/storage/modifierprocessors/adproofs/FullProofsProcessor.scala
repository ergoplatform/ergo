package org.ergoplatform.nodeView.history.storage.modifierprocessors.adproofs

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header, HistoryModifierSerializer}
import org.ergoplatform.nodeView.history.storage.modifierprocessors.FullBlockProcessor
import scorex.core.consensus.History.ProgressInfo
import scorex.crypto.encode.Base58

import scala.util.Try

/**
  * ADProof processor for node regime with DigestState
  */
trait FullProofsProcessor extends ADProofsProcessor with FullBlockProcessor {

  protected val adState: Boolean

  override protected def process(m: ADProofs): ProgressInfo[ErgoPersistentModifier] = {
    historyStorage.modifierById(m.headerId) match {
      case Some(header: Header) =>
        historyStorage.modifierById(header.transactionsId) match {
          case Some(txs: BlockTransactions) if adState =>
            processFullBlock(ErgoFullBlock(header, txs, Some(m)), txsAreNew = false)
          case _ =>
            val modifierRow = Seq((ByteArrayWrapper(m.id), ByteArrayWrapper(HistoryModifierSerializer.toBytes(m))))
            historyStorage.insert(m.id, modifierRow)
            ProgressInfo(None, Seq(), None, Seq())
        }
      case _ =>
        throw new Error(s"Header for modifier $m is no defined")
    }
  }

  override protected def validate(m: ADProofs): Try[Unit] = Try {
    require(!historyStorage.contains(m.id), s"Modifier $m is already in history")
    historyStorage.modifierById(m.headerId) match {
      case Some(header: Header) =>
        require(header.ADProofsRoot sameElements m.digest,
          s"Header ADProofs root ${Base58.encode(header.ADProofsRoot)} differs from $m digest")
        if(!header.isGenesis && adState) {
          require(typedModifierById[Header](header.parentId).exists(h => contains(h.ADProofsId)),
            s"Trying to apply proofs ${m.encodedId} for header ${header.encodedId}, which parent proofs are empty")
        }
      case _ =>
        throw new Error(s"Header for modifier $m is no defined")
    }
  }
}
