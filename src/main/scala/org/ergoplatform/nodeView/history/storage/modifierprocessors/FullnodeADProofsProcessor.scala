package org.ergoplatform.nodeView.history.storage.modifierprocessors

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.{ADProofs, Header}
import scorex.core.consensus.History.ProgressInfo
import scorex.crypto.encode.Base58

import scala.util.Try

/**
  * ADProof processor for fullnode regime
  */
trait FullnodeADProofsProcessor extends ADProofsProcessor with FullBlockProcessor {


  def process(m: ADProofs): ProgressInfo[ErgoPersistentModifier] = {
    //    Seq((ByteArrayWrapper(m.id), ByteArrayWrapper(HistoryModifierSerializer.toBytes(m))))
    ???
  }


  override def toDrop(modifier: ADProofs): Seq[ByteArrayWrapper] = Seq(ByteArrayWrapper(modifier.id))

  override def validate(m: ADProofs): Try[Unit] = Try {
    require(!historyStorage.contains(m.id), s"Modifier $m is already in history")
    historyStorage.modifierById(m.headerId) match {
      case Some(h: Header) =>
        require(h.ADProofsRoot sameElements m.id,
          s"Header ADProofs root ${Base58.encode(h.transactionsRoot)} differs from $m id")
      case _ =>
        throw new Error(s"Header for modifier $m is no defined")
    }
  }
}
