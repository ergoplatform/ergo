package org.ergoplatform.nodeView.history.storage.modifierprocessors.blocktransactions

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.BlockTransactions
import scorex.core.consensus.History.ProgressInfo
import scorex.core.utils.ScorexLogging

import scala.util.Try

trait BlockTransactionsProcessor extends ScorexLogging {

  def toDrop(modifier: BlockTransactions): Seq[ByteArrayWrapper]

  def validate(m: BlockTransactions): Try[Unit]

  def process(m: BlockTransactions): ProgressInfo[ErgoPersistentModifier]
}

