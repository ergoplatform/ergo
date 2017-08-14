package org.ergoplatform.nodeView.history.storage.modifierprocessors.blocktransactions

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.BlockTransactions
import scorex.core.consensus.History.ProgressInfo
import scorex.core.utils.ScorexLogging

import scala.util.Try

/**
  * Contains all functions required by History to process BlockTransactions.
  */
trait BlockTransactionsProcessor extends ScorexLogging {

  /**
    * @param modifier - modifier we're going to remove from history
    * @return ids to remove, new data to apply
    */
  protected def toDrop(modifier: BlockTransactions): Seq[ByteArrayWrapper]

  /**
    * @param m - BlockTransactions to validate
    * @return Succes() if BlockTransactions is valid from History point of view, Failure(error) otherwise
    */
  protected def validate(m: BlockTransactions): Try[Unit]

  /**
    * @param m - modifier to process
    * @return ProgressInfo - info required for State to be consistent with History
    */
  protected def process(m: BlockTransactions): ProgressInfo[ErgoPersistentModifier]
}

