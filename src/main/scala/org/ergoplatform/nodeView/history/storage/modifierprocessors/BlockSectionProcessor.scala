package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.modifiers.{NonHeaderBlockSection, BlockSection}
import scorex.core.consensus.ProgressInfo
import scorex.core.utils.ScorexEncoding

import scala.util.Try

/**
  * Trait that declares interfaces for validation and processing of various
  * block sections: BlockTransactions, ADProofs, etc.
  */
trait BlockSectionProcessor extends ScorexEncoding {

  /**
    * Whether state requires to download adProofs before full block application
    */
  protected def requireProofs: Boolean

  /**
    * @param m - modifier to process
    * @return ProgressInfo - info required for State to be consistent with History
    */
  protected def process(m: NonHeaderBlockSection): Try[ProgressInfo[BlockSection]]

  /**
    * @param m - modifier to validate
    * @return Success() if modifier is valid from History point of view, Failure(error) otherwise
    */
  protected def validate(m: NonHeaderBlockSection): Try[Unit]

}
