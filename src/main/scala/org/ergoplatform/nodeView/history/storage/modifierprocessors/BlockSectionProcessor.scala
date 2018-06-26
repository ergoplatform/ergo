package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.modifiers.{BlockSection, ErgoPersistentModifier}
import scorex.core.consensus.History.ProgressInfo
import scorex.core.utils.ScorexEncoding

import scala.util.Try

trait BlockSectionProcessor extends ScorexEncoding {

  /**
    * @param m - modifier to process
    * @return ProgressInfo - info required for State to be consistent with History
    */
  protected def process(m: BlockSection): ProgressInfo[ErgoPersistentModifier]

  /**
    * @param m - modifier to validate
    * @return Success() if modifier is valid from History point of view, Failure(error) otherwise
    */
  protected def validate(m: BlockSection): Try[Unit]

}
