package org.ergoplatform.nodeView.history.components

import org.ergoplatform.modifiers.{BlockSection, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.ErgoHistory
import scorex.core.consensus.History.ProgressInfo

import scala.util.{Failure, Try}

/**
  * Trait that implements BlockSectionProcessor interfaces for a regime where the node only
  * downloads block headers
  */
trait EmptyBlockSectionProcessor extends BlockSectionProcessor {
  self: NodeProcessor =>

  override protected def process(m: BlockSection): ProgressInfo[ErgoPersistentModifier] =
    ErgoHistory.emptyProgressInfo

  override protected def validate(m: BlockSection): Try[Unit] =
    Failure(new Error("Regime that does not support block sections processing"))

}
