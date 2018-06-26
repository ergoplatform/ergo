package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.modifiers.{BlockSection, ErgoPersistentModifier}
import scorex.core.consensus.History.ProgressInfo

import scala.util.{Failure, Try}

trait EmptyBlockSectionProcessor extends BlockSectionProcessor {

  override protected def process(m: BlockSection): ProgressInfo[ErgoPersistentModifier] =
    ProgressInfo[ErgoPersistentModifier](None, Seq.empty, Seq.empty, Seq.empty)

  override protected def validate(m: BlockSection): Try[Unit] =
    Failure(new Error("Regime that does not support block sections processing"))

}
