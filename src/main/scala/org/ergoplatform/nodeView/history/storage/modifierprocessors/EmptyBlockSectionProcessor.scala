package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.modifiers.{BlockSection, ErgoPersistentModifier}
import scorex.core.consensus.History.ProgressInfo

import scala.util.{Failure, Success, Try}

/**
  * Trait that implements BlockSectionProcessor interfaces for a regime where the node only
  * downloads block headers
  */
trait EmptyBlockSectionProcessor extends BlockSectionProcessor {

  override protected def process(m: BlockSection): Try[ProgressInfo[ErgoPersistentModifier]] =
    Success(ProgressInfo[ErgoPersistentModifier](None, Seq.empty, Seq.empty, Seq.empty))

  override protected def validate(m: BlockSection): Try[Unit] =
    Failure(new Error("Regime that does not support block sections processing"))

}
