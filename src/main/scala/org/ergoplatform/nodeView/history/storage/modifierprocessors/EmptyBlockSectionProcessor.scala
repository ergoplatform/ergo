package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.consensus.ProgressInfo
import org.ergoplatform.modifiers.{BlockSection, NonHeaderBlockSection}

import scala.util.{Failure, Success, Try}

/**
  * Trait that implements BlockSectionProcessor interfaces for a regime where the node is only
  * downloading block headers
  */
trait EmptyBlockSectionProcessor extends BlockSectionProcessor {

  override protected def process(m: NonHeaderBlockSection): Try[ProgressInfo[BlockSection]] =
    Success(ProgressInfo[BlockSection](None, Seq.empty, Seq.empty, Seq.empty))

  override protected def validate(m: NonHeaderBlockSection): Try[Unit] =
    Failure(new Error("Regime that does not support block sections processing"))

}
