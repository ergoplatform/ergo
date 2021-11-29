package org.ergoplatform.nodeView.history.storage.modifierprocessors.popow

import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.history.NipopowProofModifier
import org.ergoplatform.nodeView.history.storage.modifierprocessors.HeadersProcessor
import scorex.core.consensus.History.ProgressInfo

import scala.util.{Success, Try}

/**
  * Contains all functions required by History to process PoPoWProofs for regime that accept them.
  */
trait FullPoPoWProofsProcessor extends PoPoWProofsProcessor with HeadersProcessor {

  def validate(m: NipopowProofModifier): Try[Unit] = throw new Error("PoPow not yet supported")

  def process(m: NipopowProofModifier): Try[ProgressInfo[BlockSection]] =
    Success(ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty))
}

