package org.ergoplatform.nodeView.history.storage.modifierprocessors.popow

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.PoPowProof
import org.ergoplatform.nodeView.history.storage.modifierprocessors.HeadersProcessor
import scorex.core.consensus.History.ProgressInfo
import scorex.util.ScorexLogging

import scala.util.{Failure, Try}

/**
  * Contains all functions required by History to process PoPoWProofs for regime that do not accept them.
  */
trait EmptyPoPoWProofsProcessor extends PoPoWProofsProcessor {
  self: HeadersProcessor with ScorexLogging =>

  def validate(m: PoPowProof): Try[Unit] =
    Failure(new Error("Regime that do not process PoPoWProof"))

  def process(m: PoPowProof): ProgressInfo[ErgoPersistentModifier] =
    ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)

}

