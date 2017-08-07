package org.ergoplatform.nodeView.history.storage.modifierprocessors.popow

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.PoPoWProof
import scorex.core.consensus.History.ProgressInfo

import scala.util.{Failure, Try}

trait EmptyPoPoWProofsProcessor extends PoPoWProofsProcessor {

  def validate(m: PoPoWProof): Try[Unit] = Failure(new Error("Regime that do not process PoPoWProof"))

  def process(m: PoPoWProof): ProgressInfo[ErgoPersistentModifier] = ProgressInfo(None, Seq(), Seq())
}

