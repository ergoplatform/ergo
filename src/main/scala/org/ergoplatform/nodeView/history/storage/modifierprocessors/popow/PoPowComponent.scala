package org.ergoplatform.nodeView.history.storage.modifierprocessors.popow

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.PoPowProof
import org.ergoplatform.settings.PoPowParams
import scorex.core.consensus.History.ProgressInfo

import scala.util.Try

/**
  * Contains all functions required by History to process PoPoWProofs and generate them.
  */
trait PoPowComponent {

  def validate(m: PoPowProof): Try[Unit]

  def process(m: PoPowProof): ProgressInfo[ErgoPersistentModifier]

  def prove(params: PoPowParams): Try[PoPowProof]

}
