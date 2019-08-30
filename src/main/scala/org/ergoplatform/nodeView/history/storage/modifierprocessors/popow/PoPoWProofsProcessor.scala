package org.ergoplatform.nodeView.history.storage.modifierprocessors.popow

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.{HeaderChain, PoPowAlgos, PoPowProof}
import org.ergoplatform.nodeView.history.storage.modifierprocessors.HeadersProcessor
import org.ergoplatform.settings.{ErgoSettings, PoPowSettings}
import scorex.core.consensus.History.ProgressInfo
import scorex.util.ScorexLogging

import scala.util.Try

/**
  * Contains all functions required by History to process PoPoWProofs and generate them.
  */
trait PoPoWProofsProcessor {

  def validate(m: PoPowProof): Try[Unit]

  def process(m: PoPowProof): ProgressInfo[ErgoPersistentModifier]

}
