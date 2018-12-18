package org.ergoplatform.nodeView.history.storage.modifierprocessors.popow

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.{HeaderChain, NiPoPowAlgos, NiPoPowProof}
import org.ergoplatform.nodeView.history.storage.modifierprocessors.HeadersProcessor
import org.ergoplatform.settings.NodeConfigurationSettings
import scorex.core.consensus.History.ProgressInfo

import scala.util.Try

trait NiPoPowProofsProcessor extends HeadersProcessor {

  protected val config: NodeConfigurationSettings

  protected val algos: NiPoPowAlgos = NiPoPowAlgos(config.poPowSettings)

  def lastHeaders(count: Int, offset: Int = 0): HeaderChain

  def validate(m: NiPoPowProof): Try[Unit]

  def process(m: NiPoPowProof): ProgressInfo[ErgoPersistentModifier]

  def generateProof: NiPoPowProof = algos.prove(lastHeaders(headersHeight).headers)

}
