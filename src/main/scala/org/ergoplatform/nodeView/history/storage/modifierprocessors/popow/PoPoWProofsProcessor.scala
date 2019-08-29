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
trait PoPoWProofsProcessor { self: HeadersProcessor with ScorexLogging =>

  protected val settings: ErgoSettings

  protected lazy val algos: PoPowAlgos = new PoPowAlgos(settings.nodeSettings.poPowSettings)

  def poPowSettings: PoPowSettings = settings.nodeSettings.poPowSettings

  def lastHeaders(count: Int, offset: Int = 0): HeaderChain

  def validate(m: PoPowProof): Try[Unit]

  def process(m: PoPowProof): ProgressInfo[ErgoPersistentModifier]

  def generateProof: PoPowProof = algos.prove(lastHeaders(headersHeight).headers, 0) // todo: maxLevel

}
