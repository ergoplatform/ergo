package org.ergoplatform.nodeView.history.storage.modifierprocessors.popow

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.PoPoWProof
import scorex.core.consensus.History.ProgressInfo
import scorex.core.utils.ScorexLogging

import scala.util.Try

trait FullPoPoWProofsProcessor extends PoPoWProofsProcessor {

  def toDrop(modifier: PoPoWProof): Seq[ByteArrayWrapper] = ???

  def validate(m: PoPoWProof): Try[Unit] = m.validate

  def process(m: PoPoWProof): ProgressInfo[ErgoPersistentModifier] = ???
}

