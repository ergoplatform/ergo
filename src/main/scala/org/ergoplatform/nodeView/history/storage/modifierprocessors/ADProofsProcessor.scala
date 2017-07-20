package org.ergoplatform.nodeView.history.storage.modifierprocessors

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.ADProofs
import scorex.core.consensus.History.ProgressInfo

import scala.util.Try

trait ADProofsProcessor {

  def process(m: ADProofs): ProgressInfo[ErgoPersistentModifier]

  def toDrop(modifier: ADProofs): Seq[ByteArrayWrapper]

  def validate(m: ADProofs): Try[Unit]
}

