package org.ergoplatform.nodeView.history.storage.modifierprocessors

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.ADProof
import scorex.core.consensus.History.ProgressInfo

import scala.util.Try

trait ADProofsProcessor {

  protected val aDProofsRequired: Boolean

  def process(m: ADProof): ProgressInfo[ErgoPersistentModifier]

  def toDrop(modifier: ADProof): Seq[ByteArrayWrapper]

  def validate(m: ADProof): Try[Unit]
}

