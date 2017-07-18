package org.ergoplatform.nodeView.history.storage.modifierprocessors

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.ADProofs

import scala.util.Try

trait ADProofsProcessor {

  def toInsert(m: ADProofs, env: ModifierProcessorEnvironment): Seq[(ByteArrayWrapper, ByteArrayWrapper)]

  def toDrop(modifier: ADProofs): Seq[ByteArrayWrapper]

  def validate(m: ADProofs): Try[Unit]
}

