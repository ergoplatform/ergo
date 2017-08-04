package org.ergoplatform.nodeView.history.storage.modifierprocessors.popow

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.PoPoWProof
import scorex.core.consensus.History.ProgressInfo

import scala.util.{Failure, Try}

trait EmptyPoPoWProofsProcessor extends PoPoWProofsProcessor {

  def toDrop(modifier: PoPoWProof): Seq[ByteArrayWrapper] = ???

  def validate(m: PoPoWProof): Try[Unit] = Failure(new Error("Regime that do not process PoPoWProof"))

  def process(m: PoPoWProof): ProgressInfo[ErgoPersistentModifier] = ???
}

