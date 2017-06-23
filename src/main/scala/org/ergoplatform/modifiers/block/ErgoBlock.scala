package org.ergoplatform.modifiers.block

import com.google.common.primitives.Bytes
import org.ergoplatform.modifiers.transaction.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.transaction.proposition.AnyoneCanSpendProposition
import scorex.core.PersistentNodeViewModifier
import scorex.core.block.Block
import scorex.core.serialization.Serializer
import scorex.core.utils.ScorexLogging

import scala.util.Try

trait ErgoBlock extends PersistentNodeViewModifier[AnyoneCanSpendProposition, AnyoneCanSpendTransaction]
  with Block[AnyoneCanSpendProposition, AnyoneCanSpendTransaction] {
  def stateRoot: Array[Byte]
  val nonce: Int

}

