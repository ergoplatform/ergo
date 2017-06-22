package org.ergoplatform.modifiers.block

import org.ergoplatform.modifiers.transaction.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.transaction.proposition.AnyoneCanSpendProposition
import scorex.core.PersistentNodeViewModifier
import scorex.core.block.Block
import scorex.core.serialization.Serializer

import scala.util.Try

trait ErgoBlock extends PersistentNodeViewModifier[AnyoneCanSpendProposition, AnyoneCanSpendTransaction]
  with Block[AnyoneCanSpendProposition, AnyoneCanSpendTransaction]

object ErgoBlockSerializer extends Serializer[ErgoBlock] {
  override def toBytes(obj: ErgoBlock): Array[Byte] = ???

  override def parseBytes(bytes: Array[Byte]): Try[ErgoBlock] = ???
}
