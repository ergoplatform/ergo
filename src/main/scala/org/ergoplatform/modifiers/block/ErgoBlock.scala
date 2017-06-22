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
  with Block[AnyoneCanSpendProposition, AnyoneCanSpendTransaction]

object ErgoBlockSerializer extends Serializer[ErgoBlock] with ScorexLogging {

  override def toBytes(block: ErgoBlock): Array[Byte] = {
    Bytes.concat(Array(block.modifierTypeId), block.bytes)
  }
  
  override def parseBytes(bytesAll: Array[Byte]): Try[ErgoBlock] = Try {
    lazy val blockBytes = bytesAll.tail
    bytesAll.head match {
      case ErgoHeader.ModifierTypeId =>
        ErgoHeaderSerializer.parseBytes(blockBytes).get

      case m =>
        throw new Error(s"Unknown block type ${m}")
    }
  }

}