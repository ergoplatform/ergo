package org.ergoplatform.modifiers.block

import com.google.common.primitives.Bytes
import scorex.core.serialization.Serializer
import scorex.core.utils.ScorexLogging

import scala.util.Try

object ErgoBlockSerializer extends Serializer[ErgoBlock] with ScorexLogging {

  override def toBytes(block: ErgoBlock): Array[Byte] = {
    Bytes.concat(Array(block.modifierTypeId), block.bytes)
  }

  override def parseBytes(bytesAll: Array[Byte]): Try[ErgoBlock] = Try {
    lazy val tailBytes = bytesAll.tail
    bytesAll.head match {
      case ErgoHeader.ModifierTypeId =>
        ErgoHeaderSerializer.parseBytes(tailBytes).get
      case ErgoFullBlock.ModifierTypeId =>
        ErgoFullBlockSerializer.parseBytes(tailBytes).get
      case m =>
        throw new Error(s"Unknown block type $m")
    }
  }

}
