package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.history.Header
import scorex.core.NodeViewModifier
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.consensus.SyncInfo
import scorex.core.network.message.SyncInfoMessageSpec
import scorex.core.serialization.Serializer

import scala.util.Try


case class ErgoSyncInfo(answer: Boolean,
                        lastHeaderIds: Seq[ModifierId],
                        fullBlockIdOpt: Option[ModifierId]) extends SyncInfo {

  override def startingPoints: Seq[(NodeViewModifier.ModifierTypeId, NodeViewModifier.ModifierId)] = {
    lastHeaderIds.map(b => Header.ModifierTypeId -> b)
  }

  override type M = ErgoSyncInfo

  override lazy val serializer: Serializer[ErgoSyncInfo] = ErgoSyncInfoSerializer
}

object ErgoSyncInfo {
  val MaxBlockIds = 1000
}

object ErgoSyncInfoSerializer extends Serializer[ErgoSyncInfo] {

  override def toBytes(obj: ErgoSyncInfo): Array[Byte] = {
    val flag: Byte = (obj.answer, obj.fullBlockIdOpt.isDefined) match {
      case (false, false) => 0
      case (false, true) => 1
      case (true, false) => 2
      case (true, true) => 3
    }
    (flag +: obj.fullBlockIdOpt.getOrElse(Array())) ++ scorex.core.utils.concatFixLengthBytes(obj.lastHeaderIds)
  }

  override def parseBytes(bytes: Array[Byte]): Try[ErgoSyncInfo] = Try {
    val (answer, fullBlockIsDefined) = bytes.head match {
      case 0 => (false, false)
      case 1 => (false, true)
      case 2 => (true, false)
      case 3 => (true, true)
      case m => throw new Error(s"Incorrect flag $m")
    }
    val fullBlockIdOpt = if (fullBlockIsDefined) Some(bytes.slice(1, 33)) else None
    val startPosition = if (fullBlockIsDefined) 33 else 1
    val ids = bytes.slice(startPosition, bytes.length).grouped(NodeViewModifier.ModifierIdSize).toSeq
    ErgoSyncInfo(answer, ids, fullBlockIdOpt)
  }

}

object ErgoSyncInfoMessageSpec extends SyncInfoMessageSpec[ErgoSyncInfo](ErgoSyncInfoSerializer.parseBytes)