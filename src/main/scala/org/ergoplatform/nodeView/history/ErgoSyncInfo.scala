package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.block.ErgoHeader
import scorex.core.NodeViewModifier
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.consensus.SyncInfo
import scorex.core.network.message.SyncInfoMessageSpec
import scorex.core.serialization.Serializer

import scala.util.Try


case class ErgoSyncInfo(answer: Boolean, lastBlockIds: Seq[ModifierId]) extends SyncInfo {

  override def startingPoints: Seq[(NodeViewModifier.ModifierTypeId, NodeViewModifier.ModifierId)] = {
    lastBlockIds.map(b => ErgoHeader.ModifierTypeId -> b)
  }

  override type M = ErgoSyncInfo

  override def serializer: Serializer[ErgoSyncInfo] = ErgoSyncInfoSerializer
}

object ErgoSyncInfo {
  val MaxBlockIds = 1000
}

object ErgoSyncInfoSerializer extends Serializer[ErgoSyncInfo] {

  override def toBytes(obj: ErgoSyncInfo): Array[Byte] =
    (if (obj.answer) 1.toByte else 0.toByte) +: scorex.core.utils.concatFixLengthBytes(obj.lastBlockIds)

  override def parseBytes(bytes: Array[Byte]): Try[ErgoSyncInfo] = Try {
    val answer = if (bytes.head == 1.toByte) true else false
    val ids = bytes.slice(1, bytes.length).grouped(NodeViewModifier.ModifierIdSize).toSeq
    ErgoSyncInfo(answer, ids)
  }
}

object ErgoSyncInfoMessageSpec extends SyncInfoMessageSpec[ErgoSyncInfo](ErgoSyncInfoSerializer.parseBytes)