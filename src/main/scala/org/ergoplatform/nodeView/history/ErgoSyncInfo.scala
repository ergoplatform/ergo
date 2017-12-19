package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.history.Header
import scorex.core.consensus.SyncInfo
import scorex.core.network.message.SyncInfoMessageSpec
import scorex.core.serialization.Serializer
import scorex.core.{ModifierId, ModifierTypeId, NodeViewModifier}

import scala.util.Try


case class ErgoSyncInfo(answer: Boolean,
                        lastHeaderIds: Seq[ModifierId]) extends SyncInfo {

  override def startingPoints: Seq[(ModifierTypeId, ModifierId)] = {
    lastHeaderIds.map(b => Header.modifierTypeId -> b)
  }

  override type M = ErgoSyncInfo

  override lazy val serializer: Serializer[ErgoSyncInfo] = ErgoSyncInfoSerializer
}

object ErgoSyncInfo {
  val MaxBlockIds = 1000
}

object ErgoSyncInfoSerializer extends Serializer[ErgoSyncInfo] {

  override def toBytes(obj: ErgoSyncInfo): Array[Byte] = {
    val answer: Byte = if (obj.answer) 1 else 0
    answer +: scorex.core.utils.concatFixLengthBytes(obj.lastHeaderIds)
  }

  override def parseBytes(bytes: Array[Byte]): Try[ErgoSyncInfo] = Try {
    require(bytes.length <= ErgoSyncInfo.MaxBlockIds * NodeViewModifier.ModifierIdSize + 1)

    val answer = bytes.head match {
      case 0 => false
      case 1 => true
      case m => throw new Error(s"Incorrect answer flag value: $m")
    }

    val ids = ModifierId @@ bytes.slice(1, bytes.length).grouped(NodeViewModifier.ModifierIdSize).toSeq

    ErgoSyncInfo(answer, ids)
  }
}

object ErgoSyncInfoMessageSpec extends SyncInfoMessageSpec[ErgoSyncInfo](ErgoSyncInfoSerializer.parseBytes)