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
    val flag: Byte = obj.answer match {
      case false => 0
      case true => 1
    }
    flag +: scorex.core.utils.concatFixLengthBytes(obj.lastHeaderIds)
  }

  override def parseBytes(bytes: Array[Byte]): Try[ErgoSyncInfo] = Try {
    val answer = bytes.head match {
      case 0 => false
      case 1 => true
      case m => throw new Error(s"Incorrect flag $m")
    }

    val ids = ModifierId @@ bytes.slice(1, bytes.length).grouped(NodeViewModifier.ModifierIdSize).toSeq

    ErgoSyncInfo(answer, ids)
  }

}

object ErgoSyncInfoMessageSpec extends SyncInfoMessageSpec[ErgoSyncInfo](ErgoSyncInfoSerializer.parseBytes)