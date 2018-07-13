package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.history.Header
import scorex.core.consensus.SyncInfo
import scorex.core.network.message.SyncInfoMessageSpec
import scorex.core.serialization.Serializer
import scorex.core._

import scala.util.Try


case class ErgoSyncInfo(lastHeaderIds: Seq[ModifierId]) extends SyncInfo {

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
    scorex.core.utils.concatFixLengthBytes(obj.lastHeaderIds.map(idToBytes))
  }

  override def parseBytes(bytes: Array[Byte]): Try[ErgoSyncInfo] = Try {
    require(bytes.length <= ErgoSyncInfo.MaxBlockIds * NodeViewModifier.ModifierIdSize + 1)

    val ids = bytes.grouped(NodeViewModifier.ModifierIdSize).toSeq.map(bytesToId)

    ErgoSyncInfo(ids)
  }
}

object ErgoSyncInfoMessageSpec extends SyncInfoMessageSpec[ErgoSyncInfo](ErgoSyncInfoSerializer.parseBytes)
