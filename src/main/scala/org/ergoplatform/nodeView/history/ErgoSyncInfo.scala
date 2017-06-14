package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.block.ErgoHeader
import scorex.core.NodeViewModifier
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.consensus.SyncInfo
import scorex.core.network.message.SyncInfoMessageSpec
import scorex.core.serialization.Serializer

import scala.util.Try


/**
  * Stores up to 50 last PoW & Pos blocks
  * Thus maximum message size is about 100 * 33 ~= 3.2 KB
  */
case class ErgoSyncInfo(override val answer: Boolean,
                        lastBlockIds: Seq[ModifierId]
                       ) extends SyncInfo {

  override def startingPoints: Seq[(NodeViewModifier.ModifierTypeId, NodeViewModifier.ModifierId)] = {
    lastBlockIds.map(b => ErgoHeader.ModifierTypeId -> b)
  }

  override type M = ErgoSyncInfo

  override def serializer: Serializer[ErgoSyncInfo] = ErgoSyncInfoSerializer
}

object ErgoSyncInfoSerializer extends Serializer[ErgoSyncInfo] {

  override def toBytes(obj: ErgoSyncInfo): Array[Byte] = ???

  override def parseBytes(bytes: Array[Byte]): Try[ErgoSyncInfo] = ???
}

object ErgoSyncInfoMessageSpec extends SyncInfoMessageSpec[ErgoSyncInfo](ErgoSyncInfoSerializer.parseBytes)