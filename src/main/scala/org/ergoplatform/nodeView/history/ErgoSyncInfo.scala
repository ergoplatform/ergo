package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.history.header.{Header, HeaderSerializer}
import scorex.core.{ModifierTypeId, NodeViewModifier}
import scorex.core.consensus.SyncInfo
import scorex.core.network.message.SyncInfoMessageSpec
import scorex.core.serialization.ScorexSerializer
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, ScorexLogging, bytesToId, idToBytes}

/**
  * Information on sync status to be sent to peer over the wire
  *
  */
sealed trait ErgoSyncInfo extends SyncInfo {

  val syncData: Either[Seq[ModifierId], Header]

  val version: Byte

  override def startingPoints: Seq[(ModifierTypeId, ModifierId)] = {
    syncData match {
      case Left(v1Ids) => v1Ids.map(b => Header.modifierTypeId -> b)
      case Right(v2header) => Array(Header.modifierTypeId -> v2header.id)
    }
  }

  override type M = ErgoSyncInfo

  override lazy val serializer: ScorexSerializer[ErgoSyncInfo] = ErgoSyncInfoSerializer
}

/**
  * @param lastHeaderIds
  */
case class ErgoSyncInfoV1(lastHeaderIds: Seq[ModifierId]) extends ErgoSyncInfo {
  override val syncData: Either[Seq[ModifierId], Header] = Left(lastHeaderIds)
  override val version = 1
}

sealed trait ErgoSyncInfoV2 extends ErgoSyncInfo {
  override val version = 2

  val mode: Byte
}

case class ErgoSyncInfoV2Header(lastHeader: Header) extends ErgoSyncInfoV2 {

  override val syncData: Either[Seq[ModifierId], Header] = Right(lastHeader)

  val mode = ErgoSyncInfo.v2HeaderMode
}

object ErgoSyncInfo {
  // TODO move to config?
  val MaxBlockIds = 1000

  val v2HeaderMode: Byte = 1
}

object ErgoSyncInfoSerializer extends ScorexSerializer[ErgoSyncInfo] with ScorexLogging {

  override def serialize(obj: ErgoSyncInfo, w: Writer): Unit = {
    obj match {
      case v1: ErgoSyncInfoV1 =>
        w.putUShort(v1.lastHeaderIds.size)
        v1.lastHeaderIds.foreach(id => w.putBytes(idToBytes(id)))
      case v2: ErgoSyncInfoV2Header =>
        w.putUShort(0)
        w.put(v2.mode)
        val headerBytes = v2.bytes
        w.putUShort(headerBytes.length)
        w.putBytes(headerBytes)
      case _ =>
        log.error(s"Wrong SyncInfo version: $obj")
    }
  }

  override def parse(r: Reader): ErgoSyncInfo = {
    val length = r.getUShort()
    if (length == 0 && r.remaining > 1) {
      val mode = r.getByte()
      if (mode == ErgoSyncInfo.v2HeaderMode) {
        val headerBytesCount = r.getUShort()
        val headerBytes = r.getBytes(headerBytesCount)
        val header = HeaderSerializer.parseBytes(headerBytes)
        //todo: check PoW
        ErgoSyncInfoV2Header(header)
      } else {
        throw new Exception(s"Wrong SyncInfo version: $r")
      }
    } else { // parse v1
      require(length <= ErgoSyncInfo.MaxBlockIds + 1, "Too many block ids in sync info")
      val ids = (1 to length).map(_ => bytesToId(r.getBytes(NodeViewModifier.ModifierIdSize)))
      ErgoSyncInfoV1(ids)
    }
  }

}

object ErgoSyncInfoMessageSpec extends SyncInfoMessageSpec[ErgoSyncInfo](ErgoSyncInfoSerializer)
