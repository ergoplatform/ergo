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

  val syncData: Either[Seq[ModifierId], Seq[Header]]

  val version: Byte

  override def startingPoints: Seq[(ModifierTypeId, ModifierId)] = {
    syncData match {
      case Left(v1Ids) => v1Ids.map(b => Header.modifierTypeId -> b)
      case Right(v2headers) => v2headers.map(h => Header.modifierTypeId -> h.id)
    }
  }

  override type M = ErgoSyncInfo

  override lazy val serializer: ScorexSerializer[ErgoSyncInfo] = ErgoSyncInfoSerializer
}

/**
  * @param lastHeaderIds
  */
case class ErgoSyncInfoV1(lastHeaderIds: Seq[ModifierId]) extends ErgoSyncInfo {
  override val syncData: Either[Seq[ModifierId], Seq[Header]] = Left(lastHeaderIds)
  override val version = 1
}

case class ErgoSyncInfoV2(lastHeaders: Seq[Header]) extends ErgoSyncInfo {

  val height = lastHeaders.headOption.map(_.height).getOrElse(ErgoHistory.EmptyHistoryHeight)

  override val syncData: Either[Seq[ModifierId], Seq[Header]] = Right(lastHeaders)
  override val version = 2
}


object ErgoSyncInfo {
  // TODO move to config?
  val MaxBlockIds = 1000

  val v2HeaderMode: Byte = -1
}

object ErgoSyncInfoSerializer extends ScorexSerializer[ErgoSyncInfo] with ScorexLogging {

  override def serialize(obj: ErgoSyncInfo, w: Writer): Unit = {
    obj match {
      case v1: ErgoSyncInfoV1 =>
        w.putUShort(v1.lastHeaderIds.size)
        v1.lastHeaderIds.foreach(id => w.putBytes(idToBytes(id)))
      case v2: ErgoSyncInfoV2 =>
        w.putUShort(0)
        w.put(ErgoSyncInfo.v2HeaderMode)
        w.put(v2.lastHeaders.length.toByte)
        v2.lastHeaders.foreach { h =>
          val headerBytes = h.bytes
          w.putUShort(headerBytes.length)
          w.putBytes(headerBytes)
        }

      case _ =>
        log.error(s"Wrong SyncInfo version: $obj")
    }
  }

  override def parse(r: Reader): ErgoSyncInfo = {
    val length = r.getUShort()
    if (length == 0 && r.remaining > 1) {
      val mode = r.getByte()
      if (mode == ErgoSyncInfo.v2HeaderMode) {

        val headersCount = r.getUByte()

        require(headersCount < 20)

        val headers = (1 to headersCount).map { _ =>
          val headerBytesCount = r.getUShort()
          require(headersCount < 1000)
          val headerBytes = r.getBytes(headerBytesCount)
          HeaderSerializer.parseBytes(headerBytes)
        }
        ErgoSyncInfoV2(headers)
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
