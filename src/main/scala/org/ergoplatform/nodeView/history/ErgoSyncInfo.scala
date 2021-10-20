package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.history.header.{Header, HeaderSerializer}
import scorex.core.NodeViewModifier
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
  val nonEmpty: Boolean

  override type M = ErgoSyncInfo

  override lazy val serializer: ScorexSerializer[ErgoSyncInfo] = ErgoSyncInfoSerializer
}

/**
  * @param lastHeaderIds - last header ids known to a peer
  */
case class ErgoSyncInfoV1(lastHeaderIds: Seq[ModifierId]) extends ErgoSyncInfo {
  override val nonEmpty: Boolean = lastHeaderIds.nonEmpty
}

/**
  * @param lastHeaders - some recent headers (inlcuding last one) known to a peer
  */
case class ErgoSyncInfoV2(lastHeaders: Seq[Header]) extends ErgoSyncInfo {
  val height = lastHeaders.headOption.map(_.height).getOrElse(ErgoHistory.EmptyHistoryHeight)

  override val nonEmpty: Boolean = lastHeaders.nonEmpty
}

object ErgoSyncInfo {
  val MaxBlockIds = 1000
}

object ErgoSyncInfoSerializer extends ScorexSerializer[ErgoSyncInfo] with ScorexLogging {

  val v2HeaderMode: Byte = -1 // used to mark sync v2 messages

  val MaxHeadersAllowed = 50 // in sync v2 message, no more than 50 headers allowed

  val MaxHeaderSize = 1000 // currently header is about 200+ bytes, but new fields can be added via a SF,
                           // anyway we set hard max header size limit

  override def serialize(obj: ErgoSyncInfo, w: Writer): Unit = {
    obj match {
      case v1: ErgoSyncInfoV1 =>
        // in sync message we just write number of last header ids and then ids themselves
        w.putUShort(v1.lastHeaderIds.size)
        v1.lastHeaderIds.foreach(id => w.putBytes(idToBytes(id)))
      case v2: ErgoSyncInfoV2 =>
        w.putUShort(0) // to stop sync v1 parser
        w.put(v2HeaderMode) // signal that v2 message started
        w.put(v2.lastHeaders.length.toByte) // number of headers peer is announcing
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
      if (mode == v2HeaderMode) {
        // parse v2 sync message
        val headersCount = r.getUByte()

        require(headersCount <= MaxHeadersAllowed) // check to avoid spam

        val headers = (1 to headersCount).map { _ =>
          val headerBytesCount = r.getUShort()
          require(headerBytesCount < MaxHeaderSize) // check to avoid spam
          val headerBytes = r.getBytes(headerBytesCount)
          HeaderSerializer.parseBytes(headerBytes)
        }
        ErgoSyncInfoV2(headers)
      } else {
        throw new Exception(s"Wrong SyncInfo version: $r")
      }
    } else { // parse v1 sync message
      require(length <= ErgoSyncInfo.MaxBlockIds + 1, "Too many block ids in sync info")
      val ids = (1 to length).map(_ => bytesToId(r.getBytes(NodeViewModifier.ModifierIdSize)))
      ErgoSyncInfoV1(ids)
    }
  }

}

object ErgoSyncInfoMessageSpec extends SyncInfoMessageSpec[ErgoSyncInfo](ErgoSyncInfoSerializer)
