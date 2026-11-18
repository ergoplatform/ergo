package org.ergoplatform.nodeView.history

import org.ergoplatform.consensus.SyncInfo
import org.ergoplatform.modifiers.ErgoNodeViewModifier
import org.ergoplatform.modifiers.history.header.{Header, HeaderSerializer}
import org.ergoplatform.network.message.SyncInfoMessageSpec
import org.ergoplatform.nodeView.history.ErgoHistoryUtils.Height
import org.ergoplatform.serialization.ErgoSerializer
import scorex.util.Extensions._
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, ScorexLogging, bytesToId, idToBytes}

/**
  * Information on sync status to be sent to peer over the wire. It should provide an answer to the question how
  * other peer's chain is developed in comparison with best local one.
  *
  */
sealed trait ErgoSyncInfo extends SyncInfo {
  /**
   * Whether sync info message corresponds to non-empty blockchain
   */
  val nonEmpty: Boolean

  override type M = ErgoSyncInfo

  override lazy val serializer: ErgoSerializer[ErgoSyncInfo] = ErgoSyncInfoSerializer
}

/**
  * Initial sync info clients used before 4.0.16 release. Contains just last headers ids.
  * @param lastHeaderIds - last header ids known to a peer
  */
case class ErgoSyncInfoV1(lastHeaderIds: Seq[ModifierId]) extends ErgoSyncInfo {
  override val nonEmpty: Boolean = lastHeaderIds.nonEmpty
}

object ErgoSyncInfoV1 {
  val MaxBlockIds: Int = ErgoSyncInfo.MaxBlockIds
}

trait HeadersBasedSyncInfo extends ErgoSyncInfo {
  val lastHeaders: Seq[Header]

  /**
    * Height of a chain reported by a peer (so most recent header it shows)
    */
  val height: Option[Height] = lastHeaders.headOption.map(_.height)

  override val nonEmpty: Boolean = lastHeaders.nonEmpty
}

/**
  * @param lastHeaders - some recent headers (including last one) known to a peer
  */
case class ErgoSyncInfoV2(lastHeaders: Seq[Header]) extends ErgoSyncInfo with HeadersBasedSyncInfo

/**
  * Sync info message that additionally reports height ranges of headers and full-blocks stored by the peer
  *
  * @param lastHeaders - some recent headers (including last one) known to a peer
  * @param headersRanges - inclusive height ranges of headers stored by the peer
  * @param fullBlocksRanges - inclusive height ranges of full blocks stored by the peer
  */
case class ErgoSyncInfoV3(lastHeaders: Seq[Header],
                          headersRanges: Seq[(Height, Height)],
                          fullBlocksRanges: Seq[(Height, Height)]) extends ErgoSyncInfo with HeadersBasedSyncInfo

object ErgoSyncInfoV3 {
  /**
    * How many different headers ranges a peer can report
    */
  val maxHeadersRanges = 8

  /**
    * How many different full-block ranges a peer can report
    */
  val maxFullBlockRanges = 8
}

object ErgoSyncInfo {
  val MaxBlockIds = 1000
}

object ErgoSyncInfoSerializer extends ErgoSerializer[ErgoSyncInfo] with ScorexLogging {

  val v2HeaderMode: Byte = -1 // used to mark sync v2 messages

  val v3HeaderMode: Byte = -2 // used to mark sync v3 messages

  val MaxHeadersAllowed = 50 // in sync v2/v3 message, no more than 50 headers allowed

  val MaxHeaderSize = 1000 // currently header is about 200+ bytes, but new fields can be added via a SF,
                           // anyway we set hard max header size limit

  override def serialize(obj: ErgoSyncInfo, w: Writer): Unit = {
    def writeLastHeaders(w: Writer, lastHeaders: Seq[Header]): Unit = {
      w.putUByte(lastHeaders.length) // number of headers peer is announcing
      lastHeaders.foreach { h =>
        val headerBytes = h.bytes
        w.putUShort(headerBytes.length)
        w.putBytes(headerBytes)
      }
    }

    obj match {
      case v1: ErgoSyncInfoV1 =>
        // in sync message we just write number of last header ids and then ids themselves
        w.putUShort(v1.lastHeaderIds.size)
        v1.lastHeaderIds.foreach(id => w.putBytes(idToBytes(id)))
      case v2: ErgoSyncInfoV2 =>
        w.putUShort(0) // to stop sync v1 parser
        w.put(v2HeaderMode) // signal that v2 message started
        writeLastHeaders(w, v2.lastHeaders)
      case v3: ErgoSyncInfoV3 =>
        w.putUShort(0) // to stop sync v1 parser
        w.put(v3HeaderMode) // signal that v3 message started
        writeLastHeaders(w, v3.lastHeaders)
        // write headers available
        val headerRangesCount = v3.headersRanges.length
        require(headerRangesCount <= ErgoSyncInfoV3.maxHeadersRanges)
        w.putUByte(headerRangesCount)
        v3.headersRanges.foreach { case (start, end) =>
          w.putUInt(start.toLong)
          w.putUInt(end.toLong)
        }
        // write full-blocks available
        val fullblocksRangesCount = v3.fullBlocksRanges.length
        require(fullblocksRangesCount <= ErgoSyncInfoV3.maxFullBlockRanges)
        w.putUByte(fullblocksRangesCount)
        v3.fullBlocksRanges.foreach { case (start, end) =>
          w.putUInt(start.toLong)
          w.putUInt(end.toLong)
        }
      case _ =>
        log.error(s"Wrong SyncInfo version: $obj")
    }
  }

  override def parse(r: Reader): ErgoSyncInfo = {
    def readLastHeaders(r: Reader): Seq[Header] = {
      val headersCount = r.getUByte()

      require(headersCount <= MaxHeadersAllowed) // check to avoid spam

      val headers = (1 to headersCount).map { _ =>
        val headerBytesCount = r.getUShort()
        require(headerBytesCount < MaxHeaderSize) // check to avoid spam
        val headerBytes = r.getBytes(headerBytesCount)
        HeaderSerializer.parseBytes(headerBytes)
      }
      headers
    }

    val length = r.getUShort()
    if (length == 0 && r.remaining > 1) {
      val mode = r.getByte()
      if (mode == v2HeaderMode) {
        // parse v2 sync message
        val lastHeaders = readLastHeaders(r)
        ErgoSyncInfoV2(lastHeaders)
      } else if (mode == v3HeaderMode) {
        // parse v3 sync message
        val lastHeaders = readLastHeaders(r)
        val headerRangesCount = r.getUByte().toByteExact
        require(headerRangesCount <= ErgoSyncInfoV3.maxHeadersRanges)
        val headersRanges = (1 to headerRangesCount.toInt).map { _ =>
          val start = r.getUInt().toIntExact
          val end = r.getUInt().toIntExact
          start -> end
        }
        val fullBlocksRangesCount = r.getUByte().toByteExact
        require(fullBlocksRangesCount <= ErgoSyncInfoV3.maxFullBlockRanges)
        val fullBlocksRanges = (1 to fullBlocksRangesCount.toInt).map { _ =>
          val start = r.getUInt().toIntExact
          val end = r.getUInt().toIntExact
          start -> end
        }
        ErgoSyncInfoV3(lastHeaders, headersRanges, fullBlocksRanges)
      } else {
        throw new Exception(s"Wrong SyncInfo version: $r")
      }
    } else { // parse v1 sync message
      require(length <= ErgoSyncInfo.MaxBlockIds + 1, "Too many block ids in sync info")
      val ids = (1 to length).map(_ => bytesToId(r.getBytes(ErgoNodeViewModifier.ModifierIdSize)))
      ErgoSyncInfoV1(ids)
    }
  }

}

object ErgoSyncInfoMessageSpec extends SyncInfoMessageSpec[ErgoSyncInfo](ErgoSyncInfoSerializer)
