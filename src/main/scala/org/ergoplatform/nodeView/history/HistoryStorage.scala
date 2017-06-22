package org.ergoplatform.nodeView.history

import com.google.common.primitives.{Ints, Longs}
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.ergoplatform.modifiers.block.{ErgoBlock, ErgoBlockSerializer}
import org.ergoplatform.settings.ErgoSettings
import scorex.core.NodeViewModifier._
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256

import scala.util.{Failure, Success}

class HistoryStorage(storage: LSMStore, settings: ErgoSettings) extends ScorexLogging {

  private val bestBlockIdKey = ByteArrayWrapper(Array.fill(storage.keySize)(-1: Byte))

  private def blockHeightKey(blockId: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(Blake2b256("height".getBytes ++ blockId))

  def insert(b: ErgoBlock, isBest: Boolean): Unit = {
    val bHeight = heightOf(b.parentId).get + 1
    val blockH: (ByteArrayWrapper, ByteArrayWrapper) = (blockHeightKey(b.id), ByteArrayWrapper(Longs.toByteArray(bHeight)))
    val bestBlockSeq: Seq[(ByteArrayWrapper, ByteArrayWrapper)] = if (isBest) {
      Seq(bestBlockIdKey -> ByteArrayWrapper(b.id))
    } else {
      Seq()
    }

    storage.update(
      ByteArrayWrapper(b.id),
      Seq(),
      Seq(blockH) ++ bestBlockSeq ++ Seq(ByteArrayWrapper(b.id) -> ByteArrayWrapper(b.bytes)))
  }

  def drop(id: ModifierId): Unit = {
    if (id sameElements bestBlockId) {
      modifierById(id) match {
        case Some(b) =>
          storage.rollback(ByteArrayWrapper(b.parentId))
        case None =>
          log.warn(s"Trying to drop non-existing block ${Base58.encode(id)}")
      }
    } else {
      //TODO
    }
  }

  def height: Int = heightOf(bestBlockId).get

  def modifierById(id: ModifierId): Option[ErgoBlock] = storage.get(ByteArrayWrapper(id)).flatMap { bBytes =>
    ErgoBlockSerializer.parseBytes(bBytes.data) match {
      case Success(b) =>
        Some(b)
      case Failure(e) =>
        log.warn("Failed to parse block from db", e)
        None
    }
  }

  def bestBlockId: Array[Byte] = storage.get(bestBlockIdKey).map(_.data).getOrElse(settings.genesisId)

  def bestBlock: ErgoBlock = modifierById(bestBlockId).get

  def heightOf(blockId: ModifierId): Option[Int] = storage.get(blockHeightKey(blockId))
    .map(b => Ints.fromByteArray(b.data))
}
