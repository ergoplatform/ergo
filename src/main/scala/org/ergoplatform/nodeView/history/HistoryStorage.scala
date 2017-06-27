package org.ergoplatform.nodeView.history

import com.google.common.primitives.Ints
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.ergoplatform.modifiers.block.{ErgoBlock, ErgoBlockSerializer}
import scorex.core.NodeViewModifier._
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256

import scala.util.{Failure, Success, Try}

class HistoryStorage[BlockT <: ErgoBlock](storage: LSMStore, genesisId: ModifierId) extends ScorexLogging {


  private val bestBlockIdKey = ByteArrayWrapper(Array.fill(storage.keySize)(-1: Byte))

  private def blockHeightKey(blockId: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(Blake2b256("height".getBytes ++ blockId))

  def insert(b: BlockT, isBest: Boolean): Unit = {
    val bHeight = if (b.isGenesis) 1 else heightOf(b.parentId).get + 1
    val blockH: (ByteArrayWrapper, ByteArrayWrapper) = (blockHeightKey(b.id), ByteArrayWrapper(Ints.toByteArray(bHeight)))
    val bestBlockSeq: Seq[(ByteArrayWrapper, ByteArrayWrapper)] = if (isBest) {
      Seq(bestBlockIdKey -> ByteArrayWrapper(b.id))
    } else {
      Seq()
    }

    storage.update(
      ByteArrayWrapper(b.id),
      Seq(),
      Seq(blockH) ++ bestBlockSeq ++ Seq(ByteArrayWrapper(b.id) -> ByteArrayWrapper(ErgoBlockSerializer.toBytes(b))))
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

  def contains(id: ModifierId): Boolean = modifierById(id).isDefined

  def modifierById(id: ModifierId): Option[BlockT] = storage.get(ByteArrayWrapper(id)).flatMap { bBytes =>
    ErgoBlockSerializer.parseBytes(bBytes.data) match {
      case Success(b) =>
        //TODO asInstanceOf
        Try(b.asInstanceOf[BlockT]).toOption
      case Failure(e) =>
        log.warn("Failed to parse block from db", e)
        None
    }
  }

  def bestBlockId: Array[Byte] = storage.get(bestBlockIdKey).map(_.data).getOrElse(genesisId)

  def bestBlock: BlockT = {
    modifierById(bestBlockId).get
  }

  def heightOf(blockId: ModifierId): Option[Int] = storage.get(blockHeightKey(blockId))
    .map(b => Ints.fromByteArray(b.data))

}
