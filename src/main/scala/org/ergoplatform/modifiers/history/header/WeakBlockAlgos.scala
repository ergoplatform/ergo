package org.ergoplatform.modifiers.history.header

import org.ergoplatform.modifiers.history.header.WeakBlockAlgos.SubBlockInfo
import org.ergoplatform.nodeView.history.ErgoHistory.Difficulty
import scorex.core.{NodeViewModifier, bytesToId, idToBytes}
import scorex.core.network.message.Message.MessageCode
import scorex.core.network.message.MessageSpecV1
import scorex.core.serialization.ErgoSerializer
import scorex.util.serialization.{Reader, Writer}
import scorex.util.Extensions._
import scorex.util.ModifierId

import scala.collection.mutable

/**
  * Implementation steps:
  * * implement basic weak block algorithms (isweak etc)
  * * implement weak block network message
  * * implement weak block info support in sync tracker
  * * implement downloading weak blocks chain
  * * implement avoiding downloading full-blocks
  * * weak blocks support in /mining API
  * * weak confirmations API
  */
object WeakBlockAlgos {

  val weaksPerBlock = 128 // weak blocks per block

  val weakTransactionIdLength = 6

  def isWeak(header: Header, requiredDifficulty: Difficulty): Boolean = {
    val diff = requiredDifficulty / weaksPerBlock
    header.requiredDifficulty >= diff
  }



  // messages:
  //
  // weak block signal:
  // version
  // weak block (~200 bytes) - contains a link to parent block
  // previous weak block id
  // transactions since last weak blocks (8 byte ids?)

  // todo: move `txsSinceLastWeak` to a dedicated message
  case class SubBlockInfo(version: Byte, subBlock: Header, prevWeakBlockId: Array[Byte])

  object SubBlockInfo {

    val initialMessageVersion = 1

    /**
      * Serializer which can convert self to bytes
      */
    def serializer: ErgoSerializer[SubBlockInfo] = new ErgoSerializer[SubBlockInfo] {
      override def serialize(sbi: SubBlockInfo, w: Writer): Unit = {
        w.put(sbi.version)
        HeaderSerializer.serialize(sbi.subBlock, w)
        w.putBytes(sbi.prevWeakBlockId)
      }

      override def parse(r: Reader): SubBlockInfo = {
        val version = r.getByte()
        if (version == initialMessageVersion) {
          val weakBlock = HeaderSerializer.parse(r)
          val prevWeakBlockId = r.getBytes(32)
          new SubBlockInfo(version, weakBlock, prevWeakBlockId)
        } else {
          throw new Exception("Unsupported weakblock message version")
        }
      }
    }
  }

  /**
    * Message that is informing about weak block produced.
    * Contains header and link to previous weak block ().
    */
  object SubBlockMessageSpec extends MessageSpecV1[SubBlockInfo] {

    val MaxMessageSize = 10000

    override val messageCode: MessageCode = 90: Byte
    override val messageName: String = "WeakBlock"

    override def serialize(data: SubBlockInfo, w: Writer): Unit = {
      SubBlockInfo.serializer.serialize(data, w)
    }

    override def parse(r: Reader): SubBlockInfo = {
      SubBlockInfo.serializer.parse(r)
    }
  }

  /**
    * On receiving weak block or block, the node is sending last weak block or block id it has to get short transaction
    * ids since then
    */
  object GetDataSpec extends MessageSpecV1[ModifierId] {

    import scorex.util.{idToBytes, bytesToId}

    override val messageCode: MessageCode = 91: Byte
    override val messageName: String = "GetData"

    override def serialize(data: ModifierId, w: Writer): Unit = {
      w.putBytes(idToBytes(data))
    }

    override def parse(r: Reader): ModifierId = {
      bytesToId(r.getBytes(NodeViewModifier.ModifierIdSize))
    }
  }

  case class TransactionsSince(transactionsWithBlockIds: Array[(ModifierId, Array[Array[Byte]])])

  class DataSpec extends MessageSpecV1[TransactionsSince] {

    override val messageCode: MessageCode = 92: Byte
    override val messageName: String = "GetData"

    override def serialize(data: TransactionsSince, w: Writer): Unit = {
      w.putUInt(data.transactionsWithBlockIds.length)
      data.transactionsWithBlockIds.foreach { case (id, txIds) =>
        w.putBytes(idToBytes(id))
        w.putUInt(txIds.length)
        txIds.foreach { txId =>
          w.putBytes(txId)
        }
      }
    }

    override def parse(r: Reader): TransactionsSince = {
      val blocksCount = r.getUInt().toIntExact
      val records = (1 to blocksCount).map { _ =>
        val blockId = r.getBytes(32)
        val txsCount = r.getUInt().toIntExact
        val txIds = (1 to txsCount).map { _ =>
          r.getBytes(6)
        }.toArray
        bytesToId(blockId) -> txIds
      }.toArray
      TransactionsSince(records)
    }
  }

}

object structures {
  var lastBlock: Header = null

  val subBlocks: mutable.Set[ModifierId] = mutable.Set.empty

  val subBlockLinks: mutable.Map[ModifierId, ModifierId] = mutable.Map.empty

  var subBlockTxs: Map[ModifierId, Array[Array[Byte]]] = Map.empty

  // A primer algo on processing
  def processWeakBlock(sbi: SubBlockInfo) = {
    val sbHeader = sbi.subBlock
    val prevWbId = bytesToId(sbi.prevWeakBlockId)
    val sbHeight = sbHeader.height

    if(sbHeader.id == prevWbId){
      ??? // todo: throw error
    }

    if (sbHeight < lastBlock.height + 1) {
      // just ignore as we have better block already
    } else if (sbHeight == lastBlock.height + 1) {
      if (sbHeader.parentId == lastBlock.id) {
        val weakBlockId = sbHeader.id
        if(subBlocks.contains(weakBlockId)){
          // todo: what to do?
        } else {
          subBlocks += weakBlockId
          if (subBlocks.contains(prevWbId)){
            subBlockLinks.put(weakBlockId, prevWbId)
          } else {
            //todo: download prev weak block id
          }
          // todo: download weak block related txs
        }
      } else {
        // todo: we got orphaned block's weak block, process this
      }
    } else {
      // just ignoring weak block coming from future for now
    }
  }

  def processBlock(header: Header) = {
    if (header.height > lastBlock.height) {
      lastBlock = header
      subBlocks.clear()
      subBlockLinks.clear()
      subBlockTxs = Map.empty
    } else {
      ??? // todo: process
    }
  }

}

