package org.ergoplatform.modifiers.history.header

import org.ergoplatform.modifiers.history.header.SubBlockAlgos.SubBlockInfo
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
  * * implement basic sub block algorithms (isSub etc)
  * * implement sub block network message
  * * implement sub block info support in sync tracker
  * * implement downloading sub blocks chain
  * * implement avoiding downloading full-blocks
  * * sub blocks support in /mining API
  * * sub confirmations API
  */
object SubBlockAlgos {

  val subsPerBlock = 128 // sub blocks per block

  val weakTransactionIdLength = 6

  def isSub(header: Header, requiredDifficulty: Difficulty): Boolean = {
    val diff = requiredDifficulty / subsPerBlock
    header.requiredDifficulty >= diff
  }



  // messages:
  //
  // sub block signal:
  // version
  // sub block (~200 bytes) - contains a link to parent block
  // previous sub block id
  // transactions since last sub blocks (8 byte ids?)

  // todo: move `txsSinceLastSub` to a dedicated message
  case class SubBlockInfo(version: Byte, subBlock: Header, prevSubBlockId: Array[Byte])

  object SubBlockInfo {

    val initialMessageVersion = 1

    /**
      * Serializer which can convert self to bytes
      */
    def serializer: ErgoSerializer[SubBlockInfo] = new ErgoSerializer[SubBlockInfo] {
      override def serialize(sbi: SubBlockInfo, w: Writer): Unit = {
        w.put(sbi.version)
        HeaderSerializer.serialize(sbi.subBlock, w)
        w.putBytes(sbi.prevSubBlockId)
      }

      override def parse(r: Reader): SubBlockInfo = {
        val version = r.getByte()
        if (version == initialMessageVersion) {
          val subBlock = HeaderSerializer.parse(r)
          val prevSubBlockId = r.getBytes(32)
          new SubBlockInfo(version, subBlock, prevSubBlockId)
        } else {
          throw new Exception("Unsupported sub-block message version")
        }
      }
    }
  }

  /**
    * Message that is informing about sub block produced.
    * Contains header and link to previous sub block ().
    */
  object SubBlockMessageSpec extends MessageSpecV1[SubBlockInfo] {

    val MaxMessageSize = 10000

    override val messageCode: MessageCode = 90: Byte
    override val messageName: String = "SubBlock"

    override def serialize(data: SubBlockInfo, w: Writer): Unit = {
      SubBlockInfo.serializer.serialize(data, w)
    }

    override def parse(r: Reader): SubBlockInfo = {
      SubBlockInfo.serializer.parse(r)
    }
  }

  /**
    * On receiving sub block or block, the node is sending last sub block or block id it has to get short transaction
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

  case class DownloadPlan()

  // A primer algo on processing sub-blocks

  def processSubBlock(sbi: SubBlockInfo) = {
    val sbHeader = sbi.subBlock
    val prevSbId = bytesToId(sbi.prevSubBlockId)
    val sbHeight = sbHeader.height

    if(sbHeader.id == prevSbId){
      ??? // todo: malicious prev throw error
    }

    if (sbHeight < lastBlock.height + 1) {
      // just ignore as we have better block already
    } else if (sbHeight == lastBlock.height + 1) {
      if (sbHeader.parentId == lastBlock.id) {
        val subBlockId = sbHeader.id
        if(subBlocks.contains(subBlockId)){
          // todo: what to do?
        } else {
          subBlocks += subBlockId
          if (subBlocks.contains(prevSbId)){
            subBlockLinks.put(subBlockId, prevSbId)
          } else {
            //todo: download prev sub block id
          }
          // todo: download sub block related txs
        }
      } else {
        // todo: we got orphaned block's sub block, process this
      }
    } else {
      // just ignoring sub block coming from future for now
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
