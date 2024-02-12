package org.ergoplatform

import org.ergoplatform.SubBlockAlgos.SubBlockInfo
import org.ergoplatform.modifiers.history.header.{Header, HeaderSerializer}
import org.ergoplatform.network.message.MessageConstants.MessageCode
import org.ergoplatform.network.message.MessageSpecV1
import org.ergoplatform.nodeView.history.ErgoHistoryUtils.Difficulty
import org.ergoplatform.serialization.ErgoSerializer
import org.ergoplatform.settings.Constants
import scorex.util.Extensions._
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, bytesToId, idToBytes}

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

  /**
    * Sub-block message, sent by the node to peers when a sub-block is generated
    * @param version - message version (to allow injecting new fields)
    * @param subBlock - subblock
    * @param prevSubBlockId - previous sub block id `subBlock` is following, if missed, sub-block is linked
    *                         to a previous block
    */
  case class SubBlockInfo(version: Byte, subBlock: Header, prevSubBlockId: Option[Array[Byte]])

  object SubBlockInfo {

    val initialMessageVersion = 1

    /**
      * Serializer which can convert self to bytes
      */
    def serializer: ErgoSerializer[SubBlockInfo] = new ErgoSerializer[SubBlockInfo] {
      override def serialize(sbi: SubBlockInfo, w: Writer): Unit = {
        w.put(sbi.version)
        HeaderSerializer.serialize(sbi.subBlock, w)
        w.putOption(sbi.prevSubBlockId){case (w, id) => w.putBytes(id)}
      }

      override def parse(r: Reader): SubBlockInfo = {
        val version = r.getByte()
        if (version == initialMessageVersion) {
          val subBlock = HeaderSerializer.parse(r)
          val prevSubBlockId = r.getOption(r.getBytes(Constants.ModifierIdSize))
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

    import scorex.util.{bytesToId, idToBytes}

    override val messageCode: MessageCode = 91: Byte
    override val messageName: String = "GetData"

    override def serialize(data: ModifierId, w: Writer): Unit = {
      w.putBytes(idToBytes(data))
    }

    override def parse(r: Reader): ModifierId = {
      bytesToId(r.getBytes(Constants.ModifierIdSize))
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
  var lastBlock: Header = null // we ignore forks for now

  // all the sub-blocks known since the last block
  val subBlocks: mutable.Map[ModifierId, Header] = mutable.Map.empty

  // links from sub-blocks to their parent sub-blocks
  val subBlockLinks: mutable.Map[ModifierId, ModifierId] = mutable.Map.empty

  // only new transactions appeared in a sub-block
  var subBlockTxs: Map[ModifierId, Array[Array[Byte]]] = Map.empty


  /**
    * A primer algo on processing sub-blocks in p2p layer. It is updating internal sub-block related
    * caches and decides what to download next
    *
    * @param sbi
    * @return - sub-block ids to download, sub-block transactions to download
    */
  def processSubBlock(sbi: SubBlockInfo): (Seq[ModifierId], Seq[ModifierId]) = {
    val sbHeader = sbi.subBlock
    val prevSbIdOpt = sbi.prevSubBlockId.map(bytesToId)
    val sbHeight = sbHeader.height

    def emptyResult: (Seq[ModifierId], Seq[ModifierId]) = Seq.empty -> Seq.empty

    prevSbIdOpt match {
      case None => ??? // todo: link to prev block

      case Some(prevSbId) =>
        if (sbHeader.id == prevSbId) {
          ??? // todo: malicious prev throw error
        }

        if (sbHeight < lastBlock.height + 1) {
          // just ignore as we have better block already
          emptyResult
        } else if (sbHeight == lastBlock.height + 1) {
          if (sbHeader.parentId == lastBlock.id) {
            val subBlockId = sbHeader.id
            if (subBlocks.contains(subBlockId)) {
              // we got sub-block we already have
              // todo: check if previous sub-block and transactions are downloaded
              emptyResult
            } else {
              subBlocks += subBlockId -> sbHeader
              if (subBlocks.contains(prevSbId)) {
                val prevSb = subBlocks(prevSbId)
                subBlockLinks.put(subBlockId, prevSbId)
                if (prevSb.transactionsRoot != sbHeader.transactionsRoot) {
                  (Seq.empty, Seq(sbHeader.id))
                } else {
                  emptyResult // no new transactions
                }
              } else {
                //todo: download prev sub block id
                (Seq(prevSbId), Seq(sbHeader.id))
              }
              // todo: download sub block related txs
            }
          } else {
            // todo: we got orphaned block's sub block, do nothing for now, but we need to check the block is downloaded
            emptyResult
          }
        } else {
          // just ignoring sub block coming from future for now
          emptyResult
        }
    }
  }

  def processBlock(header: Header): Unit = {
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

