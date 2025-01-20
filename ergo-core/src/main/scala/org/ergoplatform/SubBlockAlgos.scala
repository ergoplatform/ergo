package org.ergoplatform

import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.network.message.MessageConstants.MessageCode
import org.ergoplatform.network.message.MessageSpecInitial
import org.ergoplatform.settings.{Constants, Parameters}
import org.ergoplatform.subblocks.InputBlockInfo
import scorex.util.Extensions._
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, bytesToId, idToBytes}

import scala.collection.mutable

/**
  * Implementation steps:
  * * implement basic input block algorithms (isInput etc)
  * * implement input block network message
  * * implement input block info support in sync tracker
  * * implement downloading input blocks chain
  * * implement avoiding downloading full-blocks
  * * input blocks support in /mining API
  * * sub confirmations API
  */
object SubBlockAlgos {

  // Only sub-blocks may have transactions, full-blocks may only bring block reward transaction ( designated
  // by using emission or re-emission NFTs).
  // As a full-block is also a sub-block, and miner does not know output in advance, the following requirements
  // for the block are introduced. And to be on par with other proposals in consensus performance, we call them
  // input block (sub-block) and ordering block(full-block):
  //  * ordering block's Merkle tree is corresponding to latest input block's Merkle tree , or latest ordering block's
  //    Merkle tree if there are no input blocks after previous ordering block, with only reward transaction added
  //  * every block (input and ordering) also contains digest of new transactions since last input block. For ordering
  //  block, they are ignored.
  //  * script execution context different for input and ordering blocks for the following fields :
  //     * timestamp - next input or ordering block has non-decreasing timestamp to ours
  //     * height - the same for input blocks and next ordering block
  //     * votes - could be different in different (input and ordering) blocks
  //     * minerPk - could be different in different (input and ordering) blocks

  // Another option is to use 2-PoW-for 1 technique, so sub-block (input block) is defined not by
  // hash(b) < T/subsPerBlock , but by reverse(hash(b)) < T/subsPerBlock , while ordering block is defined
  // by hash(b) < T

  // sub blocks per block, adjustable via miners voting
  // todo: likely we need to update rule exMatchParameters (#409) to add new parameter to vote
  val subsPerBlock = Parameters.SubsPerBlockDefault

  val weakTransactionIdLength = 6 // value taken from Bitcoin's compact blocks BIP

  lazy val powScheme = new AutolykosPowScheme(32, 26)

  sealed trait BlockKind

  case object InputBlock extends BlockKind
  case object OrderingBlock extends BlockKind
  case object InvalidPoWBlock extends BlockKind

  def blockKind(header: Header): BlockKind = {
    val fullTarget = powScheme.getB(header.nBits)
    val subTarget = fullTarget * subsPerBlock
    val hit = powScheme.hitForVersion2(header) // todo: cache hit in header

    // todo: consider 2-for-1 pow technique
    if (hit < subTarget) {
      InputBlock
    } else if (hit >= subTarget && hit < fullTarget) {
      OrderingBlock
    } else {
      InvalidPoWBlock
    }
  }

  def checkInputBlockPoW(header: Header): Boolean = {
    val hit = powScheme.hitForVersion2(header) // todo: cache hit in header

    val orderingTarget = powScheme.getB(header.nBits)
    val inputTarget = orderingTarget * subsPerBlock
    hit < inputTarget
  }

  // messages:
  //
  // sub block signal:
  // version
  // sub block (~200 bytes) - contains a link to parent block
  // previous sub block id
  // transactions since last sub blocks



  /**
    * On receiving sub block or block, the node is sending last sub block or block id it has to get short transaction
    * ids since then
    */
  object GetDataSpec extends MessageSpecInitial[ModifierId] {

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

  class DataSpec extends MessageSpecInitial[TransactionsSince] {

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
  def processSubBlock(sbi: InputBlockInfo): (Seq[ModifierId], Seq[ModifierId]) = {
    val sbHeader = sbi.header
    val prevSbIdOpt = sbi.prevInputBlockId.map(bytesToId)
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

