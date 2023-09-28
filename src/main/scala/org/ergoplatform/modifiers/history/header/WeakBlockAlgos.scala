package org.ergoplatform.modifiers.history.header

import org.ergoplatform.nodeView.history.ErgoHistory.Difficulty
import scorex.core.consensus.SyncInfo
import scorex.core.network.message.Message.MessageCode
import scorex.core.network.message.MessageSpecV1
import scorex.core.serialization.{BytesSerializable, ErgoSerializer}
import scorex.util.serialization.{Reader, Writer}
import scorex.util.Extensions._

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

  def isWeak(header: Header, requiredDifficulty: Difficulty): Boolean = {
    val diff = requiredDifficulty / weaksPerBlock
    header.requiredDifficulty >= diff
  }



  // messages:
  //
  // weak block signal:
  // version
  // weak block (~200 bytes) - contains a link to parent block
  // previous weak block
  // transactions since last weak blocks (8 byte ids?)

  class WeakBlockInfo(version: Byte, weakBlock: Header, prevWeakBlockId: Array[Byte], txsSinceLastWeak: Array[Array[Byte]]) extends BytesSerializable {
    override type M = WeakBlockInfo

    val weakTransactionIdLength = 8
    /**
      * Serializer which can convert self to bytes
      */
    override def serializer: ErgoSerializer[WeakBlockInfo] = new ErgoSerializer[WeakBlockInfo] {
      override def serialize(wbi: WeakBlockInfo, w: Writer): Unit = {
        w.put(version)
        HeaderSerializer.serialize(weakBlock, w)
        w.putBytes(prevWeakBlockId)
        w.putUShort(txsSinceLastWeak.length) // consider case when more txs than can be in short
        txsSinceLastWeak.foreach(txId => w.putBytes(txId))
      }

      override def parse(r: Reader): WeakBlockInfo = {
        val version = r.getByte()
        val weakBlock = HeaderSerializer.parse(r)
        val prevWeakBlockId = r.getBytes(32)
        val txsCount = r.getUShort().toShortExact
        val txsSinceLastWeak  = (1 to txsCount).map{_ => // todo: more efficient array construction
          r.getBytes(weakTransactionIdLength)
        }.toArray
        new WeakBlockInfo(version, weakBlock, prevWeakBlockId, txsSinceLastWeak)
      }
    }
  }

  class WeakBlockMessageSpec[SI <: SyncInfo](serializer: ErgoSerializer[SI]) extends MessageSpecV1[SI] {

    override val messageCode: MessageCode = 90: Byte
    override val messageName: String = "Sync"

    override def serialize(data: SI, w: Writer): Unit = serializer.serialize(data, w)

    override def parse(r: Reader): SI = serializer.parse(r)
  }


}
