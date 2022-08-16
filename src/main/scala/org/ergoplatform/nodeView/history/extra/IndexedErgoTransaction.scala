package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock}
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.DataInput
import scorex.core.serialization.ScorexSerializer
import scorex.core.{ModifierTypeId, idToBytes}
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, bytesToId}

case class IndexedErgoTransaction(txid: ModifierId,
                                  height: Int,
                                  globalIndex: Long) extends BlockSection {

  override val modifierTypeId: ModifierTypeId = IndexedErgoTransaction.modifierTypeId
  override def serializedId: Array[Byte] = idToBytes(txid)
  override val sizeOpt: Option[Int] = None
  override def parentId: ModifierId = null
  override type M = IndexedErgoTransaction
  override def serializer: ScorexSerializer[IndexedErgoTransaction] = IndexedErgoTransactionSerializer

  private var _blockId: ModifierId = _
  private var _inclusionHeight: Int = 0
  private var _timestamp: Header.Timestamp = 0L
  private var _index: Int = 0
  private var _numConfirmations: Int = 0
  private var _inputs: IndexedSeq[IndexedErgoBox] = IndexedSeq.empty[IndexedErgoBox]
  private var _dataInputs: IndexedSeq[DataInput] = IndexedSeq.empty[DataInput]
  private var _outputs: IndexedSeq[IndexedErgoBox] = IndexedSeq.empty[IndexedErgoBox]
  private var _txSize: Int = 0

  def blockId: ModifierId = _blockId
  def inclusionHeight: Int = _inclusionHeight
  def timestamp: Header.Timestamp = _timestamp
  def index: Int = _index
  def numConfirmations: Int = _numConfirmations
  def inputs: IndexedSeq[IndexedErgoBox] = _inputs
  def dataInputs: IndexedSeq[DataInput] = _dataInputs
  def outputs: IndexedSeq[IndexedErgoBox] = _outputs
  def txSize: Int = _txSize

  def retrieveBody(history: ErgoHistoryReader): IndexedErgoTransaction = {

    val block: ErgoFullBlock = history.typedModifierById[Header](history.headerIdsAtHeight(height).head).flatMap(history.getFullBlock).get
    val indexInBlock: Int = block.transactions.indices.find(block.transactions(_).id.equals(txid)).get
    val tx: ErgoTransaction = block.transactions(indexInBlock)

    _blockId = block.id
    _inclusionHeight = block.height
    _timestamp = block.header.timestamp
    _index = indexInBlock
    _numConfirmations = history.bestFullBlockOpt.get.height - block.height
    _inputs = tx.inputs.map(input => history.typedModifierById[IndexedErgoBox](bytesToId(input.boxId)).get)
    _dataInputs = tx.dataInputs
    _outputs = tx.outputs.map(output => history.typedModifierById[IndexedErgoBox](bytesToId(output.id)).get)
    _txSize = tx.size

    this
  }
}

object IndexedErgoTransactionSerializer extends ScorexSerializer[IndexedErgoTransaction] {

  override def serialize(iTx: IndexedErgoTransaction, w: Writer): Unit = {
    w.putUByte(iTx.serializedId.length)
    w.putBytes(iTx.serializedId)
    w.putInt(iTx.height)
    w.putLong(iTx.globalIndex)
  }

  override def parse(r: Reader): IndexedErgoTransaction = {
    val idLen = r.getUByte()
    val id = bytesToId(r.getBytes(idLen))
    val height = r.getInt()
    val globalIndex = r.getLong()
    IndexedErgoTransaction(id, height, globalIndex)
  }
}

object IndexedErgoTransaction {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ 10.toByte
}
