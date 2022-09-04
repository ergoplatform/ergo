package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.DataInput
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.nodeView.history.extra.ExtraIndexerRef.fastIdToBytes
import scorex.core.serialization.ScorexSerializer
import scorex.core.ModifierTypeId
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, ScorexLogging, bytesToId}

case class IndexedErgoTransaction(txid: ModifierId,
                                  height: Int,
                                  globalIndex: Long) extends BlockSection with ScorexLogging {

  override val modifierTypeId: ModifierTypeId = IndexedErgoTransaction.modifierTypeId
  override def serializedId: Array[Byte] = fastIdToBytes(txid)
  override val sizeOpt: Option[Int] = None
  override def parentId: ModifierId = null
  override type M = IndexedErgoTransaction
  override def serializer: ScorexSerializer[IndexedErgoTransaction] = IndexedErgoTransactionSerializer

  private var _blockId: ModifierId = ModifierId @@ ""
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

    val header: Header = history.typedModifierById[Header](history.bestHeaderIdAtHeight(height).get).get
    val blockTxs: BlockTransactions = history.typedModifierById[BlockTransactions](header.transactionsId).get

    _blockId = header.id
    _inclusionHeight = height
    _timestamp = header.timestamp
    _index = blockTxs.txs.indices.find(blockTxs.txs(_).id == txid).get
    _numConfirmations = history.bestFullBlockOpt.get.height - height
    _inputs = blockTxs.txs(_index).inputs.map(input => history.typedModifierById[IndexedErgoBox](bytesToId(input.boxId)).get)
    _dataInputs = blockTxs.txs(_index).dataInputs
    _outputs = blockTxs.txs(_index).outputs.map(output => history.typedModifierById[IndexedErgoBox](bytesToId(output.id)).get)
    _txSize = blockTxs.txs(_index).size

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
