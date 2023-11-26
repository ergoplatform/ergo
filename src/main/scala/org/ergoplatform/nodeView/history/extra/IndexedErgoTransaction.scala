package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.DataInput
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.{ExtraIndexTypeId, fastIdToBytes}
import scorex.core.serialization.ErgoSerializer
import scorex.crypto.authds.ADKey
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, bytesToId}
import spire.implicits.cfor

/**
  * Minimum general information for transaction. Not storing the whole transation is done to save space.
  * @param txid        - id of this transaction
  * @param index       - index of transaction in parent block
  * @param height      - height of the block which includes this transaction
  * @param size        - size of this transaction in bytes
  * @param globalIndex - serial number of this transaction counting from block 1
  * @param inputNums   - list of transaction inputs
  * @param outputNums  - list of transaction outputs
  */
case class IndexedErgoTransaction(txid: ModifierId,
                                  index: Int,
                                  height: Int,
                                  size: Int,
                                  globalIndex: Long,
                                  inputNums: Array[Long],
                                  outputNums: Array[Long],
                                  dataInputs: Array[DataInput]) extends ExtraIndex {

  override lazy val id: ModifierId = txid
  override def serializedId: Array[Byte] = fastIdToBytes(id)

  private var _blockId: ModifierId = ModifierId @@ ""
  private var _inclusionHeight: Int = 0
  private var _timestamp: Header.Timestamp = 0L
  private var _numConfirmations: Int = 0
  private var _inputs: IndexedSeq[IndexedErgoBox] = IndexedSeq.empty[IndexedErgoBox]
  private var _outputs: IndexedSeq[IndexedErgoBox] = IndexedSeq.empty[IndexedErgoBox]

  def blockId: ModifierId = _blockId
  def inclusionHeight: Int = _inclusionHeight
  def timestamp: Header.Timestamp = _timestamp
  def numConfirmations: Int = _numConfirmations
  def inputs: IndexedSeq[IndexedErgoBox] = _inputs
  def outputs: IndexedSeq[IndexedErgoBox] = _outputs

  /**
    * Get all information related to this transaction from database.
    * @param history - database handle
    * @return this transaction augmented with additional information
    */
  def retrieveBody(history: ErgoHistoryReader): IndexedErgoTransaction = {

    val header: Header = history.typedModifierById[Header](history.bestHeaderIdAtHeight(height).get).get

    _blockId = header.id
    _inclusionHeight = height
    _timestamp = header.timestamp
    _numConfirmations = history.fullBlockHeight - height
    _inputs = inputNums.flatMap(NumericBoxIndex.getBoxByNumber(history, _))
    _outputs = outputNums.flatMap(NumericBoxIndex.getBoxByNumber(history, _))

    this
  }
}

object IndexedErgoTransactionSerializer extends ErgoSerializer[IndexedErgoTransaction] {

  override def serialize(iTx: IndexedErgoTransaction, w: Writer): Unit = {
    w.putUByte(iTx.serializedId.length)
    w.putBytes(iTx.serializedId)
    w.putInt(iTx.index)
    w.putInt(iTx.height)
    w.putInt(iTx.size)
    w.putLong(iTx.globalIndex)
    w.putUShort(iTx.inputNums.length)
    cfor(0)(_ < iTx.inputNums.length, _ + 1) { i => w.putLong(iTx.inputNums(i)) }
    w.putUShort(iTx.outputNums.length)
    cfor(0)(_ < iTx.outputNums.length, _ + 1) { i => w.putLong(iTx.outputNums(i)) }
    w.putUShort(iTx.dataInputs.length)
    cfor(0)(_ < iTx.dataInputs.length, _ + 1) { i => w.putBytes(iTx.dataInputs(i).boxId) }
  }

  override def parse(r: Reader): IndexedErgoTransaction = {
    val idLen = r.getUByte()
    val id = bytesToId(r.getBytes(idLen))
    val index = r.getInt()
    val height = r.getInt()
    val size = r.getInt()
    val globalIndex = r.getLong()
    val inputCount: Int = r.getUShort()
    val inputNums: Array[Long] = Array.ofDim[Long](inputCount)
    cfor(0)(_ < inputCount, _ + 1) { i => inputNums(i) = r.getLong() }
    val outputCount: Int = r.getUShort()
    val outputNums: Array[Long] = Array.ofDim[Long](outputCount)
    cfor(0)(_ < outputCount, _ + 1) { i => outputNums(i) = r.getLong() }
    val dataInputsCount = r.getUShort()
    val dataInputs: Array[DataInput] = Array.ofDim[DataInput](dataInputsCount)
    cfor(0)(_ < dataInputsCount, _ + 1) { i => dataInputs(i) = DataInput(ADKey @@ r.getBytes(32)) }
    IndexedErgoTransaction(id, index, height, size, globalIndex, inputNums, outputNums, dataInputs)
  }
}

object IndexedErgoTransaction {
  val extraIndexTypeId: ExtraIndexTypeId = 10.toByte

  def fromTx(tx: ErgoTransaction, index: Int, height: Int, globalIndex: Long, inputs: Array[Long], outputs: Array[Long]): IndexedErgoTransaction =
    IndexedErgoTransaction(tx.id, index, height, tx.size, globalIndex, inputs, outputs, tx.dataInputs.toArray)
}
