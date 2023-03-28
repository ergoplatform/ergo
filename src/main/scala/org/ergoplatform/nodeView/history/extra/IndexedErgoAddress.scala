package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.{ErgoAddressEncoder, ErgoBox}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.{ExtraIndexTypeId, fastIdToBytes}
import org.ergoplatform.nodeView.history.extra.IndexedErgoAddress.{getBoxes, getSegmentsForRange, getTxs, segmentTreshold, slice}
import org.ergoplatform.nodeView.history.extra.IndexedErgoAddressSerializer.{boxSegmentId, txSegmentId}
import org.ergoplatform.settings.Algos
import scorex.core.serialization.ErgoSerializer
import scorex.util.{ModifierId, ScorexLogging, bytesToId}
import scorex.util.serialization.{Reader, Writer}
import sigmastate.Values.ErgoTree

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import spire.syntax.all.cfor

/**
  * An index of an address (ErgoTree)
  * @param treeHash    - hash of the corresponding ErgoTree
  * @param txs         - list of numberic transaction indexes associated with this address
  * @param boxes       - list of numberic box indexes associated with this address
  * @param balanceInfo - balance information (Optional because fragments do not contain it)
  */
case class IndexedErgoAddress(treeHash: ModifierId,
                              txs: ListBuffer[Long],
                              boxes: ListBuffer[Long],
                              balanceInfo: Option[BalanceInfo]) extends ExtraIndex with ScorexLogging {

  override def id: ModifierId = treeHash
  override def serializedId: Array[Byte] = fastIdToBytes(treeHash)

  private[extra] var boxSegmentCount: Int = 0
  private[extra] var txSegmentCount: Int = 0

  /**
    * @return total number of transactions associated with this address
    */
  def txCount(): Long = segmentTreshold * txSegmentCount + txs.length

  /**
    * @return total number of boxes associated with this address
    */
  def boxCount(): Long = segmentTreshold * boxSegmentCount + boxes.length

  /**
    * Get a range of the transactions associated with this address
    * @param history - history to use
    * @param offset  - items to skip from the start
    * @param limit   - items to retrieve
    * @return array of transactions with full bodies
    */
  def retrieveTxs(history: ErgoHistoryReader, offset: Int, limit: Int): Array[IndexedErgoTransaction] = {
    if(offset + limit > txs.length && txSegmentCount > 0) {
      val range: Array[Int] = getSegmentsForRange(offset, limit)
      val data: ListBuffer[Long] = ListBuffer.empty[Long]
      cfor(0)(_ < range.length, _ + 1) { i =>
        history.typedExtraIndexById[IndexedErgoAddress](txSegmentId(treeHash, txSegmentCount - range(i))).get.txs ++=: data
      }
      getTxs(slice(data ++= (if(offset < txs.length) txs else Nil), offset % segmentTreshold, limit))(history)
    } else
      getTxs(slice(txs, offset, limit))(history)
  }

  /**
    * Get a range of the boxes associated with this address
    * @param history - history to use
    * @param offset  - items to skip from the start
    * @param limit   - items to retrieve
    * @return array of boxes
    */
  def retrieveBoxes(history: ErgoHistoryReader, offset: Int, limit: Int): Array[IndexedErgoBox] = {
    if(offset + limit > boxes.length && boxSegmentCount > 0) {
      val range: Array[Int] = getSegmentsForRange(offset, limit)
      val data: ListBuffer[Long] = ListBuffer.empty[Long]
      cfor(0)(_ < range.length, _ + 1) { i =>
        history.typedExtraIndexById[IndexedErgoAddress](boxSegmentId(treeHash, boxSegmentCount - range(i))).get.boxes ++=: data
      }
      getBoxes(slice(data ++= (if(offset < boxes.length) boxes else Nil), offset % segmentTreshold, limit))(history)
    } else
      getBoxes(slice(boxes, offset, limit))(history)
  }

  /**
    * Get a range of the boxes associated with this address that are NOT spent
    * @param history - history to use
    * @param offset  - items to skip from the start
    * @param limit   - items to retrieve
    * @return array of unspent boxes
    */
  def retrieveUtxos(history: ErgoHistoryReader, offset: Int, limit: Int): Array[IndexedErgoBox] = {
    val data: ListBuffer[IndexedErgoBox] = ListBuffer.empty[IndexedErgoBox]
    data ++= boxes.map(n => NumericBoxIndex.getBoxByNumber(history, n).get).filter(!_.trackedBox.isSpent)
    var segment: Int = boxSegmentCount
    while(data.length < limit && segment > 0) {
      segment -= 1
      history.typedExtraIndexById[IndexedErgoAddress](boxSegmentId(treeHash, segment)).get.boxes
        .map(n => NumericBoxIndex.getBoxByNumber(history, n).get).filter(!_.trackedBox.isSpent) ++=: data
    }
    slice(data, offset, limit).toArray
  }

  /**
    * Associate transaction index with this address
    * @param tx - numeric transaction index
    * @return this address
    */
  private[extra] def addTx(tx: Long): IndexedErgoAddress = {
    if(txs.lastOption.getOrElse(-1) != tx) txs += tx // check for duplicates
    this
  }

  /**
    * Associate box with this address and update BalanceInfo
    * @param iEb - box to use
    * @param record - whether to add box to boxes list, used in rollbacks (true by default)
    * @return this address
    */
  private[extra] def addBox(iEb: IndexedErgoBox, record: Boolean = true): IndexedErgoAddress = {
    if(record) boxes += iEb.globalIndex
    balanceInfo.get.add(iEb.box)
    this
  }

  /**
    * Update BalanceInfo by spending a box associated with this address
    * @param box - box to spend
    * @return this address
    */
  private[extra] def spendBox(box: ErgoBox)(implicit ae: ErgoAddressEncoder): IndexedErgoAddress = {
    balanceInfo.get.subtract(box)
    this
  }

  /**
    * Rollback the state of this address and of boxes associted with it
    * @param txTarget  - remove transaction numbers above this number
    * @param boxTarget - remove box numbers above this number and revert the balance
    * @param _history  - history handle to update address in database
    */
  private[extra] def rollback(txTarget: Long, boxTarget: Long)(_history: ErgoHistory): Unit = {

    if(txs.last <= txTarget && boxes.last <= boxTarget) return

    def history: ErgoHistoryReader = _history.getReader

    val toSave: ArrayBuffer[ExtraIndex] = ArrayBuffer.empty[ExtraIndex]
    val toRemove: ArrayBuffer[ModifierId] = ArrayBuffer.empty[ModifierId]

    // filter tx numbers
    do {
      val tmp = txs.takeWhile(_ <= txTarget)
      txs.clear()
      txs ++= tmp
      if(txs.isEmpty && txSegmentCount > 0) { // entire current tx set removed, retrieving more from database if possible
        val id = txSegmentId(treeHash, txSegmentCount - 1)
        txs ++= history.typedExtraIndexById[IndexedErgoAddress](id).get.txs
        toRemove += id
        txSegmentCount -= 1
      }
    }while(txCount() > 0 && txs.last > txTarget)

    // filter box numbers
    do {
      val tmp = boxes.takeWhile(_ <= boxTarget)
      boxes.clear()
      boxes ++= tmp
      if(boxes.isEmpty && boxSegmentCount > 0) { // entire current box set removed, retrieving more from database if possible
        val id = boxSegmentId(treeHash, boxSegmentCount - 1)
        boxes ++= history.typedExtraIndexById[IndexedErgoAddress](id).get.boxes
        toRemove += id
        boxSegmentCount -= 1
      }
    }while(boxCount() > 0 && boxes.last > boxTarget)

    if(txCount() == 0 && boxCount() == 0)
      toRemove += this.id // address is empty after rollback, delete
    else
      toSave += this // save the changes made to this address

    _history.historyStorage.insertExtra(Array.empty, toSave.toArray)
    _history.historyStorage.removeExtra(toRemove.toArray)

  }

  /**
    * Create an array addresses each containing a "segmentTreshold" number of this address's transaction and box indexes.
    * These special addresses have their ids calculated by "txSegmentId" and "boxSegmentId" respectively.
    * @return array of addresses
    */
  private[extra] def splitToSegments(): Array[IndexedErgoAddress] = {
    val data: Array[IndexedErgoAddress] = new Array[IndexedErgoAddress]((txs.length / segmentTreshold) + (boxes.length / segmentTreshold))
    var i: Int = 0
    while(txs.length >= segmentTreshold) {
      data(i) = new IndexedErgoAddress(txSegmentId(treeHash, txSegmentCount), txs.take(segmentTreshold), ListBuffer.empty[Long], None)
      i += 1
      txSegmentCount += 1
      txs.remove(0, segmentTreshold)
    }
    while(boxes.length >= segmentTreshold) {
      data(i) = new IndexedErgoAddress(boxSegmentId(treeHash, boxSegmentCount), ListBuffer.empty[Long], boxes.take(segmentTreshold), None)
      i += 1
      boxSegmentCount += 1
      boxes.remove(0, segmentTreshold)
    }
    data
  }
}

object IndexedErgoAddressSerializer extends ErgoSerializer[IndexedErgoAddress] {

  def hashErgoTree(tree: ErgoTree): Array[Byte] = Algos.hash(tree.bytes)

  /**
    * Calculates id of an address segment containing box indexes.
    * @param hash       - hash of the parent addresses ErgoTree
    * @param segmentNum - numberic identifier of the segment
    * @return calculated ModifierId
    */
  def boxSegmentId(hash: ModifierId, segmentNum: Int): ModifierId = bytesToId(Algos.hash(hash + " box segment " + segmentNum))

  /**
    * Calculates id of an address segment transaction indexes.
    * @param hash       - hash of the parent addresses ErgoTree
    * @param segmentNum - numberic identifier of the segment
    * @return calculated ModifierId
    */
  def txSegmentId(hash: ModifierId, segmentNum: Int): ModifierId = bytesToId(Algos.hash(hash + " tx segment " + segmentNum))

  override def serialize(iEa: IndexedErgoAddress, w: Writer): Unit = {
    w.putBytes(fastIdToBytes(iEa.treeHash))
    w.putUInt(iEa.txs.length)
    cfor(0)(_ < iEa.txs.length, _ + 1) { i => w.putLong(iEa.txs(i))}
    w.putUInt(iEa.boxes.length)
    cfor(0)(_ < iEa.boxes.length, _ + 1) { i => w.putLong(iEa.boxes(i))}
    w.putOption[BalanceInfo](iEa.balanceInfo)((ww, bI) => BalanceInfoSerializer.serialize(bI, ww))
    w.putInt(iEa.boxSegmentCount)
    w.putInt(iEa.txSegmentCount)
  }

  override def parse(r: Reader): IndexedErgoAddress = {
    val addressHash: ModifierId = bytesToId(r.getBytes(32))
    val txnsLen: Long = r.getUInt()
    val txns: ListBuffer[Long] = ListBuffer.empty[Long]
    cfor(0)(_ < txnsLen, _ + 1) { _ => txns += r.getLong()}
    val boxesLen: Long = r.getUInt()
    val boxes: ListBuffer[Long] = ListBuffer.empty[Long]
    cfor(0)(_ < boxesLen, _ + 1) { _ => boxes += r.getLong()}
    val balanceInfo: Option[BalanceInfo] = r.getOption[BalanceInfo](BalanceInfoSerializer.parse(r))
    val iEa: IndexedErgoAddress = new IndexedErgoAddress(addressHash, txns, boxes, balanceInfo)
    iEa.boxSegmentCount = r.getInt()
    iEa.txSegmentCount = r.getInt()
    iEa
  }
}

object IndexedErgoAddress {

  val extraIndexTypeId: ExtraIndexTypeId = 15.toByte

  val segmentTreshold: Int = 512

  /**
    * Calculate the segment offsets for the given range.
    * @param offset - items to skip from the start
    * @param limit  - items to retrieve
    * @return array of offsets
    */
  def getSegmentsForRange(offset: Int, limit: Int): Array[Int] =
    (math.max(math.ceil(offset * 1F / segmentTreshold).toInt, 1) to math.ceil((offset + limit) * 1F / segmentTreshold).toInt).toArray

  /**
    * Shorthand to get a range from an array by offset and limit.
    * @param arr    - array to get range from
    * @param offset - items to skip from the start
    * @param limit  - items to retrieve
    * @tparam T     - type of "arr" array
    * @return range in "arr" array
    */
  def slice[T](arr: Iterable[T], offset: Int, limit: Int): Iterable[T] =
    arr.slice(arr.size - offset - limit, arr.size - offset)

  /**
    * Get an array of transactions with full bodies from an array of numeric transaction indexes
    * @param arr     - array of numeric transaction indexes to retrieve
    * @param history - database handle
    * @return array of transactions with full bodies
    */
  def getTxs(arr: Iterable[Long])(history: ErgoHistoryReader): Array[IndexedErgoTransaction] = // sorted to match explorer
    arr.map(n => NumericTxIndex.getTxByNumber(history, n).get.retrieveBody(history)).toArray.sortBy(tx => (-tx.height, tx.id))

  /**
    * Get an array of boxes from an array of numeric box indexes
    * @param arr     - array of numeric box indexes to retrieve
    * @param history - database handle
    * @return array of boxes
    */
  def getBoxes(arr: Iterable[Long])(history: ErgoHistoryReader): Array[IndexedErgoBox] =
    arr.map(n => NumericBoxIndex.getBoxByNumber(history, n).get).toArray
}
