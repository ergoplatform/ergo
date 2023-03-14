package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.http.api.SortDirection.{ASC, DESC, Direction}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.{ExtraIndexTypeId, fastIdToBytes}
import org.ergoplatform.nodeView.history.extra.IndexedErgoAddress.{getBoxes, getFromSegments, getTxs, segmentTreshold, slice}
import org.ergoplatform.nodeView.history.extra.IndexedErgoAddressSerializer.{boxSegmentId, txSegmentId}
import org.ergoplatform.settings.Algos
import scorex.core.serialization.ScorexSerializer
import scorex.util.{ModifierId, ScorexLogging, bytesToId}
import scorex.util.serialization.{Reader, Writer}
import sigmastate.Values.ErgoTree

import scala.collection.mutable.ArrayBuffer
import spire.syntax.all.cfor

import java.lang.Math.abs

/**
  * An index of an address (ErgoTree)
  * @param treeHash    - hash of the corresponding ErgoTree
  * @param txs         - list of numberic transaction indexes associated with this address
  * @param boxes       - list of numberic box indexes associated with this address, negative values indicate the box is spent
  * @param balanceInfo - balance information (Optional because fragments do not contain it)
  */
case class IndexedErgoAddress(treeHash: ModifierId,
                              txs: ArrayBuffer[Long],
                              boxes: ArrayBuffer[Long],
                              balanceInfo: Option[BalanceInfo]) extends ExtraIndex with ScorexLogging {

  override lazy val id: ModifierId = treeHash

  // Internal segment buffer used when spending boxes
  private[extra] val segments: ArrayBuffer[IndexedErgoAddress] = ArrayBuffer.empty[IndexedErgoAddress]

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
    * Copied from [[java.util.Arrays.binarySearch]]
    */
  private def binarySearch(a: ArrayBuffer[Long], key: Long): Int = {
    var low: Int = 0
    var high: Int = a.length - 1

    while (low <= high) {
      val mid = (low + high) >>> 1
      val midVal = abs(a(mid)) // ignore negativity

      if (midVal < key)
        low = mid + 1
      else if (midVal > key)
        high = mid - 1
      else
        return mid // key found
    }
    -1 // key not found.
  }

  /**
    * Retrieve segment with specified id from buffer or database
    * @param history - database handle to search, if segment is not found in buffer
    * @param id - address segment to search for
    * @return
    */
  private def getSegmentFromBufferOrHistroy(history: ErgoHistoryReader, id: ModifierId): Int = {
    cfor(segments.length - 1)(_ >= 0, _ - 1) { i =>
      if(segments(i).id.equals(id)) return i
    }
    segments += history.typedExtraIndexById[IndexedErgoAddress](id).get
    segments.length - 1
  }

  /**
    * Locate which segment the given box number is in and change its sign, meaning it spends unspent boxes and vice versa.
    * @param boxNum - box number to locate
    * @param history - database to retrieve swegments from
    */
  private[extra] def findAndModBox(boxNum: Long, history: ErgoHistoryReader): Unit = {
    val boxNumAbs = abs(boxNum)
    val inCurrent: Int = binarySearch(boxes, boxNumAbs)
    if(inCurrent >= 0) { // box found in current box array
      boxes(inCurrent) = -boxes(inCurrent)
    } else { // box is in another segment, use binary search to locate
      var n = 0
      var low = 0
      var high = boxSegmentCount - 1
      while(low <= high) {
        val mid = (low + high) >>> 1
        n = getSegmentFromBufferOrHistroy(history, boxSegmentId(treeHash, mid))
        if(abs(segments(n).boxes.head) < boxNumAbs &&
           abs(segments(n).boxes.last) < boxNumAbs)
          low = mid + 1
        else if(abs(segments(n).boxes.head) > boxNumAbs &&
                abs(segments(n).boxes.last) > boxNumAbs)
          high = mid - 1
        else
          low = high + 1 // break
      }
      val i: Int = binarySearch(segments(n).boxes, boxNumAbs)
      if(i >= 0)
        segments(n).boxes(i) = -segments(n).boxes(i)
      else
        log.warn(s"Box $boxNum not found in any segment of parent address when trying to spend")
    }
  }

  /**
    * Get a range of the transactions associated with this address
    * @param history - history to use
    * @param offset  - items to skip from the start
    * @param limit   - items to retrieve
    * @return array of transactions with full bodies
    */
  def retrieveTxs(history: ErgoHistoryReader, offset: Int, limit: Int): Array[IndexedErgoTransaction] =
    getFromSegments(history, treeHash, offset, limit, txSegmentCount, txs, txSegmentId, _.txs, getTxs)

  /**
    * Get a range of the boxes associated with this address
    * @param history - history to use
    * @param offset  - items to skip from the start
    * @param limit   - items to retrieve
    * @return array of boxes
    */
  def retrieveBoxes(history: ErgoHistoryReader, offset: Int, limit: Int): Array[IndexedErgoBox] =
    getFromSegments(history, treeHash, offset, limit, boxSegmentCount, boxes, boxSegmentId, _.boxes, getBoxes)

  /**
    * Get a range of the boxes associated with this address that are NOT spent
    * @param history - history to use
    * @param offset  - items to skip from the start
    * @param limit   - items to retrieve
    * @param sortDir - whether to start retreival from newest box ([[DESC]]) or oldest box ([[ASC]])
    * @return array of unspent boxes
    */
  def retrieveUtxos(history: ErgoHistoryReader, offset: Int, limit: Int, sortDir: Direction): Array[IndexedErgoBox] = {
    val data: ArrayBuffer[IndexedErgoBox] = ArrayBuffer.empty[IndexedErgoBox]
    sortDir match {
      case DESC =>
        data ++= boxes.filter(_ > 0).map(n => NumericBoxIndex.getBoxByNumber(history, n).get)
        var segment: Int = boxSegmentCount
        while (data.length < (limit + offset) && segment > 0) {
          segment -= 1
          history.typedExtraIndexById[IndexedErgoAddress](boxSegmentId(treeHash, segment)).get.boxes
            .filter(_ > 0).map(n => NumericBoxIndex.getBoxByNumber(history, n).get) ++=: data
        }
        slice(data.reverse, offset, limit).toArray
      case ASC =>
        var segment: Int = 0
        while (data.length < (limit + offset) && segment < boxSegmentCount) {
          data ++= history.typedExtraIndexById[IndexedErgoAddress](boxSegmentId(treeHash, segment)).get.boxes
            .filter(_ > 0).map(n => NumericBoxIndex.getBoxByNumber(history, n).get)
          segment += 1
        }
        if(data.length < (limit + offset))
          data ++= boxes.filter(_ > 0).map(n => NumericBoxIndex.getBoxByNumber(history, n).get)
        slice(data, offset, limit).toArray
    }
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
    * @param iEb - box to spend
    * @param historyOpt - history handle to update address fragment if spent box is old
    * @return this address
    */
  private[extra] def spendBox(iEb: IndexedErgoBox, historyOpt: Option[ErgoHistoryReader] = None)(implicit ae: ErgoAddressEncoder): IndexedErgoAddress = {
    balanceInfo.get.subtract(iEb.box)

    if(historyOpt.isEmpty)
      return this

    findAndModBox(iEb.globalIndex, historyOpt.get)

    this
  }

  /**
    * Rollback the state of this address and of boxes associted with it
    * @param txTarget  - remove transaction numbers above this number
    * @param boxTarget - remove box numbers above this number and revert the balance
    * @param _history  - history handle to update address in database
    */
  private[extra] def rollback(txTarget: Long, boxTarget: Long)(_history: ErgoHistory): Unit = {

    if(txs.last <= txTarget && abs(boxes.last) <= boxTarget) return

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
      val tmp = boxes.takeWhile(abs(_) <= boxTarget)
      boxes.clear()
      boxes ++= tmp
      if(boxes.isEmpty && boxSegmentCount > 0) { // entire current box set removed, retrieving more from database if possible
        val id = boxSegmentId(treeHash, boxSegmentCount - 1)
        boxes ++= history.typedExtraIndexById[IndexedErgoAddress](id).get.boxes
        toRemove += id
        boxSegmentCount -= 1
      }
    }while(boxCount() > 0 && abs(boxes.last) > boxTarget)

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
  private[extra] def splitToSegments: Array[IndexedErgoAddress] = {
    val data: Array[IndexedErgoAddress] = new Array[IndexedErgoAddress]((txs.length / segmentTreshold) + (boxes.length / segmentTreshold))
    var i: Int = 0
    while(txs.length >= segmentTreshold) {
      data(i) = new IndexedErgoAddress(txSegmentId(treeHash, txSegmentCount), txs.take(segmentTreshold), ArrayBuffer.empty[Long], None)
      i += 1
      txSegmentCount += 1
      txs.remove(0, segmentTreshold)
    }
    while(boxes.length >= segmentTreshold) {
      data(i) = new IndexedErgoAddress(boxSegmentId(treeHash, boxSegmentCount), ArrayBuffer.empty[Long], boxes.take(segmentTreshold), None)
      i += 1
      boxSegmentCount += 1
      boxes.remove(0, segmentTreshold)
    }
    data
  }
}

object IndexedErgoAddressSerializer extends ScorexSerializer[IndexedErgoAddress] {

  def hashErgoTree(tree: ErgoTree): ModifierId = bytesToId(Algos.hash(tree.bytes))

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
    val txns: ArrayBuffer[Long] = ArrayBuffer.empty[Long]
    cfor(0)(_ < txnsLen, _ + 1) { _ => txns += r.getLong()}
    val boxesLen: Long = r.getUInt()
    val boxes: ArrayBuffer[Long] = ArrayBuffer.empty[Long]
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
  private def getSegmentsForRange(offset: Int, limit: Int): Array[Int] =
    (math.max(math.ceil(offset * 1F / segmentTreshold).toInt, 1) to math.ceil((offset + limit) * 1F / segmentTreshold).toInt).toArray

  /**
    * Shorthand to get a range from an ArrayBuffer by offset and limit.
    * @param arr    - array to get range from
    * @param offset - items to skip from the start
    * @param limit  - items to retrieve
    * @tparam T     - type of ArrayBuffer
    * @return range in "arr" ArrayBuffer
    */
  private def slice[T](arr: ArrayBuffer[T], offset: Int, limit: Int): ArrayBuffer[T] =
    arr.slice(offset, offset + limit)

  /**
    * Get an array of transactions with full bodies from an array of numeric transaction indexes
    * @param arr     - array of numeric transaction indexes to retrieve
    * @param history - database handle
    * @return array of transactions with full bodies
    */
  private def getTxs(arr: ArrayBuffer[Long], history: ErgoHistoryReader): Array[IndexedErgoTransaction] = // sorted to match explorer
    arr.map(n => NumericTxIndex.getTxByNumber(history, n).get.retrieveBody(history)).toArray.sortBy(tx => (-tx.height, tx.id))

  /**
    * Get an array of boxes from an array of numeric box indexes
    * @param arr     - array of numeric box indexes to retrieve
    * @param history - database handle
    * @return array of boxes
    */
  private def getBoxes(arr: ArrayBuffer[Long], history: ErgoHistoryReader): Array[IndexedErgoBox] =
    arr.map(n => NumericBoxIndex.getBoxByNumber(history, n).get).toArray

  /**
    * Get a set of address segments from database containing numeric transaction or box indexes. Then actually retreive these indexes.
    * @param history - database handle
    * @param treeHash - hash of parent ErgoTree
    * @param offset - number of items to skip from the start
    * @param limit - max number of item to be returned
    * @param segmentCount - number of segments of the parent address
    * @param array - the indexes already in memory
    * @param idOf - function to calculate segment ids, either [[txSegmentId]] or [[boxSegmentId]]
    * @param arraySelector - function to select index array from retreived segments
    * @param retreive - function to retreive indexes from database
    * @tparam T - type of desired indexes, either [[IndexedErgoTransaction]] or [[IndexedErgoBox]]
    * @return
    */
  private def getFromSegments[T](history: ErgoHistoryReader,
                                 treeHash: ModifierId,
                                 offset: Int,
                                 limit: Int,
                                 segmentCount: Int,
                                 array: ArrayBuffer[Long],
                                 idOf: (ModifierId, Int) => ModifierId,
                                 arraySelector: IndexedErgoAddress => ArrayBuffer[Long],
                                 retreive: (ArrayBuffer[Long], ErgoHistoryReader) => Array[T]): Array[T] = {
    if (offset + limit > array.length && segmentCount > 0) {
      val range: Array[Int] = getSegmentsForRange(offset, limit)
      val data: ArrayBuffer[Long] = ArrayBuffer.empty[Long]
      cfor(0)(_ < range.length, _ + 1) { i =>
        arraySelector(
          history.typedExtraIndexById[IndexedErgoAddress](idOf(treeHash, segmentCount - range(i))).get
        ) ++=: data
      }
      retreive(slice(data ++= (if (offset < array.length) array else Nil), offset % segmentTreshold, limit), history)
    } else
      retreive(slice(array, offset, limit), history)
  }
}
