package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.http.api.SortDirection.{ASC, DESC, Direction}
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.fastIdToBytes
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.nodeView.history.extra.SegmentSerializer._
import org.ergoplatform.sdk.JavaHelpers.Algos
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, ScorexLogging, bytesToId}
import spire.implicits.cfor

import scala.collection.mutable.ArrayBuffer
import java.lang.Math.abs
import scala.reflect.ClassTag

abstract class Segment[T <: Segment[_] : ClassTag](parentId: ModifierId, factory: ModifierId => T) extends ExtraIndex with ScorexLogging {

  override lazy val id: ModifierId = parentId
  override def serializedId: Array[Byte] = fastIdToBytes(parentId)

  /**
   * Internal segment buffer
   */
  private[extra] val segments: ArrayBuffer[T] = ArrayBuffer.empty[T]

  val txs: ArrayBuffer[Long] = ArrayBuffer.empty[Long]
  val boxes: ArrayBuffer[Long] = ArrayBuffer.empty[Long]

  private[extra] var boxSegmentCount: Int = 0
  private[extra] var txSegmentCount: Int = 0

  /**
   * @return total number of boxes associated with this address
   */
  def boxCount(implicit segmentTreshold: Int): Long = segmentTreshold * boxSegmentCount + boxes.length

  /**
   * @return total number of transactions associated with this address
   */
  def txCount(implicit segmentTreshold: Int): Long = segmentTreshold * txSegmentCount + txs.length

  /**
   * Retrieve segment with specified id from buffer or database
   *
   * @param history - database handle to search, if segment is not found in buffer
   * @param searchId- address segment to search for
   * @return
   */
  protected def getSegmentFromBufferOrHistroy(history: ErgoHistoryReader, searchId: ModifierId): Int = {
    cfor(segments.length - 1)(_ >= 0, _ - 1) { i =>
      if(segments(i).id.equals(searchId)) return i
    }
    segments += history.typedExtraIndexById[T](id).get
    segments.length - 1
  }

  /**
   * Locate which segment the given box number is in and change its sign, meaning it spends unspent boxes and vice versa.
   *
   * @param boxNum  - box number to locate
   * @param history - database to retrieve segments from
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
      while (low <= high) {
        val mid = (low + high) >>> 1
        n = getSegmentFromBufferOrHistroy(history, boxSegmentId(id, mid))
        if (abs(segments(n).boxes.head) < boxNumAbs &&
          abs(segments(n).boxes.last) < boxNumAbs)
          low = mid + 1
        else if (abs(segments(n).boxes.head) > boxNumAbs &&
          abs(segments(n).boxes.last) > boxNumAbs)
          high = mid - 1
        else
          low = high + 1 // break
      }
      val i: Int = binarySearch(segments(n).boxes, boxNumAbs)
      if (i >= 0)
        segments(n).boxes(i) = -segments(n).boxes(i)
      else
        log.warn(s"Box $boxNum not found in any segment of parent address when trying to spend")
    }
  }

  /**
   * Create an array addresses each containing a "segmentTreshold" number of this address's transaction and box indexes.
   * These special addresses have their ids calculated by "txSegmentId" and "boxSegmentId" respectively.
   *
   * @return array of addresses
   */
  private[extra] def splitToSegments(implicit segmentTreshold: Int): Array[T] = {
    val data: Array[T] = new Array[T]((txs.length / segmentTreshold) + (boxes.length / segmentTreshold))
    var i: Int = 0

    // Split txs until under segmentTreshold
    while(txs.length >= segmentTreshold) {
      data(i) = factory(txSegmentId(id, txSegmentCount))
      data(i).txs ++= txs.take(segmentTreshold)
      i += 1
      txSegmentCount += 1
      txs.remove(0, segmentTreshold)
    }

    // Split boxes until under segmentTreshold
    while(boxes.length >= segmentTreshold) {
      data(i) = factory(boxSegmentId(id, boxSegmentCount))
      data(i).boxes ++= boxes.take(segmentTreshold)
      i += 1
      boxSegmentCount += 1
      boxes.remove(0, segmentTreshold)
    }
    data
  }

  /**
   * Calculate the segment offsets for the given range.
   *
   * @param offset - items to skip from the start
   * @param limit  - items to retrieve
   * @return array of offsets
   */
  private def getSegmentsForRange(offset: Int, limit: Int)(implicit segmentTreshold: Int): Array[Int] =
    (math.max(math.floor(offset * 1F / segmentTreshold).toInt, 1) to math.ceil((offset + limit) * 1F / segmentTreshold).toInt).toArray

  /**
   * Get a range of elements from an ArrayBuffer by removing the last "offset" elements,
   * then getting the last "limit" elements reversed.
   *
   * @param arr    - array to get range from
   * @param offset - number of items to skip from the end
   * @param limit  - number of items to retrieve
   * @return a reversed range in "arr" ArrayBuffer
   */
  private def sliceReversed(arr: ArrayBuffer[Long], offset: Int, limit: Int): ArrayBuffer[Long] =
    arr.slice(arr.length - limit - offset, arr.length - offset).reverse

  /**
   * Get an array of transactions with full bodies from an array of numeric transaction indexes
   *
   * @param arr     - array of numeric transaction indexes to retrieve
   * @param history - database handle
   * @return array of transactions with full bodies
   */
  private def getTxs(arr: ArrayBuffer[Long], history: ErgoHistoryReader): Array[IndexedErgoTransaction] = // sorted to match explorer
    arr.map(n => NumericTxIndex.getTxByNumber(history, n).get.retrieveBody(history)).toArray.sortBy(tx => (-tx.height, tx.id))

  /**
   * Get an array of boxes from an array of numeric box indexes
   *
   * @param arr     - array of numeric box indexes to retrieve
   * @param history - database handle
   * @return array of boxes
   */
  private def getBoxes(arr: ArrayBuffer[Long], history: ErgoHistoryReader): Array[IndexedErgoBox] =
    arr.map(n => NumericBoxIndex.getBoxByNumber(history, n).get).toArray

  /**
   * Get a set of address segments from database containing numeric transaction or box indexes. Then actually retreive these indexes.
   *
   * @param history       - database handle
   * @param offset        - number of items to skip from the start
   * @param limit         - max number of item to be returned
   * @param segmentCount  - number of segments of the parent address
   * @param array         - the indexes already in memory
   * @param idOf          - function to calculate segment ids, either [[txSegmentId]] or [[boxSegmentId]]
   * @param arraySelector - function to select index array from retreived segments
   * @param retreive      - function to retreive indexes from database
   * @tparam B - type of desired indexes, either [[IndexedErgoTransaction]] or [[IndexedErgoBox]]
   * @return
   */
  private def getFromSegments[B: ClassTag](history: ErgoHistoryReader,
                                           offset: Int,
                                           limit: Int,
                                           segmentCount: Int,
                                           array: ArrayBuffer[Long],
                                           idOf: (ModifierId, Int) => ModifierId,
                                           arraySelector: T => ArrayBuffer[Long],
                                           retreive: (ArrayBuffer[Long], ErgoHistoryReader) => Array[B])
                                          (implicit segmentTreshold: Int): Array[B] = {
    val total: Int = segmentTreshold * segmentCount + array.length
    if(offset >= total)
      return Array.empty[B] // return empty array if all elements are skipped
    if(offset + limit > array.length && segmentCount > 0) {
      val data: ArrayBuffer[Long] = ArrayBuffer.empty[Long]
      getSegmentsForRange(offset, limit).map(n => math.max(segmentCount - n, 0)).distinct.foreach { num =>
        arraySelector(
          history.typedExtraIndexById[T](idOf(id, num)).get
        ) ++=: data
      }
      data ++= (if(offset < array.length) array else Nil)
      retreive(sliceReversed(data, offset % segmentTreshold, math.min(total - offset, limit)), history)
    } else
      retreive(sliceReversed(array, offset, limit), history)
  }

  /**
   * Get a range of the transactions associated with this address
   *
   * @param history - history to use
   * @param offset  - items to skip from the start
   * @param limit   - items to retrieve
   * @return array of transactions with full bodies
   */
  def retrieveTxs(history: ErgoHistoryReader, offset: Int, limit: Int)(implicit segmentTreshold: Int): Array[IndexedErgoTransaction] =
    getFromSegments(history, offset, limit, txSegmentCount, txs, txSegmentId, _.txs, getTxs)

  /**
   * Get a range of the boxes associated with this address
   *
   * @param history - history to use
   * @param offset  - items to skip from the start
   * @param limit   - items to retrieve
   * @return array of boxes
   */
  def retrieveBoxes(history: ErgoHistoryReader, offset: Int, limit: Int)(implicit segmentTreshold: Int): Array[IndexedErgoBox] =
    getFromSegments(history, offset, limit, boxSegmentCount, boxes, boxSegmentId, _.boxes, getBoxes)

  /**
   * Get a range of the boxes associated with this address that are NOT spent
   *
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
        while(data.length < (limit + offset) && segment > 0) {
          segment -= 1
          history.typedExtraIndexById[T](boxSegmentId(id, segment)).get.boxes
            .filter(_ > 0).map(n => NumericBoxIndex.getBoxByNumber(history, n).get) ++=: data
        }
        data.reverse.slice(offset, offset + limit).toArray
      case ASC =>
        var segment: Int = 0
        while(data.length < (limit + offset) && segment < boxSegmentCount) {
          data ++= history.typedExtraIndexById[T](boxSegmentId(id, segment)).get.boxes
            .filter(_ > 0).map(n => NumericBoxIndex.getBoxByNumber(history, n).get)
          segment += 1
        }
        if (data.length < (limit + offset))
          data ++= boxes.filter(_ > 0).map(n => NumericBoxIndex.getBoxByNumber(history, n).get)
        data.slice(offset, offset + limit).toArray
    }
  }

  /**
   * Rollback the state of segments in memory and in db
   *
   * @param txTarget  - remove transaction numbers above this number
   * @param boxTarget - remove box numbers above this number and revert the balance
   * @param _history  - history handle to update segment in database
   * @return modifier ids to remove
   */
  private[extra] def rollback(txTarget: Long, boxTarget: Long, _history: ErgoHistory)(implicit segmentTreshold: Int): Array[ModifierId] = {

    if((txCount == 0 && boxCount == 0) || // already rolled back
      (txs.last <= txTarget && abs(boxes.last) <= boxTarget)) // no rollback needed
      return Array.empty[ModifierId]

    def history: ErgoHistoryReader = _history.getReader

    val toSave: ArrayBuffer[ExtraIndex] = ArrayBuffer.empty[ExtraIndex]
    val toRemove: ArrayBuffer[ModifierId] = ArrayBuffer.empty[ModifierId]

    // filter tx numbers
    do {
      val tmp = txs.takeWhile(_ <= txTarget)
      txs.clear()
      txs ++= tmp
      if(txs.isEmpty && txSegmentCount > 0) { // entire current tx set removed, retrieving more from database if possible
        val id = txSegmentId(parentId, txSegmentCount - 1)
        txs ++= history.typedExtraIndexById[T](id).get.txs
        toRemove += id
        txSegmentCount -= 1
      }
    } while (txCount > 0 && txs.last > txTarget)

    // filter box numbers
    do {
      val tmp = boxes.takeWhile(abs(_) <= boxTarget)
      boxes.clear()
      boxes ++= tmp
      if(boxes.isEmpty && boxSegmentCount > 0) { // entire current box set removed, retrieving more from database if possible
        val id = boxSegmentId(parentId, boxSegmentCount - 1)
        boxes ++= history.typedExtraIndexById[T](id).get.boxes
        toRemove += id
        boxSegmentCount -= 1
      }
    } while (boxCount > 0 && abs(boxes.last) > boxTarget)

    if (txCount == 0 && boxCount == 0)
      toRemove += this.id // all segments empty after rollback, delete
    else // TODO this will not work
      toSave += this // save the changes made to this address

    // Save changes
    _history.historyStorage.insertExtra(Array.empty, toSave.toArray)

    toRemove.toArray
  }



  /**
   * Add transaction index
   *
   * @param tx - numeric transaction index
   * @return this
   */
  private[extra] def addTx(tx: Long): T

  /**
   * Add box index
   *
   * @param iEb    - box to use
   * @param record - whether to add box to boxes list, used in rollbacks (true by default)
   * @return this
   */
  private[extra] def addBox(iEb: IndexedErgoBox, record: Boolean = true): T

  /**
   * Update segments in memory or in database by spending a box
   *
   * @param iEb        - box to spend
   * @param historyOpt - history handle to update segment in db if spent box is old
   * @return this
   */
  private[extra] def spendBox(iEb: IndexedErgoBox, historyOpt: Option[ErgoHistoryReader] = None)(implicit ae: ErgoAddressEncoder): T

}

object SegmentSerializer {

  /**
   * Calculates the id of a segment containing box indexes.
   *
   * @param parentId   - parent class identifier
   * @param segmentNum - numberic identifier of the segment
   * @return calculated ModifierId
   */
  def boxSegmentId(parentId: ModifierId, segmentNum: Int): ModifierId = bytesToId(Algos.hash(parentId + " box segment " + segmentNum))

  /**
   * Calculates the id of a segment containing transaction indexes.
   *
   * @param parentId   - parent class identifier
   * @param segmentNum - numberic identifier of the segment
   * @return calculated ModifierId
   */
  def txSegmentId(parentId: ModifierId, segmentNum: Int): ModifierId = bytesToId(Algos.hash(parentId + " tx segment " + segmentNum))

  /**
   * Copied from [[java.util.Arrays.binarySearch]]
   */
  private[extra] def binarySearch(a: ArrayBuffer[Long], key: Long): Int = {
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

  def serialize(s: Segment[_], w: Writer): Unit = {
    w.putUInt(s.txs.length)
    cfor(0)(_ < s.txs.length, _ + 1) { i => w.putLong(s.txs(i)) }
    w.putUInt(s.boxes.length)
    cfor(0)(_ < s.boxes.length, _ + 1) { i => w.putLong(s.boxes(i)) }
    w.putInt(s.boxSegmentCount)
    w.putInt(s.txSegmentCount)
  }

  def parse(r: Reader, s: Segment[_]): Unit = {
    val txnsLen: Long = r.getUInt()
    cfor(0)(_ < txnsLen, _ + 1) { _ => s.txs += r.getLong() }
    val boxesLen: Long = r.getUInt()
    cfor(0)(_ < boxesLen, _ + 1) { _ => s.boxes += r.getLong() }
    s.boxSegmentCount = r.getInt()
    s.txSegmentCount = r.getInt()
  }

}
