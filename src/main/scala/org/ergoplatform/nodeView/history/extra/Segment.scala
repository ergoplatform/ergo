package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.{ErgoAddressEncoder, ErgoBox}
import org.ergoplatform.http.api.SortDirection.{ASC, DESC, Direction}
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.fastIdToBytes
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.nodeView.history.extra.SegmentSerializer._
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.sdk.JavaHelpers.Algos
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, ScorexLogging, bytesToId}
import spire.implicits.cfor

import scala.collection.mutable.ArrayBuffer
import java.lang.Math.abs
import scala.collection.mutable
import scala.reflect.ClassTag

/**
 * Class to manage the tracking of transactions/boxes in relation to some other object (ErgoTree/token).
 * When [[ExtraIndexerBase.segmentThreshold]] number of transaction/box indexes are accumulated, new instances of the parent object are created to contain them.
 * This mechanism is used to prevent excessive serialization/deserialization delays caused by objects with a lot of transaction/box indexes.
 * @param parentId - identifier of parent object
 * @param factory  - parent object factory
 * @param txs      - list of numeric transaction indexes
 * @param boxes    - list of numeric box indexes, negative values indicate the box is spent
 * @param idMod    - function to apply to ids during segmentation and db lookup
 * @tparam T       - type of parent object
 */
abstract class Segment[T <: Segment[_] : ClassTag](val parentId: ModifierId,
                                                   val factory: ModifierId => T,
                                                   val txs: ArrayBuffer[Long],
                                                   val boxes: ArrayBuffer[Long],
                                                   val idMod: ModifierId => ModifierId = id => id)
  extends ExtraIndex with ScorexLogging {

  override lazy val id: ModifierId = parentId
  override def serializedId: Array[Byte] = fastIdToBytes(parentId)

  /**
   * Internal segment buffer
   */
  private[extra] val buffer: mutable.HashMap[ModifierId,T] = new mutable.HashMap[ModifierId,T]

  /**
   * Number of segments in database containing box numbers
   */
  private[extra] var boxSegmentCount: Int = 0

  /**
   * Number of segments in database containing transaction numbers
   */
  private[extra] var txSegmentCount: Int = 0

  /**
   * @return total number of boxes associated with the parent object
   */
  def boxCount(implicit segmentThreshold: Int): Long = segmentThreshold * boxSegmentCount + boxes.length

  /**
   * @return total number of transactions associated with the parent object
   */
  def txCount(implicit segmentThreshold: Int): Long = segmentThreshold * txSegmentCount + txs.length

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
      boxes.update(inCurrent, -boxes(inCurrent))
    } else { // box is in another segment, use binary search to locate
      var segmentId: ModifierId = ModifierId @@ ""
      var low = 0
      var high = boxSegmentCount - 1
      while(low <= high) {
        val mid = (low + high) >>> 1
        segmentId = boxSegmentId(parentId, mid)
        buffer.get(segmentId).orElse(history.typedExtraIndexById[T](idMod(segmentId))).foreach { segment =>
          if(abs(segment.boxes.head) < boxNumAbs &&
            abs(segment.boxes.last) < boxNumAbs)
            low = mid + 1
          else if(abs(segment.boxes.head) > boxNumAbs &&
            abs(segment.boxes.last) > boxNumAbs)
            high = mid - 1
          else {
            low = high + 1 // break
            buffer.put(segmentId, segment)
          }
        }
      }
      buffer.get(segmentId) match {
        case Some(segment) =>
          val i: Int = binarySearch(segment.boxes, boxNumAbs)
          if (i >= 0) {
            segment.boxes(i) = -segment.boxes(i)
          } else {
            log.error(s"Box $boxNum not found in predicted segment of parent: ${segment.boxes.mkString("[", ",", "]")}")
          }
        case None =>
          log.error(s"Box $boxNum not found in any segment of parent")
      }
    }
  }

  /**
   * Create an array of parent objects each containing [[ExtraIndexerBase.segmentThreshold]] number of transaction/box indexes.
   * These objects have their ids calculated by "txSegmentId" and "boxSegmentId" respectively.
   *
   * @return array of parent objects
   */
  private[extra] def splitToSegments(implicit segmentTreshold: Int): Array[T] = {
    val data: ArrayBuffer[T] = new ArrayBuffer[T]

    // Split txs until under segmentTreshold
    while(txs.length > segmentTreshold) {
      data += factory(txSegmentId(parentId, txSegmentCount))
      data.last.txs ++= txs.take(segmentTreshold)
      txSegmentCount += 1
      txs.remove(0, segmentTreshold)
    }

    // Split boxes until under segmentTreshold
    while(boxes.length > segmentTreshold) {
      data += factory(boxSegmentId(parentId, boxSegmentCount))
      data.last.boxes ++= boxes.take(segmentTreshold)
      boxSegmentCount += 1
      boxes.remove(0, segmentTreshold)
    }
    data.toArray
  }

  /**
   * Calculate the segment offsets for the given range.
   *
   * @param offset - items to skip from the start
   * @param limit  - items to retrieve
   * @return array of offsets
   */
  private[extra] def getSegmentsForRange(offset: Int, limit: Int)(implicit segmentTreshold: Int): Array[Int] = {
    val floor = math.max(math.floor(offset * 1F / segmentTreshold).toInt, 0)
    val ceil = math.ceil((offset + limit) * 1F / segmentTreshold).toInt
    (floor to ceil).toArray
  }

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
   * Get a set of segments from database containing numeric transaction or box indexes. Then actually retreive these indexes.
   *
   * @param history       - database handle
   * @param offset        - number of items to skip from the start
   * @param limit         - max number of item to be returned
   * @param segmentCount  - number of segments of the parent address
   * @param array         - the indexes already in memory
   * @param idOf          - function to calculate segment ids, either [[txSegmentId]] or [[boxSegmentId]]
   * @param arraySelector - function to select index array from retreived segments
   * @param retrieve      - function to retrieve indexes from database
   * @tparam B - type of desired indexes, either [[IndexedErgoTransaction]] or [[IndexedErgoBox]]
   * @return
   */
  private[extra] def getFromSegments[B: ClassTag](history: ErgoHistoryReader,
                                                  offset: Int,
                                                  limit: Int,
                                                  segmentCount: Int,
                                                  array: ArrayBuffer[Long],
                                                  idOf: (ModifierId, Int) => ModifierId,
                                                  arraySelector: T => ArrayBuffer[Long],
                                                  retrieve: (ArrayBuffer[Long], ErgoHistoryReader) => Array[B])
                                                 (implicit segmentTreshold: Int): Array[B] = {
    val total: Int = segmentTreshold * segmentCount + array.length
    if (offset >= total)
      return Array.empty[B] // return empty array if all elements are skipped
    if (offset + limit > array.length && segmentCount > 0) {

      val target = offset + limit

      val altData: ArrayBuffer[Long] = ArrayBuffer.empty[Long]
      altData ++= (if (offset < array.length) array.slice(offset, Math.min(offset + limit, array.length)) else Nil)
      val segments = getSegmentsForRange(offset - array.length, limit).map(n => math.min(segmentCount - 1, n)).distinct
      println("0: " + history.typedExtraIndexById[T](idMod(idOf(parentId, 0))).isDefined)
      println("1: " + history.typedExtraIndexById[T](idMod(idOf(parentId, 1))).isDefined)
      println("2: " + history.typedExtraIndexById[T](idMod(idOf(parentId, 2))).isDefined)
      println("3: " + history.typedExtraIndexById[T](idMod(idOf(parentId, 3))).isDefined)
      println("segments: " + segments.mkString(", "))
      segments.foreach { num =>
        val lowerBound = array.length + num * segmentTreshold
        val upperBound = lowerBound + segmentTreshold

        if (altData.length < limit && target > lowerBound) {
          val arr = arraySelector(
            history.typedExtraIndexById[T](idMod(idOf(parentId, num))).get
          ).reverse
          if (target > upperBound) {
            altData ++= arr.slice(offset - lowerBound, arr.size)
          } else {
            if (offset > lowerBound) {
              altData ++= arr.slice(offset - lowerBound, offset - lowerBound + limit)
            } else {
              altData ++= arr.slice(0, target - lowerBound)
            }
          }
        }
      }

      retrieve(altData, history)
    } else {
      retrieve(array.slice(offset, offset + limit), history)
    }
  }

  /**
   * Get a range of the transactions associated with the parent object
   *
   * @param history - history to use
   * @param offset  - items to skip from the start
   * @param limit   - items to retrieve
   * @return array of transactions with full bodies
   */
  def retrieveTxs(history: ErgoHistoryReader, offset: Int, limit: Int)(implicit segmentTreshold: Int): Array[IndexedErgoTransaction] =
    getFromSegments(history, offset, limit, txSegmentCount, txs, txSegmentId, _.txs, getTxs)

  /**
   * Get a range of the boxes associated with the parent object
   *
   * @param history - history to use
   * @param offset  - items to skip from the start
   * @param limit   - items to retrieve
   * @return array of boxes
   */
  def retrieveBoxes(history: ErgoHistoryReader, offset: Int, limit: Int)(implicit segmentTreshold: Int): Array[IndexedErgoBox] =
    getFromSegments(history, offset, limit, boxSegmentCount, boxes, boxSegmentId, _.boxes, getBoxes)

  /**
   * Get a range of the boxes associated with the parent that are NOT spent
   *
   * @param history     - history to use
   * @param mempool     - mempool to use, if unconfirmed is true
   * @param offset      - items to skip from the start
   * @param limit       - items to retrieve
   * @param sortDir     - whether to start retrieval from newest box (DESC) or oldest box (ASC)
   * @param unconfirmed - whether to include unconfirmed boxes
   * @param retrieve    - function to retrieve indexes from database
   * @param memMap      - function to transform mempool boxes
   * @return array of unspent boxes
   */
  private[extra] def retrieveUtxos[B: ClassTag]
                 (history: ErgoHistoryReader,
                  mempool: ErgoMemPoolReader,
                  offset: Int,
                  limit: Int,
                  sortDir: Direction,
                  unconfirmed: Boolean,
                  retrieve: (ArrayBuffer[Long], ErgoHistoryReader) => Array[B],
                  memMap: ErgoBox => B): Seq[B] = {
    val data: ArrayBuffer[B] = ArrayBuffer.empty[B]
    val confirmedBoxes: Seq[B] = sortDir match {
      case DESC =>
        data ++= retrieve(boxes.filter(_ > 0), history)
        var segment: Int = boxSegmentCount
        while (data.length < (limit + offset) && segment > 0) {
          segment -= 1
          retrieve(history.typedExtraIndexById[T](idMod(boxSegmentId(parentId, segment))).get.boxes.filter(_ > 0), history) ++=: data
        }
        data.reverse.slice(offset, offset + limit)
      case ASC =>
        var segment: Int = 0
        while (data.length < (limit + offset) && segment < boxSegmentCount) {
          data ++= retrieve(history.typedExtraIndexById[T](idMod(boxSegmentId(parentId, segment))).get.boxes.filter(_ > 0), history)
          segment += 1
        }
        if (data.length < (limit + offset))
          data ++= retrieve(boxes.filter(_ > 0), history)
        data.slice(offset, offset + limit)
    }
    if (unconfirmed) {
      val unconfirmedBoxes = filterMempool(mempool.getAll.flatMap(_.transaction.outputs)).map(memMap)
      sortDir match {
        case DESC => unconfirmedBoxes ++ confirmedBoxes
        case ASC => confirmedBoxes ++ unconfirmedBoxes
      }
    } else
      confirmedBoxes
  }

  /**
   * Get a range of the boxes associated with the parent that are NOT spent
   *
   * @param history     - history to use
   * @param mempool     - mempool to use, if unconfirmed is true
   * @param offset      - items to skip from the start
   * @param limit       - items to retrieve
   * @param sortDir     - whether to start retrieval from newest box (DESC) or oldest box (ASC)
   * @param unconfirmed - whether to include unconfirmed boxes
   * @return array of unspent boxes
   */
  def retrieveUtxos(history: ErgoHistoryReader,
                    mempool: ErgoMemPoolReader,
                    offset: Int,
                    limit: Int,
                    sortDir: Direction,
                    unconfirmed: Boolean): Seq[IndexedErgoBox] =
    retrieveUtxos(history, mempool, offset, limit, sortDir, unconfirmed, getBoxes, box => new IndexedErgoBox(0, None, None, box, 0))

  /**
   * Logic for [[Segment.rollback]]
   *
   * @param txTarget  - remove transaction numbers above this number
   * @param boxTarget - remove box numbers above this number
   * @param history   - history handle to update segment(s) in database
   * @return modifier ids to remove
   */
  protected def rollbackState(txTarget: Long, boxTarget: Long, history: ErgoHistoryReader)
                             (implicit segmentTreshold: Int): ArrayBuffer[ModifierId] = {

    if((txCount == 0 && boxCount == 0) || // already rolled back
      (txs.lastOption.getOrElse[Long](0) <= txTarget &&
        abs(boxes.lastOption.getOrElse[Long](0)) <= boxTarget)) // no rollback needed
      return ArrayBuffer.empty[ModifierId]

    val toRemove: ArrayBuffer[ModifierId] = ArrayBuffer.empty[ModifierId]

    // filter tx numbers
    do {
      val tmp = txs.takeWhile(_ <= txTarget)
      txs.clear()
      txs ++= tmp
      if (txs.isEmpty && txSegmentCount > 0) { // entire current tx set removed, retrieving more from database if possible
        val segmentId = idMod(txSegmentId(parentId, txSegmentCount - 1))
        txs ++= history.typedExtraIndexById[T](segmentId).get.txs
        toRemove += segmentId
        txSegmentCount -= 1
      }
    } while (txCount > 0 && txs.last > txTarget)

    // filter box numbers
    do {
      val tmp = boxes.takeWhile(abs(_) <= boxTarget)
      boxes.clear()
      boxes ++= tmp
      if (boxes.isEmpty && boxSegmentCount > 0) { // entire current box set removed, retrieving more from database if possible
        val segmentId = idMod(boxSegmentId(parentId, boxSegmentCount - 1))
        boxes ++= history.typedExtraIndexById[T](segmentId).get.boxes
        toRemove += segmentId
        boxSegmentCount -= 1
      }
    } while (boxCount > 0 && abs(boxes.last) > boxTarget)

    toRemove
  }

  /**
   * Rollback the state of segments in memory and in db
   *
   * @param txTarget  - remove transaction numbers above this number
   * @param boxTarget - remove box numbers above this number
   * @param history  - history handle to update segment(s) in database
   * @return modifier ids to remove
   */
  private[extra] def rollback(txTarget: Long, boxTarget: Long, history: ErgoHistory)(implicit segmentTreshold: Int): Array[ModifierId]

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

  /**
   * Filter mempool boxes if API call requires it
   *
   * @param boxes - all boxes in mempool
   * @return associated boxes
   */
  private[extra] def filterMempool(boxes: Seq[ErgoBox]): Seq[ErgoBox]

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
    w.putInt(s.txs.length)
    cfor(0)(_ < s.txs.length, _ + 1) { i => w.putLong(s.txs(i)) }
    w.putInt(s.boxes.length)
    cfor(0)(_ < s.boxes.length, _ + 1) { i => w.putLong(s.boxes(i)) }
    w.putInt(s.boxSegmentCount)
    w.putInt(s.txSegmentCount)
  }

  def parse(r: Reader, s: Segment[_]): Unit = {
    val txnsLen: Int = r.getInt()
    s.txs.sizeHint(txnsLen)
    cfor(0)(_ < txnsLen, _ + 1) { _ => s.txs += r.getLong() }
    val boxesLen: Int = r.getInt()
    s.boxes.sizeHint(boxesLen)
    cfor(0)(_ < boxesLen, _ + 1) { _ => s.boxes += r.getLong() }
    s.boxSegmentCount = r.getInt()
    s.txSegmentCount = r.getInt()
  }

}
