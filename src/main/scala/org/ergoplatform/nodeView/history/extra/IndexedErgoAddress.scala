package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.ErgoAddress
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.history.extra.ExtraIndexerRef.fastIdToBytes
import org.ergoplatform.nodeView.history.extra.IndexedErgoAddress.{getSegmentsForRange, segmentTreshold, slice}
import org.ergoplatform.nodeView.history.extra.IndexedErgoAddressSerializer.{boxSegmentId, txSegmentId}
import org.ergoplatform.settings.Algos
import scorex.core.ModifierTypeId
import scorex.core.serialization.ScorexSerializer
import scorex.util.{ModifierId, ScorexLogging, bytesToId}
import scorex.util.serialization.{Reader, Writer}
import sigmastate.Values.ErgoTree

import scala.collection.mutable.ListBuffer
import spire.syntax.all.cfor

case class IndexedErgoAddress(treeHash: ModifierId,
                              txs: ListBuffer[Long],
                              boxes: ListBuffer[Long]) extends BlockSection with ScorexLogging {

  override val sizeOpt: Option[Int] = None
  override def serializedId: Array[Byte] = fastIdToBytes(treeHash)
  override def parentId: ModifierId = null
  override val modifierTypeId: ModifierTypeId = IndexedErgoAddress.modifierTypeId
  override type M = IndexedErgoAddress
  override def serializer: ScorexSerializer[IndexedErgoAddress] = IndexedErgoAddressSerializer

  private[extra] var boxSegmentCount: Int = 0
  private[extra] var txSegmentCount: Int = 0

  def txCount(): Long = segmentTreshold * txSegmentCount + txs.length
  def boxCount(): Long = segmentTreshold * boxSegmentCount + boxes.length

  def retrieveTxs(history: ErgoHistoryReader, offset: Int, limit: Int): Array[IndexedErgoTransaction] = {
    if (offset + limit > txs.length && txSegmentCount > 0) {
      val range: Array[Int] = getSegmentsForRange(offset, limit)
      cfor(0)(_ < range.length, _ + 1) { i =>
        txs ++=: (history.typedModifierById[IndexedErgoAddress](txSegmentId(treeHash, txSegmentCount - range(i))) match {
          case Some(iEa) => iEa.txs
          case None      => ListBuffer.empty[Long]
        })
      }
    }
    slice(txs, offset, limit).map(n => NumericTxIndex.getTxByNumber(history, n).get.retrieveBody(history)).toArray
  }

  def retrieveBoxes(history: ErgoHistoryReader, offset: Int, limit: Int): Array[IndexedErgoBox] = {
    if(offset + limit > boxes.length && boxSegmentCount > 0) {
      val range: Array[Int] = getSegmentsForRange(offset, limit)
      cfor(0)(_ < range.length, _ + 1) { i =>
        boxes ++=: (history.typedModifierById[IndexedErgoAddress](boxSegmentId(treeHash, boxSegmentCount - range(i))) match {
          case Some(iEb) => iEb.boxes
          case None      => ListBuffer.empty[Long]
        })
      }
    }
    slice(boxes, offset, limit).map(n => NumericBoxIndex.getBoxByNumber(history, n).get).toArray
  }

  def retrieveUtxos(history: ErgoHistoryReader, offset: Int, limit: Int): Array[IndexedErgoBox] = {
    val data: ListBuffer[IndexedErgoBox] = ListBuffer.empty[IndexedErgoBox]
    data ++= boxes.map(n => NumericBoxIndex.getBoxByNumber(history, n).get).filter(!_.trackedBox.isSpent)
    var segment = boxSegmentCount
    while(data.length < limit && segment > 0) {
      segment -= 1
      data ++=: history.typedModifierById[IndexedErgoAddress](boxSegmentId(treeHash, segment)).get.boxes
        .map(n => NumericBoxIndex.getBoxByNumber(history, n).get).filter(!_.trackedBox.isSpent)
    }
    slice(data, offset, limit).toArray
  }

  def addTx(tx: Long): IndexedErgoAddress = {
    if(txs.last != tx) txs += tx // check for duplicates
    this
  }

  def addBox(box: Long): IndexedErgoAddress = {
    boxes += box
    this
  }

  def splitToSegment(): Array[IndexedErgoAddress] = {
    require(segmentTreshold < txs.length || segmentTreshold < boxes.length, "address does not have enough transactions or boxes for segmentation")
    val data: ListBuffer[IndexedErgoAddress] = ListBuffer.empty[IndexedErgoAddress]
    if(segmentTreshold < txs.length) {
      data += new IndexedErgoAddress(txSegmentId(treeHash, txSegmentCount), txs.take(segmentTreshold), ListBuffer.empty[Long])
      txSegmentCount += 1
      txs.remove(0, segmentTreshold)
    }
    if(segmentTreshold < boxes.length) {
      data += new IndexedErgoAddress(boxSegmentId(treeHash, boxSegmentCount), ListBuffer.empty[Long], boxes.take(segmentTreshold))
      boxSegmentCount += 1
      boxes.remove(0, segmentTreshold)
    }
    data.toArray
  }
}

object IndexedErgoAddressSerializer extends ScorexSerializer[IndexedErgoAddress] {

  def hashErgoTree(tree: ErgoTree): Array[Byte] = Algos.hash(tree.bytes)
  def hashAddress(address: ErgoAddress): Array[Byte] = hashErgoTree(address.script)

  def boxSegmentId(hash: ModifierId, segmentNum: Int): ModifierId = bytesToId(Algos.hash(hash + " box segment " + segmentNum))
  def txSegmentId(hash: ModifierId, segmentNum: Int): ModifierId = bytesToId(Algos.hash(hash + " tx segment " + segmentNum))

  override def serialize(iEa: IndexedErgoAddress, w: Writer): Unit = {
    w.putBytes(fastIdToBytes(iEa.treeHash))
    w.putUInt(iEa.txs.length)
    cfor(0)(_ < iEa.txs.length, _ + 1) { i => w.putLong(iEa.txs(i))}
    w.putUInt(iEa.boxes.length)
    cfor(0)(_ < iEa.boxes.length, _ + 1) { i => w.putLong(iEa.boxes(i))}
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
    val iEa: IndexedErgoAddress = new IndexedErgoAddress(addressHash, txns, boxes)
    iEa.boxSegmentCount = r.getInt()
    iEa.txSegmentCount = r.getInt()
    iEa
  }
}

object IndexedErgoAddress {

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ 15.toByte

  val segmentTreshold: Int = 512

  def getSegmentsForRange(offset: Int, limit: Int): Array[Int] =
    (math.ceil((offset + limit) * 1F / segmentTreshold).toInt to math.ceil((offset + 1F) / segmentTreshold).toInt by -1).toArray.reverse

  def slice[T](arr: Iterable[T], offset: Int, limit: Int): Iterable[T] =
    arr.slice(arr.size - offset - limit, arr.size - offset)

}
