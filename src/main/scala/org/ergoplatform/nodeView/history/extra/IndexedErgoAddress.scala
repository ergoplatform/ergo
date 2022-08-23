package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.ErgoAddress
import org.ergoplatform.ErgoAddressEncoder.{ChecksumLength, hash256}
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.history.extra.IndexedErgoAddress.{getSegmentsForRange, segmentTreshold, slice}
import org.ergoplatform.nodeView.history.extra.IndexedErgoAddressSerializer.segmentId
import org.ergoplatform.settings.Algos
import scorex.core.ModifierTypeId
import scorex.core.serialization.ScorexSerializer
import scorex.util.{ModifierId, bytesToId, idToBytes}
import scorex.util.serialization.{Reader, Writer}

import scala.collection.mutable.ListBuffer
import spire.syntax.all.cfor

case class IndexedErgoAddress(addressHash: ModifierId,
                              txs: ListBuffer[Long],
                              boxes: ListBuffer[Long]) extends BlockSection {

  override val sizeOpt: Option[Int] = None
  override def serializedId: Array[Byte] = idToBytes(addressHash)
  override def parentId: ModifierId = null
  override val modifierTypeId: ModifierTypeId = IndexedErgoAddress.modifierTypeId
  override type M = IndexedErgoAddress
  override def serializer: ScorexSerializer[IndexedErgoAddress] = IndexedErgoAddressSerializer

  private[extra] var segmentCount: Int = 0

  def txCount(): Long = segmentTreshold * segmentCount + txs.length
  def boxCount(): Long = segmentTreshold * segmentCount + boxes.length

  def retrieveTxs(history: ErgoHistoryReader, offset: Long, limit: Long): Array[IndexedErgoTransaction] =
    if(offset > txs.length) {
      val range: (Int, Int) = getSegmentsForRange(offset, limit)
      val data: ListBuffer[IndexedErgoTransaction] =
        history.typedModifierById[IndexedErgoAddress](segmentId(addressHash, segmentCount - range._1)).get.txs.map(n => NumericTxIndex.getTxByNumber(history, n).get)
      if(range._2 > 0) data ++=
        history.typedModifierById[IndexedErgoAddress](segmentId(addressHash, segmentCount - range._2)).get.txs.map(n => NumericTxIndex.getTxByNumber(history, n).get)
      data.toArray
    } else {
      slice(txs, offset, limit).map(n => NumericTxIndex.getTxByNumber(history, n).get).toArray
    }

  def retrieveBoxes(history: ErgoHistoryReader, offset: Long, limit: Long): Array[IndexedErgoBox] =
    if(offset > boxes.length) {
      val range: (Int, Int) = getSegmentsForRange(offset, limit)
      val data: ListBuffer[IndexedErgoBox] =
        history.typedModifierById[IndexedErgoAddress](segmentId(addressHash, segmentCount - range._1)).get.boxes.map(n => NumericBoxIndex.getBoxByNumber(history, n).get)
      if(range._2 > 0) data ++=
        history.typedModifierById[IndexedErgoAddress](segmentId(addressHash, segmentCount - range._2)).get.boxes.map(n => NumericBoxIndex.getBoxByNumber(history, n).get)
      data.toArray
    } else {
      slice(boxes, offset, limit).map(n => NumericBoxIndex.getBoxByNumber(history, n).get).toArray
    }

  def retrieveUtxos(history: ErgoHistoryReader, offset: Long, limit: Long): Array[IndexedErgoBox] = {
    val data: ListBuffer[IndexedErgoBox] = ListBuffer.empty[IndexedErgoBox]
    data ++= boxes.map(n => NumericBoxIndex.getBoxByNumber(history, n).get).filter(!_.trackedBox.isSpent)
    var segment = segmentCount
    while(data.length < limit && segment > 0) {
      segment -= 1
      data ++=: history.typedModifierById[IndexedErgoAddress](segmentId(addressHash, segment)).get.boxes
        .map(n => NumericBoxIndex.getBoxByNumber(history, n).get).filter(!_.trackedBox.isSpent)
    }
    slice(data, offset, limit).toArray
  }

  def addTx(tx: Long): IndexedErgoAddress = {
    cfor(txs.length - 1)(_ >= 0, _ - 1) { i => if(txs(i) == tx) return this} // check for duplicates
    txs += tx
    this
  }

  def addBox(box: Long): IndexedErgoAddress = {
    boxes += box
    this
  }

  def splitToSegment(): IndexedErgoAddress = {
    require(segmentTreshold < txs.length && segmentTreshold < boxes.length, "address does not have enough transactions or boxes for segmentation")
    val iEa: IndexedErgoAddress = new IndexedErgoAddress(segmentId(addressHash, segmentCount), txs.take(segmentTreshold), boxes.take(segmentTreshold))
    segmentCount += 1
    txs.remove(0, segmentTreshold)
    boxes.remove(0, segmentTreshold)
    iEa
  }
}

object IndexedErgoAddressSerializer extends ScorexSerializer[IndexedErgoAddress] {

  def addressToBytes(address: ErgoAddress): Array[Byte] = {
    val withNetworkByte = (ExtraIndexerRef.getAddressEncoder.networkPrefix + address.addressTypePrefix).toByte +: address.contentBytes
    withNetworkByte ++ hash256(withNetworkByte).take(ChecksumLength)
  }

  def hashAddress(address: ErgoAddress): Array[Byte] = Algos.hash(addressToBytes(address))

  def addressToModifierId(address: ErgoAddress): ModifierId = bytesToId(hashAddress(address))

  def segmentId(addressHash: ModifierId, segmentNum: Int): ModifierId = bytesToId(Algos.hash(addressHash + " segment " + segmentNum))

  override def serialize(iEa: IndexedErgoAddress, w: Writer): Unit = {
    w.putBytes(idToBytes(iEa.addressHash))
    w.putUInt(iEa.txs.length)
    cfor(0)(_ < iEa.txs.length, _ + 1) { i => w.putLong(iEa.txs(i))}
    w.putUInt(iEa.boxes.length)
    cfor(0)(_ < iEa.boxes.length, _ + 1) { i => w.putLong(iEa.boxes(i))}
    w.putInt(iEa.segmentCount)
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
    iEa.segmentCount = r.getInt()
    iEa
  }
}

object IndexedErgoAddress {

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ 15.toByte

  val segmentTreshold: Int = 8192

  def getSegmentsForRange(offset: Long, limit: Long): (Int, Int) = {
    require(limit <= segmentTreshold, "limit exceeds segmentTreshold")
    val x: Int = math.ceil(offset / segmentTreshold).toInt
    (x, if(offset % segmentTreshold >= limit) -1 else x - 1)
  }

  def slice[T](arr: Iterable[T], offset: Long, limit: Long): Iterable[T] =
    arr.slice((arr.size - offset - limit).toInt, (arr.size - offset + 1).toInt)

}
