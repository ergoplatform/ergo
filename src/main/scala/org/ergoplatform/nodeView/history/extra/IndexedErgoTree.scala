package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.history.extra.ExtraIndexerRef.fastIdToBytes
import org.ergoplatform.nodeView.history.extra.IndexedErgoAddress.{getSegmentsForRange, segmentTreshold, slice}
import org.ergoplatform.nodeView.history.extra.IndexedErgoTreeSerializer.segmentId
import org.ergoplatform.settings.Algos
import scorex.core.ModifierTypeId
import scorex.core.serialization.ScorexSerializer
import scorex.util.{ModifierId, bytesToId}
import scorex.util.serialization.{Reader, Writer}
import sigmastate.Values.ErgoTree

import scala.collection.mutable.ListBuffer
import spire.syntax.all.cfor

case class IndexedErgoTree(treeHash: ModifierId, boxes: ListBuffer[Long]) extends BlockSection {
  override val sizeOpt: Option[Int] = None
  override def serializedId: Array[Byte] = fastIdToBytes(treeHash)
  override def parentId: ModifierId = null
  override val modifierTypeId: ModifierTypeId = IndexedErgoTree.modifierTypeId
  override type M = IndexedErgoTree
  override def serializer: ScorexSerializer[IndexedErgoTree] = IndexedErgoTreeSerializer

  private[extra] var segmentCount: Int = 0

  def boxCount(): Long = segmentTreshold * segmentCount + boxes.length

  def retrieveBoxes(history: ErgoHistoryReader, offset: Long, limit: Long): Array[IndexedErgoBox] = {
    if(offset > boxes.length) {
      val range: Array[Int] = getSegmentsForRange(offset, limit)
      cfor(0)(_ < range.length, _ + 1) { i =>
        boxes ++=: history.typedModifierById[IndexedErgoAddress](segmentId(treeHash, segmentCount - range(i))).get.boxes
      }
    }
    slice(boxes, offset, limit).map(n => NumericBoxIndex.getBoxByNumber(history, n).get).toArray
  }

  def retrieveUtxos(history: ErgoHistoryReader, offset: Long, limit: Long): Array[IndexedErgoBox] = {
    val data: ListBuffer[IndexedErgoBox] = ListBuffer.empty[IndexedErgoBox]
    data ++= boxes.map(n => NumericBoxIndex.getBoxByNumber(history, n).get).filter(!_.trackedBox.isSpent)
    var segment = segmentCount
    while(data.length < limit && segment > 0) {
      segment -= 1
      data ++=: history.typedModifierById[IndexedErgoAddress](segmentId(treeHash, segment)).get.boxes
        .map(n => NumericBoxIndex.getBoxByNumber(history, n).get).filter(!_.trackedBox.isSpent)
    }
    slice(data, offset, limit).toArray
  }


  def addBox(num: Long): IndexedErgoTree = {
    boxes += num
    this
  }

  def splitToSegment(): IndexedErgoTree = {
    require(segmentTreshold < boxes.length, "ergotree does not have enough boxes for segmentation")
    val iEt: IndexedErgoTree = IndexedErgoTree(segmentId(treeHash, segmentCount), boxes.take(segmentTreshold))
    segmentCount += 1
    boxes.remove(0, segmentTreshold)
    iEt
  }
}

object IndexedErgoTreeSerializer extends ScorexSerializer[IndexedErgoTree] {

  def ergoTreeHash(tree: ErgoTree): Array[Byte] = Algos.hash(tree.bytes)

  def segmentId(treeHash: ModifierId, segmentNum: Int): ModifierId = bytesToId(Algos.hash(treeHash + " segment " + segmentNum))

  override def serialize(iEt: IndexedErgoTree, w: Writer): Unit = {
    w.putBytes(iEt.serializedId)
    w.putLong(iEt.boxes.length)
    cfor(0)(_ < iEt.boxes.length, _ + 1) { i => w.putLong(iEt.boxes(i))}
    w.putInt(iEt.segmentCount)
  }

  override def parse(r: Reader): IndexedErgoTree = {
    val treeHash: ModifierId = bytesToId(r.getBytes(32))
    val boxesLen: Long = r.getLong()
    val boxes: ListBuffer[Long] = ListBuffer.empty[Long]
    cfor(0)(_ < boxesLen, _ + 1) { _ => boxes += r.getLong()}
    val iEt: IndexedErgoTree = IndexedErgoTree(treeHash, boxes)
    iEt.segmentCount = r.getInt()
    iEt
  }
}

object IndexedErgoTree {

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ 20.toByte

}
