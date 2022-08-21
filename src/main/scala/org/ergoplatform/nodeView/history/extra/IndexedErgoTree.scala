package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.settings.Algos
import scorex.core.{ModifierTypeId, idToBytes}
import scorex.core.serialization.ScorexSerializer
import scorex.util.{ModifierId, bytesToId}
import scorex.util.serialization.{Reader, Writer}
import sigmastate.Values.ErgoTree

import scala.collection.mutable.ListBuffer
import spire.syntax.all.cfor

case class IndexedErgoTree(treeHash: ModifierId, boxIds: ListBuffer[ModifierId]) extends BlockSection {
  override val sizeOpt: Option[Int] = None
  override def serializedId: Array[Byte] = idToBytes(treeHash)
  override def parentId: ModifierId = null
  override val modifierTypeId: ModifierTypeId = IndexedErgoTree.modifierTypeId
  override type M = IndexedErgoTree
  override def serializer: ScorexSerializer[IndexedErgoTree] = IndexedErgoTreeSerializer

  def retrieveBody(history: ErgoHistoryReader, lastN: Long): Seq[IndexedErgoBox] =
    (
      if(lastN > 0)
        boxIds.slice(math.max((boxIds.size - lastN).toInt, 0), boxIds.size)
      else
        boxIds
    ).map(history.typedModifierById[IndexedErgoBox](_).get)

  def addBox(id: ModifierId): IndexedErgoTree = {
    boxIds += id
    this
  }
}

object IndexedErgoTreeSerializer extends ScorexSerializer[IndexedErgoTree] {

  def ergoTreeHash(tree: ErgoTree): Array[Byte] = Algos.hash(tree.bytes)

  override def serialize(iEt: IndexedErgoTree, w: Writer): Unit = {
    w.putBytes(iEt.serializedId)
    w.putUInt(iEt.boxIds.length)
    cfor(0)(_ < iEt.boxIds.length, _ + 1) { i => w.putBytes(idToBytes(iEt.boxIds(i)))}
  }

  override def parse(r: Reader): IndexedErgoTree = {
    val treeHash: ModifierId = bytesToId(r.getBytes(32))
    val boxIdsLen: Long = r.getUInt()
    val boxIds: ListBuffer[ModifierId] = ListBuffer.empty[ModifierId]
    cfor(0)(_ < boxIdsLen, _ + 1) { _ => boxIds += bytesToId(r.getBytes(32))}
    IndexedErgoTree(treeHash, boxIds)
  }
}

object IndexedErgoTree {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ 20.toByte
}
