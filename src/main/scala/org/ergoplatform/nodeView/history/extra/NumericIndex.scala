package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.history.extra.ExtraIndexerRef.fastIdToBytes
import org.ergoplatform.settings.Algos
import scorex.core.ModifierTypeId
import scorex.core.serialization.ScorexSerializer
import scorex.util.{ModifierId, bytesToId}
import scorex.util.serialization.{Reader, Writer}

case class NumericTxIndex(n: Long, m: ModifierId) extends BlockSection {
  override val sizeOpt: Option[Int] = None
  override def serializedId: Array[Byte] = NumericTxIndex.indexToBytes(n)
  override def parentId: ModifierId = null
  override val modifierTypeId: ModifierTypeId = NumericTxIndex.modifierTypeId
  override type M = NumericTxIndex
  override def serializer: ScorexSerializer[NumericTxIndex] = NumericTxIndexSerializer

  def hashValue: Array[Byte] = Array.empty[Byte]
}
object NumericTxIndexSerializer extends ScorexSerializer[NumericTxIndex] {

  override def serialize(ni: NumericTxIndex, w: Writer): Unit = {
    w.putLong(ni.n)
    w.putBytes(fastIdToBytes(ni.m))
  }

  override def parse(r: Reader): NumericTxIndex = {
    val n: Long = r.getLong()
    val m: ModifierId = bytesToId(r.getBytes(32))
    NumericTxIndex(n, m)
  }
}

object NumericTxIndex {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ 25.toByte

  def indexToBytes(n: Long): Array[Byte] = Algos.hash("txns height " + n)

  def getTxByNumber(history: ErgoHistoryReader, n: Long): Option[IndexedErgoTransaction] =
    history.typedModifierById[IndexedErgoTransaction](history.typedModifierById[NumericTxIndex](bytesToId(NumericTxIndex.indexToBytes(n))).get.m)
}

case class NumericBoxIndex(n: Long, m: ModifierId) extends BlockSection {
  override val sizeOpt: Option[Int] = None
  override def serializedId: Array[Byte] = NumericBoxIndex.indexToBytes(n)
  override def parentId: ModifierId = null
  override val modifierTypeId: ModifierTypeId = NumericBoxIndex.modifierTypeId
  override type M = NumericBoxIndex
  override def serializer: ScorexSerializer[NumericBoxIndex] = NumericBoxIndexSerializer

  def hashValue: Array[Byte] = Array.empty[Byte]
}
object NumericBoxIndexSerializer extends ScorexSerializer[NumericBoxIndex] {

  override def serialize(ni: NumericBoxIndex, w: Writer): Unit = {
    w.putLong(ni.n)
    w.putBytes(fastIdToBytes(ni.m))
  }

  override def parse(r: Reader): NumericBoxIndex = {
    val n: Long = r.getLong()
    val m: ModifierId = bytesToId(r.getBytes(32))
    NumericBoxIndex(n, m)
  }
}

object NumericBoxIndex {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ 30.toByte

  def indexToBytes(n: Long): Array[Byte] = Algos.hash("boxes height " + n)

  def getBoxByNumber(history: ErgoHistoryReader, n: Long): Option[IndexedErgoBox] =
    history.typedModifierById[IndexedErgoBox](history.typedModifierById[NumericBoxIndex](bytesToId(NumericBoxIndex.indexToBytes(n))).get.m)
}
