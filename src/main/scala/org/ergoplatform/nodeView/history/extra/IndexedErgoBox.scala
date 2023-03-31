package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.ErgoBox
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.{ExtraIndexTypeId, fastIdToBytes}
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer
import scorex.core.serialization.ScorexSerializer
import scorex.util.{ModifierId, bytesToId}
import scorex.util.serialization.{Reader, Writer}

/**
  * Wrapper with dditional information for ErgoBox.
  * @param inclusionHeight   - height of the block in which the creating transaction was included in
  * @param spendingTxIdOpt   - optional, id of the spending transaction
  * @param spendingHeightOpt - optional, height of the block in which the spending transaction was included in
  * @param box               - underlying ErgoBox
  * @param globalIndex       - serial number of this output counting from genesis box
  */
class IndexedErgoBox(val inclusionHeight: Int,
                     var spendingTxIdOpt: Option[ModifierId],
                     var spendingHeightOpt: Option[Int],
                     val box: ErgoBox,
                     val globalIndex: Long) extends ExtraIndex {

  override lazy val id: ModifierId = bytesToId(serializedId)
  override def serializedId: Array[Byte] = box.id


  /**
    * Fill in spending parameters.
    * @param txId     - id of the spending transaction
    * @param txHeight - height of the block in which the spending transaction was included in
    * @return this box
    */
  def asSpent(txId: ModifierId, txHeight: Int): IndexedErgoBox = {
    spendingTxIdOpt = Some(txId)
    spendingHeightOpt = Some(txHeight)
    this
  }

  /**
    * Whether or not the box is spent
    * @return true if spent, false otherwise
    */
  def isSpent: Boolean = spendingTxIdOpt.isDefined
}
object IndexedErgoBoxSerializer extends ScorexSerializer[IndexedErgoBox] {

  override def serialize(iEb: IndexedErgoBox, w: Writer): Unit = {
    w.putInt(iEb.inclusionHeight)
    w.putOption[ModifierId](iEb.spendingTxIdOpt)((ww, id) => ww.putBytes(fastIdToBytes(id)))
    w.putOption[Int](iEb.spendingHeightOpt)(_.putInt(_))
    ErgoBoxSerializer.serialize(iEb.box, w)
    w.putLong(iEb.globalIndex)
  }

  override def parse(r: Reader): IndexedErgoBox = {
    val inclusionHeight: Int = r.getInt()
    val spendingTxIdOpt: Option[ModifierId] = r.getOption[ModifierId](bytesToId(r.getBytes(32)))
    val spendingHeightOpt: Option[Int] = r.getOption[Int](r.getInt())
    val box: ErgoBox = ErgoBoxSerializer.parse(r)
    val globalIndex: Long = r.getLong()
    new IndexedErgoBox(inclusionHeight, spendingTxIdOpt, spendingHeightOpt, box, globalIndex)
  }
}

object IndexedErgoBox {
  val extraIndexTypeId: ExtraIndexTypeId = 5.toByte
}
