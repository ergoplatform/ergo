package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.{ErgoAddress, ErgoBox, Pay2SAddress}
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.nodeView.wallet.WalletBox
import org.ergoplatform.wallet.Constants.ScanId
import org.ergoplatform.wallet.boxes.{ErgoBoxSerializer, TrackedBox}
import scorex.core.ModifierTypeId
import scorex.core.serialization.ScorexSerializer
import scorex.util.{ModifierId, bytesToId, idToBytes}
import scorex.util.serialization.{Reader, Writer}

class IndexedErgoBox(val inclusionHeightOpt: Option[Int],
                     val spendingTxIdOpt: Option[ModifierId],
                     val spendingHeightOpt: Option[Int],
                     val box: ErgoBox,
                     val globalIndex: Long,
                     val confirmations: Option[Int])
  extends WalletBox(new TrackedBox(box.transactionId,
                                   box.index,
                                   inclusionHeightOpt,
                                   spendingTxIdOpt,
                                   spendingHeightOpt,
                                   box,
                                   Set.empty[ScanId]),
                                   confirmations
                   ) with BlockSection {

  override def parentId: ModifierId = null
  override val modifierTypeId: ModifierTypeId = IndexedErgoBox.modifierTypeId
  override val sizeOpt: Option[Int] = None
  override def serializedId: Array[Byte] = box.id
  override type M = IndexedErgoBox
  override def serializer: ScorexSerializer[IndexedErgoBox] = IndexedErgoBoxSerializer

  def getAddress: ErgoAddress = box.ergoTree.root match {
    case Right(_) => ExtraIndexerRef.getAddressEncoder.fromProposition(box.ergoTree).get // default most of the time
    case Left(_) => new Pay2SAddress(box.ergoTree, box.ergoTree.bytes)(ExtraIndexerRef.getAddressEncoder) // needed for burn address 4MQyMKvMbnCJG3aJ
  }

  def toBytes: Array[Byte] = serializer.toBytes(this)

  def asSpent(txId: ModifierId, txHeight: Int): IndexedErgoBox =
    new IndexedErgoBox(inclusionHeightOpt, Some(txId), Some(txHeight), box, globalIndex, confirmations)
}
object IndexedErgoBoxSerializer extends ScorexSerializer[IndexedErgoBox] {

  override def serialize(iEb: IndexedErgoBox, w: Writer): Unit = {
    w.putUByte(IndexedErgoBox.modifierTypeId)
    w.putOption[Int](iEb.inclusionHeightOpt)(_.putInt(_))
    w.putOption[ModifierId](iEb.spendingTxIdOpt)((ww, id) => ww.putBytes(idToBytes(id)))
    w.putOption[Int](iEb.spendingHeightOpt)(_.putInt(_))
    ErgoBoxSerializer.serialize(iEb.box, w)
    w.putLong(iEb.globalIndex)
    w.putOption[Int](iEb.confirmations)(_.putInt(_))
  }

  override def parse(r: Reader): IndexedErgoBox = {
    val inclusionHeightOpt: Option[Int] = r.getOption[Int](r.getInt())
    val spendingTxIdOpt: Option[ModifierId] = r.getOption[ModifierId](bytesToId(r.getBytes(32)))
    val spendingHeightOpt: Option[Int] = r.getOption[Int](r.getInt())
    val box: ErgoBox = ErgoBoxSerializer.parse(r)
    val globalIndex: Long = r.getLong()
    val confirmations: Option[Int] = r.getOption[Int](r.getInt())
    new IndexedErgoBox(inclusionHeightOpt, spendingTxIdOpt, spendingHeightOpt, box, globalIndex, confirmations)
  }
}

object IndexedErgoBox {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ 5.toByte
}
