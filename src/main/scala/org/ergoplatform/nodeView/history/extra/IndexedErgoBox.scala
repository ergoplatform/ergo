package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.{ErgoAddress, ErgoBox, Pay2SAddress}
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.nodeView.history.extra.ExtraIndexerRef.fastIdToBytes
import org.ergoplatform.nodeView.wallet.WalletBox
import org.ergoplatform.wallet.Constants.ScanId
import org.ergoplatform.wallet.boxes.{ErgoBoxSerializer, TrackedBox}
import scorex.core.ModifierTypeId
import scorex.core.serialization.ScorexSerializer
import scorex.util.{ModifierId, bytesToId}
import scorex.util.serialization.{Reader, Writer}
import sigmastate.Values.ErgoTree

class IndexedErgoBox(val inclusionHeightOpt: Option[Int],
                     var spendingTxIdOpt: Option[ModifierId],
                     var spendingHeightOpt: Option[Int],
                     val box: ErgoBox,
                     val globalIndex: Long,
                     val confirmations: Option[Int])
  extends WalletBox(TrackedBox(box.transactionId,
                               box.index,
                               inclusionHeightOpt,
                               spendingTxIdOpt,
                               spendingHeightOpt,
                               box,
                               Set.empty[ScanId]),
                               confirmations) with BlockSection {

  override def parentId: ModifierId = null
  override val modifierTypeId: ModifierTypeId = IndexedErgoBox.modifierTypeId
  override val sizeOpt: Option[Int] = None
  override def serializedId: Array[Byte] = box.id
  override type M = IndexedErgoBox
  override def serializer: ScorexSerializer[IndexedErgoBox] = IndexedErgoBoxSerializer

  def getAddress: ErgoAddress = IndexedErgoBoxSerializer.getAddress(box.ergoTree)

  def asSpent(txId: ModifierId, txHeight: Int): IndexedErgoBox = {
    spendingTxIdOpt = Some(txId)
    spendingHeightOpt = Some(txHeight)
    this
  }
}
object IndexedErgoBoxSerializer extends ScorexSerializer[IndexedErgoBox] {

  def getAddress(tree: ErgoTree): ErgoAddress =
    tree.root match {
      case Right(_) => ExtraIndexerRef.getAddressEncoder.fromProposition(tree).get // default most of the time
      case Left(_) => new Pay2SAddress(tree, tree.bytes)(ExtraIndexerRef.getAddressEncoder) // needed for burn address 4MQyMKvMbnCJG3aJ
    }

  override def serialize(iEb: IndexedErgoBox, w: Writer): Unit = {
    w.putOption[Int](iEb.inclusionHeightOpt)(_.putInt(_))
    w.putOption[ModifierId](iEb.spendingTxIdOpt)((ww, id) => ww.putBytes(fastIdToBytes(id)))
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
