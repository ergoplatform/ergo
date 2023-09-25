package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.{ExtraIndexTypeId, fastIdToBytes}
import org.ergoplatform.settings.Algos
import scorex.core.serialization.ErgoSerializer
import scorex.util.{ModifierId, bytesToId}
import scorex.util.serialization.{Reader, Writer}
import sigmastate.Values.ErgoTree

/**
  * An index of an address (ErgoTree)
  * @param treeHash    - hash of the corresponding ErgoTree
  * @param txs         - list of numberic transaction indexes associated with this address
  * @param boxes       - list of numberic box indexes associated with this address, negative values indicate the box is spent
  */
case class IndexedErgoAddress(treeHash: ModifierId)
  extends Segment[IndexedErgoAddress](treeHash, id => IndexedErgoAddress(id)) with ExtraIndex {

  override lazy val id: ModifierId = treeHash
  override def serializedId: Array[Byte] = fastIdToBytes(treeHash)

  /**
   * Balance information (Optional because fragments do not contain it)
   */
  var balanceInfo: Option[BalanceInfo] = None

  private[extra] def initBalance: IndexedErgoAddress = {
    balanceInfo = Some(BalanceInfo())
    this
  }

  /**
    * Associate box with this address and update BalanceInfo
    * @param iEb - box to use
    * @param record - whether to add box to boxes list, used in rollbacks (true by default)
    * @return this address
    */
  override private[extra] def addBox(iEb: IndexedErgoBox, record: Boolean = true): IndexedErgoAddress = {
    if(record) boxes += iEb.globalIndex
    balanceInfo.foreach(_.add(iEb.box))
    this
  }

  /**
    * Update BalanceInfo by spending a box associated with this address
    * @param iEb - box to spend
    * @param historyOpt - history handle to update address fragment if spent box is old
    * @return this address
    */
  override private[extra] def spendBox(iEb: IndexedErgoBox, historyOpt: Option[ErgoHistoryReader] = None)(implicit ae: ErgoAddressEncoder): IndexedErgoAddress = {
    if(historyOpt.isDefined)
      findAndModBox(iEb.globalIndex, historyOpt.get)
    balanceInfo.foreach(_.subtract(iEb.box))
    this
  }

  /**
   * Add transaction index
   *
   * @param tx - numeric transaction index
   * @return this
   */
  override private[extra] def addTx(tx: Long): IndexedErgoAddress = {
    if (txs.lastOption.getOrElse(-1) != tx) txs += tx // check for duplicates
    this
  }

}

object IndexedErgoAddressSerializer extends ErgoSerializer[IndexedErgoAddress] {

  /**
    * Compute the Blake2b hash of given ErgoTree
    * @param tree - tree to use
    * @return hex representation of hash
    */
  def hashErgoTree(tree: ErgoTree): ModifierId = bytesToId(Algos.hash(tree.bytes))

  override def serialize(iEa: IndexedErgoAddress, w: Writer): Unit = {
    w.putBytes(iEa.serializedId)
    w.putOption[BalanceInfo](iEa.balanceInfo)((ww, bI) => BalanceInfoSerializer.serialize(bI, ww))
    SegmentSerializer.serialize(iEa, w)
  }

  override def parse(r: Reader): IndexedErgoAddress = {
    val addressHash: ModifierId = bytesToId(r.getBytes(32))
    val iEa: IndexedErgoAddress = new IndexedErgoAddress(addressHash)
    iEa.balanceInfo = r.getOption[BalanceInfo](BalanceInfoSerializer.parse(r))
    SegmentSerializer.parse(r, iEa)
    iEa
  }
}

object IndexedErgoAddress {

  val extraIndexTypeId: ExtraIndexTypeId = 15.toByte

}
