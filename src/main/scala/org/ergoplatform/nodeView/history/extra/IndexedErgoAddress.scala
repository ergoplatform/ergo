package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.{ErgoAddressEncoder, ErgoBox}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.{ExtraIndexTypeId, fastIdToBytes}
import org.ergoplatform.nodeView.history.extra.IndexedErgoAddressSerializer.hashErgoTree
import org.ergoplatform.settings.Algos
import org.ergoplatform.serialization.ErgoSerializer
import scorex.util.{ModifierId, bytesToId}
import scorex.util.serialization.{Reader, Writer}
import sigmastate.Values.ErgoTree

import scala.collection.mutable.ArrayBuffer

/**
  * An index of an address (ErgoTree)
  * @param treeHash - hash of the corresponding ErgoTree
  * @param txs      - list of numberic transaction indexes
  * @param boxes    - list of numberic box indexes, negative values indicate the box is spent
  */
case class IndexedErgoAddress(treeHash: ModifierId,
                              override val txs: ArrayBuffer[Long] = new ArrayBuffer[Long],
                              override val boxes: ArrayBuffer[Long] = new ArrayBuffer[Long])
  extends Segment[IndexedErgoAddress](treeHash, id => IndexedErgoAddress(id), txs, boxes) with ExtraIndex {

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

  /**
   * Rollback the state of segments in memory and in db
   *
   * @param txTarget  - remove transaction numbers above this number
   * @param boxTarget - remove box numbers above this number
   * @param history   - history handle to update segment in database
   * @return modifier ids to remove
   */
  override private[extra] def rollback(txTarget: Long, boxTarget: Long, history: ErgoHistory)(implicit segmentTreshold: Int): Array[ModifierId] = {

    val toRemove: ArrayBuffer[ModifierId] = rollbackState(txTarget, boxTarget, history.getReader)

    if (txCount == 0 && boxCount == 0)
      toRemove += treeHash // all segments empty after rollback, delete parent
    else
      history.historyStorage.insertExtra(Array.empty, Array(this)) // save the changes made to this address

    toRemove.toArray
  }

  /**
   * Filter mempool boxes if API call requires it
   *
   * @param boxes - all boxes in mempool
   * @return associated boxes
   */
  override private[extra] def filterMempool(boxes: Seq[ErgoBox]): Seq[ErgoBox] =
    boxes.filter(box => hashErgoTree(box.ergoTree) == treeHash)

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
