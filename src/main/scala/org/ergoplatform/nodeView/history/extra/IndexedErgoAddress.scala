package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.{ErgoAddressEncoder, ErgoBox}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.{ExtraIndexTypeId, fastIdToBytes}
import org.ergoplatform.nodeView.history.extra.IndexedErgoAddressSerializer.hashErgoTree
import org.ergoplatform.settings.Algos
import org.ergoplatform.serialization.ErgoSerializer
import scorex.util.{ModifierId, bytesToId}
import scorex.util.serialization.{Reader, Writer}
import sigma.ast.ErgoTree

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
  extends Segment[IndexedErgoAddress](id => IndexedErgoAddress(id), txs, boxes) with ExtraIndex {

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
    if(historyOpt.isDefined) {
      findAndModBox(iEb.globalIndex, historyOpt.get)
    }
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
      history.historyStorage.insertExtraTry(Array.empty, Array(this)).get // save the changes made to this address

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

  /**
    * Get a range of the transactions of this address, like [[retrieveTxs]], each listing among its
    * inputs, its outputs or both only the boxes of this address: those whose ErgoTree is the tree
    * of this address, the boxes the indexer associates with it.
    *
    * The boxes of other addresses are mostly not read, see [[ownBoxes]].
    *
    * The bodies are put on copies of the transactions, see [[IndexedErgoTransaction.retrieveBody]].
    *
    * @param history         - history to use
    * @param offset          - items to skip from the start
    * @param limit           - items to retrieve
    * @param relevantInputs  - whether to list only the inputs of this address
    * @param relevantOutputs - whether to list only the outputs of this address
    * @return array of transactions with (filtered) bodies
    */
  def retrieveTxs(history: ErgoHistoryReader,
                  offset: Int,
                  limit: Int,
                  relevantInputs: Boolean,
                  relevantOutputs: Boolean)(implicit segmentTreshold: Int): Array[IndexedErgoTransaction] =
    if (!relevantInputs && !relevantOutputs) {
      retrieveTxs(history, offset, limit)
    } else {
      val own = ownBoxes(history)
      lazy val all = IndexedErgoTransaction.allBoxes(history)
      val inputBoxes = if (relevantInputs) own else all
      val outputBoxes = if (relevantOutputs) own else all
      getFromSegments(history, offset, limit, txSegmentCount, txs, SegmentSerializer.txSegmentId, _.txs,
        (nums: ArrayBuffer[Long], h: ErgoHistoryReader) =>
          nums.map(n => NumericTxIndex.getTxByNumber(h, n).get.retrieveBody(h, inputBoxes, outputBoxes)).toArray)
    }

  /**
    * Reads the boxes of this address among given box numbers, see [[retrieveTxs]] with filters.
    *
    * Whether a box number is one of this address is looked up in the box numbers this address
    * lists ([[boxLookup]]); only the boxes it finds are read, and kept if their tree is the tree of
    * this address. A number the lookup cannot place, because a segment cannot be read, is read and
    * kept by its tree as well.
    *
    * The lookup needs this object to be unchanged while it is used, and its box numbers to cover
    * its transaction numbers. Both hold for a row read from the database: address rows are segments,
    * which the database cache does not keep (`HistoryStorage.getExtraIndex`), so every read parses
    * the row as the indexer last saved it, in one batch with its segments, and nobody else has it.
    *
    * @param history - database handle
    * @return function from box numbers to the boxes of this address among them
    */
  private[extra] def ownBoxes(history: ErgoHistoryReader): Array[Long] => IndexedSeq[IndexedErgoBox] = {
    val listed = boxLookup(history)
    nums => nums.flatMap { n =>
      val lookup = listed(n)
      if (lookup.contains(false)) {
        None
      } else {
        NumericBoxIndex.getBoxByNumber(history, n).filter { iEb =>
          val own = hashErgoTree(iEb.box.ergoTree) == treeHash
          if (!own && lookup.isDefined) log.debug(s"Address $treeHash lists box $n of another tree")
          own
        }
      }
    }
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
