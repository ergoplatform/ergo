package org.ergoplatform.wallet.boxes

import org.ergoplatform.sdk.wallet
import org.ergoplatform.sdk.wallet.TokensMap
import org.ergoplatform.wallet.Constants
import org.ergoplatform.wallet.Constants.ScanId
import org.ergoplatform.wallet.serialization.ErgoWalletSerializer
import org.ergoplatform.{ErgoBox, ErgoBoxAssets, ErgoLikeTransaction}
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, bytesToId, idToBytes}
import sigma.Extensions._

/**
  * A box tracked by a wallet that contains Ergo box itself as well as
  * its state (e.g. spent or not, confirmed or not etc).
  *
  * @param creationTxId        - Id of transaction created the box
  * @param creationOutIndex    - Output index in the creation transaction
  * @param inclusionHeightOpt  - Height the transaction was included into blockchain
  * @param spendingTxIdOpt     - Id of transaction which spends the box if exists and known
  * @param spendingHeightOpt   - Height of the spending transaction block in blockchain if known
  * @param box                 - Underlying Ergo box
  * @param scans               - Identifiers of scans the box refers to
  */
case class TrackedBox(creationTxId: ModifierId,
                      creationOutIndex: Short,
                      inclusionHeightOpt: Option[Int],
                      spendingTxIdOpt: Option[ModifierId],
                      spendingHeightOpt: Option[Int],
                      box: ErgoBox,
                      scans: Set[ScanId]) extends ErgoBoxAssets {

  /**
    * Whether the box is spent or not
    */
  def spendingStatus: SpendingStatus =
    if (spendingTxIdOpt.isEmpty) SpendingStatus.Unspent
    else SpendingStatus.Spent

  /**
    * Whether box creation is confirmed or not.
    * Can be derived from `spendingStatus` and `chainStatus` combination
    */
  def creationChainStatus: ChainStatus =
    if (inclusionHeightOpt.isEmpty) ChainStatus.OffChain
    else ChainStatus.OnChain

  /**
    * Whether box spending is confirmed or not, `Offchain` for unspent boxes.
    * Can be derived from `spendingStatus` and `chainStatus` combination
    */
  def spendingChainStatus: ChainStatus =
    if (spendingStatus == SpendingStatus.Unspent || spendingHeightOpt.isEmpty) ChainStatus.OffChain
    else ChainStatus.OnChain

  /**
    * Same as `creationChainStatus` for unspent boxes,
    * same as `spendingChainStatus` for spent boxes
    */
  def chainStatus: ChainStatus =
    if (creationChainStatus == ChainStatus.OffChain || spendingStatus == SpendingStatus.Spent &&
      spendingChainStatus == ChainStatus.OffChain) ChainStatus.OffChain
    else ChainStatus.OnChain

  lazy val boxId: ModifierId = bytesToId(box.id)

  override def value: Long = box.value

  def isSpent: Boolean = spendingHeightOpt.isDefined

  lazy val tokens: TokensMap = box.additionalTokens.toArray.map {
    case (id, amt) => id.toModifierId -> amt
  }.toMap

  override def equals(obj: Any): Boolean = obj match {
    case tb: TrackedBox => tb.creationTxId == creationTxId && tb.creationOutIndex == creationOutIndex && box == tb.box
    case _ => false
  }

  override def hashCode(): Int = creationTxId.hashCode() * 31 + creationOutIndex.hashCode()

}

object TrackedBox {

  def apply(creationTx: ErgoLikeTransaction, creationOutIndex: Short, creationHeight: Option[Int],
            box: ErgoBox, appStatuses: Set[ScanId]): TrackedBox =
    apply(creationTx.id, creationOutIndex, creationHeight, None, None, box, appStatuses)

  /**
    * Creates unspent box with given inclusion height and scans the box is associated with
    * @param box
    * @param inclusionHeight
    * @param scans
    * @return
    */
  def apply(box: ErgoBox, inclusionHeight: Int, scans: Set[ScanId]): TrackedBox = {
    new TrackedBox(box.transactionId, box.index, Some(inclusionHeight), None, None, box, scans)
  }

}

object TrackedBoxSerializer extends ErgoWalletSerializer[TrackedBox] {

  override def serialize(obj: TrackedBox, w: Writer): Unit = {
    w.putBytes(idToBytes(obj.creationTxId))
    w.putShort(obj.creationOutIndex)
    w.putOption(obj.inclusionHeightOpt)(_.putInt(_))
    w.putOption(obj.spendingTxIdOpt)((bf, id) => bf.putBytes(idToBytes(id)))
    w.putOption(obj.spendingHeightOpt)(_.putInt(_))

    val appsCount = obj.scans.size.toShort

    if (appsCount == 1 && obj.scans.head == Constants.PaymentsScanId) {
      w.putShort(0)
    } else {
      w.putShort(appsCount)
      obj.scans.foreach { scanId =>
        w.putShort(scanId)
      }
    }
    ErgoBoxSerializer.serialize(obj.box, w)
  }

  override def parse(r: Reader): TrackedBox = {
    val creationTxId = bytesToId(r.getBytes(wallet.Constants.ModifierIdLength))
    val creationOutIndex = r.getShort()
    val inclusionHeightOpt = r.getOption(r.getInt())
    val spendingTxIdOpt = r.getOption(r.getBytes(wallet.Constants.ModifierIdLength)).map(bytesToId)
    val spendingHeightOpt = r.getOption(r.getInt())

    val appsCount = r.getShort()
    val appStatuses: Set[ScanId] = if (appsCount == 0){
      Set(Constants.PaymentsScanId)
    } else {
      (0 until appsCount).map(_ => ScanId @@ r.getShort()).toSet
    }
    val box = ErgoBoxSerializer.parse(r)
    TrackedBox(
      creationTxId, creationOutIndex, inclusionHeightOpt, spendingTxIdOpt, spendingHeightOpt, box, appStatuses)
  }

}
