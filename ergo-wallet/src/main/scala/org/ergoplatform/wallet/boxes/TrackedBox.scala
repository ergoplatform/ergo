package org.ergoplatform.wallet.boxes

import org.ergoplatform.wallet.Constants
import org.ergoplatform.wallet.serialization.ErgoWalletSerializer
import org.ergoplatform.{ErgoBox, ErgoLikeTransaction}
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, bytesToId, idToBytes}

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
  * @param applicationStatuses - Identifier of applications the box refers to and corresponding statuses
  *                            (whether the box definitely belongs to the application or not really)
  */
final case class TrackedBox(creationTxId: ModifierId,
                            creationOutIndex: Short,
                            inclusionHeightOpt: Option[Int],
                            spendingTxIdOpt: Option[ModifierId],
                            spendingHeightOpt: Option[Int],
                            box: ErgoBox,
                            applicationStatuses: Seq[(Short, BoxCertainty)]) {

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

  lazy val value: Long = box.value

  lazy val spent: Boolean = spendingHeightOpt.isDefined

  lazy val assets: Map[ModifierId, Long] = box.additionalTokens.toArray.map {
    case (id, amt) => bytesToId(id) -> amt
  }.toMap

}

object TrackedBox {

  def apply(creationTx: ErgoLikeTransaction, creationOutIndex: Short, creationHeight: Option[Int],
            box: ErgoBox, appStatuses: Seq[(Short, BoxCertainty)]): TrackedBox =
    apply(creationTx.id, creationOutIndex, creationHeight, None, None, box, appStatuses)
}

object TrackedBoxSerializer extends ErgoWalletSerializer[TrackedBox] {
  val walletAppId: Short = Constants.PaymentsAppId

  override def serialize(obj: TrackedBox, w: Writer): Unit = {
    w.putBytes(idToBytes(obj.creationTxId))
    w.putShort(obj.creationOutIndex)
    w.putOption(obj.inclusionHeightOpt)(_.putInt(_))
    w.putOption(obj.spendingTxIdOpt)((bf, id) => bf.putBytes(idToBytes(id)))
    w.putOption(obj.spendingHeightOpt)(_.putInt(_))

    val appsCount = obj.applicationStatuses.size.toShort

    if (appsCount == 1 && obj.applicationStatuses.head._1 == walletAppId) {
      w.putShort(0)
    } else {
      w.putShort(appsCount)
      obj.applicationStatuses.foreach { case (appId, certainty) =>
        w.putShort(appId)
        w.put(if (certainty.certain) 0x01 else 0x00)
      }
    }
    ErgoBoxSerializer.serialize(obj.box, w)
  }

  override def parse(r: Reader): TrackedBox = {
    val creationTxId = bytesToId(r.getBytes(Constants.ModifierIdLength))
    val creationOutIndex = r.getShort()
    val inclusionHeightOpt = r.getOption(r.getInt())
    val spendingTxIdOpt = r.getOption(r.getBytes(Constants.ModifierIdLength)).map(bytesToId)
    val spendingHeightOpt = r.getOption(r.getInt())

    val appsCount = r.getShort()
    val appStatuses: Seq[(Short, BoxCertainty)] = if (appsCount == 0){
      Seq((walletAppId, BoxCertainty.Certain))
    } else {
      (0 until appsCount)
        .map(_ => (r.getShort(), if (r.getByte() == 0x01) BoxCertainty.Certain else BoxCertainty.Uncertain))
    }
    val box = ErgoBoxSerializer.parse(r)
    TrackedBox(
      creationTxId, creationOutIndex, inclusionHeightOpt, spendingTxIdOpt, spendingHeightOpt, box, appStatuses)
  }

}
