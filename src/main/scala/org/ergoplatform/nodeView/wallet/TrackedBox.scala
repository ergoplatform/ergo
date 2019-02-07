package org.ergoplatform.nodeView.wallet

import io.circe.Encoder
import org.ergoplatform.ErgoBox
import org.ergoplatform.api.ApiEncoderOption.HideDetails
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import org.ergoplatform.nodeView.wallet.ChainStatus.{Offchain, Onchain}
import org.ergoplatform.nodeView.wallet.SpendingStatus.{Spent, Unspent}
import org.ergoplatform.settings.Algos
import scorex.util.{ModifierId, bytesToId}

/**
  * A box tracked by a wallet that contains Ergo box itself as well as
  * its state (e.g. spent or not, confirmed or not etc).
  *
  * @param creationTx       Transaction created the box
  * @param creationOutIndex Output index in the creation transaction
  * @param inclusionHeight  Height the transaction was included into blockchain
  * @param spendingTx       Transaction which spends the box if exists and known
  * @param spendingHeight   Height of the spending transaction block in blockchain if known
  * @param box              Underlying Ergo box
  * @param certainty        Whether the box is definitely belongs to the user or not
  */
case class TrackedBox(creationTx: ErgoTransaction,
                      creationOutIndex: Short,
                      inclusionHeight: Option[Height],
                      spendingTx: Option[ErgoTransaction],
                      spendingHeight: Option[Height],
                      box: ErgoBox,
                      certainty: BoxCertainty) {

  require(spendingHeight.isEmpty || inclusionHeight.nonEmpty,
    s"Onchain transaction $encodedSpendingTxId at height $spendingHeight " +
      s"is spending offchain box $encodedBoxId from transaction $encodedCreationTxId")

  /** Whether the box is spent or not
    */
  def spendingStatus: SpendingStatus = {
    if (spendingTx.isEmpty) Unspent else Spent
  }

  /** Whether box creation is confirmed or not.
    * Can be derived from `spendingStatus` and `chainStatus` combination
    */
  def creationChainStatus: ChainStatus = {
    if (inclusionHeight.isEmpty) Offchain else Onchain
  }

  /** Whether box spending is confirmed or not, `Offchain` for unspent boxes.
    * Can be derived from `spendingStatus` and `chainStatus` combination
    */
  def spendingChainStatus: ChainStatus = {
    if (spendingStatus == Unspent || spendingHeight.isEmpty) Offchain else Onchain
  }

  /** Same as `creationChainStatus` for unspent boxes,
    * same as `spendingChainStatus` for spent boxes
    */
  def chainStatus: ChainStatus = {
    if (creationChainStatus == Offchain || spendingStatus == Spent && spendingChainStatus == Offchain) {
      Offchain
    } else {
      Onchain
    }
  }

  lazy val boxId: ModifierId = bytesToId(box.id)

  def encodedBoxId: String = Algos.encode(boxId)

  def value: Long = box.value

  lazy val assets: Map[ModifierId, Long] = box.additionalTokens.map { case (id, amt) =>
    bytesToId(id) -> amt
  }.toMap

  def creationTxId: ModifierId = creationTx.id

  def encodedCreationTxId: String = Algos.encode(creationTxId)

  def spendingTxId: Option[ModifierId] = spendingTx.map(_.id)

  def encodedSpendingTxId: Option[String] = spendingTxId.map(Algos.encode)

  override def toString: String = {
    getClass.getSimpleName + " " + TrackedBox.encoder(this).noSpaces
  }

}

object TrackedBox {

  /**
    * Create unspent tracked box
    */
  def apply(creationTx: ErgoTransaction, creationOutIndex: Short, creationHeight: Option[Height],
            box: ErgoBox, certainty: BoxCertainty): TrackedBox = {
    apply(creationTx, creationOutIndex, creationHeight, None, None, box, certainty)
  }

  def encoder: Encoder[TrackedBox] = ErgoTransaction.trackedBoxEncoder(HideDetails)

}
