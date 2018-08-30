package org.ergoplatform.nodeView.wallet

import io.circe.Encoder
import org.ergoplatform.ErgoBox
import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.api.ApiEncoderOption.HideDetails
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import org.ergoplatform.nodeView.wallet.BoxCertainty.Certain
import org.ergoplatform.nodeView.wallet.ChainStatus.{Offchain, Onchain}
import org.ergoplatform.nodeView.wallet.SpendingStatus.{Spent, Unspent}
import org.ergoplatform.settings.Algos
import scorex.core.{ModifierId, bytesToId}

/**
  * A generic interface for a box tracked by a wallet. A TrackedBox instantiation contains box itself as well as
  * its state (e.g. spent or not, confirmed or not etc).
  */
case class TrackedBox(
                      /**
                        * Transaction created the box
                        */
                      creationTx: ErgoTransaction,

                      /**
                        * Output index in the creation transaction
                        */
                      creationOutIndex: Short,

                      /**
                        * Height of the creation transaction block in blockchain if known
                        */
                      creationHeight: Option[Height],

                      /**
                        * Transaction which spends the box if exists and known
                        */
                      spendingTx: Option[ErgoTransaction],

                      /**
                        * Height of the spending transaction block in blockchain if known
                        */
                      spendingHeight: Option[Height],

                      /**
                        * Underlying Ergo box
                        */
                      box: ErgoBox,

                      /**
                        * Whether the box is definitely belongs to the user or not
                        */
                      certainty: BoxCertainty) {

  require(spendingHeight.isEmpty || creationHeight.nonEmpty,
    s"Onchain transaction $encodedSpendingTxId at height $spendingHeight " +
    s"is spending offchain box $encodedBoxId from transaction $encodedCreationTxId")

  /**
    * Whether the box is spent or not
    */
  def spendingStatus: SpendingStatus = {
    if (spendingTx.isEmpty) Unspent else Spent
  }

  /**
    * Whether the box is confirmed or not
    */
  def chainStatus: ChainStatus = {
    if (creationHeight.isEmpty || spendingTx.nonEmpty && spendingHeight.isEmpty) Offchain else Onchain
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

}

object TrackedBox {

  /**
    * Create unspent tracked box
    */
  def apply(creationTx: ErgoTransaction, creationOutIndex: Short, creationHeight: Option[Height],
            box: ErgoBox, certainty: BoxCertainty): TrackedBox = {
    apply(creationTx, creationOutIndex, creationHeight, None, None, box, certainty)
  }

}
