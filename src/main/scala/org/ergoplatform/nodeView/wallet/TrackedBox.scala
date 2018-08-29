package org.ergoplatform.nodeView.wallet

import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import org.ergoplatform.nodeView.wallet.OnchainStatus.{Offchain, Onchain}
import org.ergoplatform.nodeView.wallet.SpendingStatus.{Spent, Unspent}
import org.ergoplatform.settings.Algos
import scorex.core.{ModifierId, bytesToId}

/**
  * A generic interface for a box tracked by a wallet. A TrackedBox instantiation contains box itself as well as
  * its state (e.g. spent or not, confirmed or not etc).
  */
sealed trait TrackedBox {

  /**
    * Transaction created the box
    */
  def creationTx: ErgoTransaction

  /**
    * Output index in the creation transaction
    */
  def creationOutIndex: Short

  /**
    * Height of the creation transaction block in blockchain if known
    */
  def creationHeightOpt: Option[Height]

  /**
    * Transaction which spends the box if exists and known
    */
  def spendingTxOpt: Option[ErgoTransaction]

  /**
    * Height of the spending transaction block in blockchain if known
    */
  def spendingHeightOpt: Option[Height]

  /**
    * Underlying Ergo box
    */
  def box: ErgoBox

  /**
    * Whether the box is definitely belongs to the user or not
    */
  def certainty: BoxCertainty

  /**
    * Whether the box is spent or not
    */
  def spendingStatus: SpendingStatus

  /**
    * Whether the box is confirmed or not
    */
  def onchainStatus: OnchainStatus = {
    if (creationHeightOpt.isEmpty || spendingTxOpt.nonEmpty && spendingHeightOpt.isEmpty) Offchain else Onchain
  }

  lazy val boxId: ModifierId = bytesToId(box.id)

  def encodedBoxId: String = Algos.encode(boxId)

  def value: Long = box.value

  lazy val assets: Map[ModifierId, Long] = box.additionalTokens.map { case (id, amt) =>
    bytesToId(id) -> amt
  }.toMap

  def creationTxId: ModifierId = creationTx.id

  def encodedCreationTxId: String = Algos.encode(creationTxId)

  def spendingTxId: Option[ModifierId] = spendingTxOpt.map(_.id)

  def encodedSpendingTxId: Option[String] = spendingTxId.map(Algos.encode)

  def copy(creationTx: ErgoTransaction = creationTx,
           creationOutIndex: Short = creationOutIndex,
           creationHeightOpt: Option[Height] = creationHeightOpt,
           spendingTxOpt: Option[ErgoTransaction] = spendingTxOpt,
           spendingHeightOpt: Option[Height] = spendingHeightOpt,
           box: ErgoBox = box,
           certainty: BoxCertainty = certainty): TrackedBox = {
    TrackedBox(creationTx, creationOutIndex, creationHeightOpt, spendingTxOpt, spendingHeightOpt,
               box, certainty)
  }

  require(spendingTxOpt.nonEmpty == spendingStatus.spent,
    s"Box $encodedBoxId spending status $spendingStatus " +
    s"should correspond to spendingTx optional value $spendingTxOpt")

  require(spendingHeightOpt.isEmpty || creationHeightOpt.nonEmpty,
    s"Onchain transaction $encodedSpendingTxId at height $spendingHeightOpt " +
    s"is spending offchain box $encodedBoxId from transaction $encodedCreationTxId")

}

object TrackedBox {

  def apply(creationTx: ErgoTransaction,
            creationOutIndex: Short,
            creationHeightOpt: Option[Height],
            spendingTxOpt: Option[ErgoTransaction],
            spendingHeightOpt: Option[Height],
            box: ErgoBox,
            certainty: BoxCertainty): TrackedBox = {
    spendingTxOpt match {
      case None => UnspentBox(creationTx, creationOutIndex, creationHeightOpt, box, certainty)
      case Some(spendingTx) => SpentBox(creationTx, creationOutIndex, creationHeightOpt,
                                        spendingTx, spendingHeightOpt, box, certainty)
    }
  }

}

final case class UnspentBox(creationTx: ErgoTransaction,
                            creationOutIndex: Short,
                            creationHeightOpt: Option[Height],
                            box: ErgoBox,
                            certainty: BoxCertainty) extends TrackedBox {

  def spendingStatus: SpendingStatus = Unspent
  def spendingTxOpt: Option[ErgoTransaction] = None
  def spendingHeightOpt: Option[Height] = None
}

final case class SpentBox(creationTx: ErgoTransaction,
                          creationOutIndex: Short,
                          creationHeightOpt: Option[Height],
                          spendingTx: ErgoTransaction,
                          spendingHeightOpt: Option[Height],
                          box: ErgoBox,
                          certainty: BoxCertainty) extends TrackedBox {

  def spendingStatus: SpendingStatus = Spent
  def spendingTxOpt: Option[ErgoTransaction] = Option(spendingTx)
}
