package org.ergoplatform.nodeView.wallet

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import org.ergoplatform.nodeView.wallet.BoxCertainty.Certain
import org.ergoplatform.nodeView.wallet.OnchainStatus.{Offchain, Onchain}
import org.ergoplatform.nodeView.wallet.SpendingStatus.{Spent, Unspent}
import scorex.core.utils.ScorexLogging

sealed trait TrackedBox extends ScorexLogging {
  def certainty: BoxCertainty
  def onchainStatus: OnchainStatus
  def spendingStatus: SpendingStatus

  final def spent: Boolean = spendingStatus.spent
  final def onchain: Boolean = onchainStatus.onchain
  final def certain: Boolean = certainty.certain

  def creationTx: ErgoTransaction
  def creationOutIndex: Short
  val box: ErgoBox
  lazy val boxId = ByteArrayWrapper(box.id)
  def value: Long = box.value

  lazy val assets = box.additionalTokens.map { case (id, amt) =>
    ByteArrayWrapper(id) -> amt
  }.toMap

  def register(): Unit = Registry.putToRegistry(this)

  def deregister(): Unit = Registry.removeFromRegistry(boxId)

  def transition(spendingTransaction: ErgoTransaction, spendingHeightOpt: Option[Height]): Option[TrackedBox]

  def transition(creationHeight: Height): Option[TrackedBox]

  def transitionBack(toHeight: Int): Option[TrackedBox]

  def makeCertain(): TrackedBox

}

sealed trait UnspentBox extends TrackedBox {
  final def spendingStatus: SpendingStatus = Unspent
}

sealed trait SpentBox extends TrackedBox {
  final def spendingStatus: SpendingStatus = Spent
  val spendingTx: ErgoTransaction
}

sealed trait OffchainBox extends TrackedBox {
  final def onchainStatus: OnchainStatus = Offchain
}

sealed trait OnchainBox extends TrackedBox {
  final def onchainStatus: OnchainStatus = Onchain
}

case class UnspentOffchainBox(creationTx: ErgoTransaction,
                              creationOutIndex: Short,
                              box: ErgoBox,
                              certainty: BoxCertainty) extends UnspentBox with OffchainBox {
  override def register(): Unit = {
    super.register()
    log.info("New offchain box arrived: " + this)
    if (certain) Registry.increaseBalances(this)
  }

  override def deregister(): Unit = {
    super.deregister()
    if (certain) Registry.decreaseBalances(this)
  }

  def transition(creationHeight: Height): Option[TrackedBox] =
    Some(UnspentOnchainBox(creationTx, creationOutIndex, creationHeight, box, certainty))

  def transition(spendingTransaction: ErgoTransaction, heightOpt: Option[Height]): Option[TrackedBox] = {
    heightOpt match {
      case Some(_) => log.warn(s"Onchain transaction ${spendingTransaction.id} is spending offchain box $box"); None
      case None => Some(SpentOffchainBox(creationTx, creationOutIndex, None, spendingTransaction, box, certainty))
    }
  }

  def transitionBack(toHeight: Int): Option[TrackedBox] = None

  def makeCertain(): UnspentOffchainBox = if (certain) this else copy(certainty = Certain)
}

case class UnspentOnchainBox(creationTx: ErgoTransaction,
                             creationOutIndex: Short,
                             creationHeight: Int,
                             box: ErgoBox,
                             certainty: BoxCertainty) extends UnspentBox with OnchainBox {

  override def register(): Unit = {
    super.register()
    log.info("New onchain box arrived: " + this)
    Registry.putToConfirmedIndex(creationHeight, boxId)
    if (certain) Registry.increaseBalances(this)
  }

  override def deregister(): Unit = {
    super.deregister()
    if (certain) Registry.decreaseBalances(this)
  }

  def transition(spendingTransaction: ErgoTransaction, heightOpt: Option[Height]): Option[TrackedBox] = {
    Some(heightOpt match {
      case Some(h) =>
        SpentOnchainBox(creationTx, creationOutIndex, creationHeight, spendingTransaction, h, box, certainty)
      case None =>
        SpentOffchainBox(creationTx, creationOutIndex, Some(creationHeight), spendingTransaction, box, certainty)
    })
  }

  def transition(creationHeight: Height): Option[TrackedBox] = {
    log.warn(s"Double creation of UncertainUnspentOnchainBox for $boxId")
    None
  }

  def transitionBack(toHeight: Int): Option[TrackedBox] = {
    if (creationHeight > toHeight) {
      Some(UnspentOffchainBox(creationTx, creationOutIndex, box, certainty))
    } else {
      None
    }
  }

  def makeCertain(): UnspentOnchainBox = if (certain) this else copy(certainty = Certain)

}

case class SpentOffchainBox(creationTx: ErgoTransaction,
                            creationOutIndex: Short,
                            creationHeight: Option[Int],
                            spendingTx: ErgoTransaction,
                            box: ErgoBox,
                            certainty: BoxCertainty) extends SpentBox with OffchainBox {

  def transition(spendingTransaction: ErgoTransaction, heightOpt: Option[Height]): Option[TrackedBox] = {
    heightOpt match {
      case Some(h) =>
        require(creationHeight.isDefined)
        Some(SpentOnchainBox(creationTx, creationOutIndex, creationHeight.get, spendingTransaction, h, box, certainty))
      case None =>
        log.warn(s"Double spending of an unconfirmed box $boxId")
        //todo: handle double-spending strategy for an unconfirmed tx
        None
    }
  }

  def transition(creationHeight: Height): Option[TrackedBox] = this.creationHeight match {
    case Some(_) => log.warn(s"Double creation of $boxId"); None
    case None => Some(copy(creationHeight = Some(creationHeight)))
  }

  def transitionBack(toHeight: Int): Option[TrackedBox] = creationHeight match {
    case Some(h) if h < toHeight => Some(copy(creationHeight = None))
    case _ => None
  }

  def makeCertain(): SpentOffchainBox = if (certain) this else copy(certainty = Certain)

}

case class SpentOnchainBox(creationTx: ErgoTransaction,
                           creationOutIndex: Short,
                           creationHeight: Height,
                           spendingTx: ErgoTransaction,
                           spendingHeight: Height,
                           box: ErgoBox,
                           certainty: BoxCertainty) extends SpentBox with OnchainBox {

  override def register(): Unit = {
    super.register()
    Registry.putToConfirmedIndex(spendingHeight, boxId)
  }

  def transition(spendingTransaction: ErgoTransaction, heightOpt: Option[Height]): Option[TrackedBox] = None

  def transition(creationHeight: Height): Option[TrackedBox] = None

  def transitionBack(toHeight: Int): Option[TrackedBox] = {
    (toHeight < spendingHeight, toHeight < creationHeight) match {
      case (false, false) => None
      case (true, false) => Some(UnspentOnchainBox(creationTx, creationOutIndex, creationHeight, box, certainty))
      case (true, true) => Some(UnspentOffchainBox(creationTx, creationOutIndex, box, certainty))
      case (false, true) => log.warn(s"Wrong heights. Spending: $spendingHeight, creation: $creationHeight"); None
    }
  }

  def makeCertain(): SpentOnchainBox = if (certain) this else copy(certainty = Certain)

}
