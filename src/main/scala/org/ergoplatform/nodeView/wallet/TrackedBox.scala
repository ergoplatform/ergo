package org.ergoplatform.nodeView.wallet

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import org.ergoplatform.nodeView.wallet.BoxCertainty.Certain
import org.ergoplatform.nodeView.wallet.OnchainStatus.{Offchain, Onchain}
import org.ergoplatform.nodeView.wallet.SpendingStatus.{Spent, Unspent}
import org.ergoplatform.settings.Algos
import scorex.core.utils.ScorexLogging

sealed trait TrackedBox extends ScorexLogging {

  def spendingStatus: SpendingStatus
  def onchainStatus: OnchainStatus
  def certainty: BoxCertainty

  final def spent: Boolean = spendingStatus.spent
  final def onchain: Boolean = onchainStatus.onchain
  final def certain: Boolean = certainty.certain

  def creationTx: ErgoTransaction
  def creationOutIndex: Short
  val box: ErgoBox
  lazy val boxId = ByteArrayWrapper(box.id)
  def value: Long = box.value

  lazy val assets: Map[ByteArrayWrapper, Long] = box.additionalTokens.map { case (id, amt) =>
    ByteArrayWrapper(id) -> amt
  }.toMap

  def register(registry: Registry): Unit = registry.putToRegistry(this)

  def deregister(registry: Registry): Unit = registry.removeFromRegistry(boxId)

  def transition(spendingTransaction: ErgoTransaction, spendingHeightOpt: Option[Height]): Option[TrackedBox]

  def transition(creationHeight: Height): Option[TrackedBox]

  def transitionBack(toHeight: Height): Option[TrackedBox]

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
  override def register(registry: Registry): Unit = {
    super.register(registry)
    log.info("New offchain box arrived: " + this)
    if (certain) registry.increaseBalances(this)
  }

  override def deregister(registry: Registry): Unit = {
    super.deregister(registry)
    if (certain) registry.decreaseBalances(this)
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

  override def register(registry: Registry): Unit = {
    super.register(registry: Registry)
    log.info("New onchain box arrived: " + this)
    registry.putToConfirmedIndex(creationHeight, boxId)
    if (certain) registry.increaseBalances(this)
  }

  override def deregister(registry: Registry): Unit = {
    super.deregister(registry)
    if (certain) registry.decreaseBalances(this)
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

  def transitionBack(toHeight: Height): Option[TrackedBox] = {
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
                            creationHeightOpt: Option[Int],
                            spendingTx: ErgoTransaction,
                            box: ErgoBox,
                            certainty: BoxCertainty) extends SpentBox with OffchainBox {

  def transition(spendingTransaction: ErgoTransaction, heightOpt: Option[Height]): Option[TrackedBox] = {
    heightOpt match {
      case Some(h) =>
        creationHeightOpt match {
          case Some(creationHeight) =>
            Some(SpentOnchainBox(creationTx, creationOutIndex, creationHeight, spendingTransaction, h, box, certainty))
          case None =>
            log.error(s"Invalid state for ${Algos.encode(box.id)}: no creation height, but spent on-chain.")
            None
        }
      case None =>
        log.warn(s"Double spending of an unconfirmed box $boxId")
        //todo: handle double-spending strategy for an unconfirmed tx
        None
    }
  }

  def transition(creationHeight: Height): Option[TrackedBox] = this.creationHeightOpt match {
    case Some(_) => log.warn(s"Double creation of $boxId"); None
    case None => Some(copy(creationHeightOpt = Some(creationHeight)))
  }

  def transitionBack(toHeight: Height): Option[TrackedBox] = creationHeightOpt match {
    case Some(h) if h < toHeight => Some(copy(creationHeightOpt = None))
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

  override def register(registry: Registry): Unit = {
    super.register(registry)
    registry.putToConfirmedIndex(spendingHeight, boxId)
  }

  def transition(spendingTransaction: ErgoTransaction, heightOpt: Option[Height]): Option[TrackedBox] = None

  def transition(creationHeight: Height): Option[TrackedBox] = None

  def transitionBack(toHeight: Height): Option[TrackedBox] = {
    (toHeight < spendingHeight, toHeight < creationHeight) match {
      case (false, false) => None
      case (true, false) => Some(UnspentOnchainBox(creationTx, creationOutIndex, creationHeight, box, certainty))
      case (true, true) => Some(UnspentOffchainBox(creationTx, creationOutIndex, box, certainty))
      case (false, true) => log.warn(s"Wrong heights. Spending: $spendingHeight, creation: $creationHeight"); None
    }
  }

  def makeCertain(): SpentOnchainBox = if (certain) this else copy(certainty = Certain)

}
