package org.ergoplatform.nodeView.wallet

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import scorex.core.utils.ScorexLogging

sealed trait TrackedBox extends ScorexLogging {
  val creationTx: ErgoTransaction
  val creationOutIndex: Short

  val onchain: Boolean

  val box: ErgoBox

  lazy val value = box.value
  lazy val assets = box.additionalTokens.map { case (id, amt) =>
    ByteArrayWrapper(id) -> amt
  }.toMap


  lazy val boxId = ByteArrayWrapper(box.id)

  def register(): Unit = Registry.putToRegistry(this)

  def deregister(): Unit = Registry.removeFromRegistry(boxId)

  def transition(spendingTransaction: ErgoTransaction, spendingHeightOpt: Option[Height]): Option[TrackedBox]

  def transition(creationHeight: Height): Option[TrackedBox]

  def transitionBack(toHeight: Int): Option[TrackedBox]
}



trait UncertainBox extends TrackedBox {
  def makeCertain(): TrackedBox
}

trait CertainBox extends TrackedBox

trait UnspentBox extends TrackedBox

trait SpentBox extends TrackedBox {
  val spendingTx: ErgoTransaction
}

trait OffchainBox extends TrackedBox {
  override val onchain = false
}

trait OnchainBox extends TrackedBox {
  override val onchain = true
}


trait UnspentOffchainTrackedBox extends UnspentBox with OffchainBox {
  override def register(): Unit = {
    super.register()
    log.info("New offchain box arrived: " + this)
    if (this.isInstanceOf[CertainBox]) Registry.increaseBalances(this)
  }

  override def deregister(): Unit = {
    super.deregister()
    if (this.isInstanceOf[CertainBox]) Registry.decreaseBalances(this)
  }
}

trait UnspentOnchainTrackedBox extends UnspentBox with OnchainBox {
  val creationHeight: Int

  override def register(): Unit = {
    super.register()
    log.info("New onchain box arrived: " + this)
    Registry.putToConfirmedIndex(creationHeight, boxId)
    if (this.isInstanceOf[CertainBox]) Registry.increaseBalances(this)
  }

  override def deregister(): Unit = {
    super.deregister()
    if (this.isInstanceOf[CertainBox]) Registry.decreaseBalances(this)
  }
}

trait SpentOffchainTrackedBox extends SpentBox with OffchainBox {
  val creationHeight: Option[Int]
}

trait SpentOnchainTrackedBox extends SpentBox with OnchainBox {
  val creationHeight: Int
  val spendingHeight: Int


  override def register(): Unit = {
    super.register()
    Registry.putToConfirmedIndex(spendingHeight, boxId)
  }
}


case class UncertainUnspentOffchainBox(override val creationTx: ErgoTransaction,
                                       override val creationOutIndex: Short,
                                       override val box: ErgoBox) extends UncertainBox with UnspentOffchainTrackedBox {
  override def transition(creationHeight: Height): Option[TrackedBox] =
    Some(UncertainUnspentOnchainBox(creationTx, creationOutIndex, creationHeight, box))

  override def transition(spendingTransaction: ErgoTransaction, heightOpt: Option[Height]): Option[TrackedBox] = {
    heightOpt match {
      case Some(h) => log.warn(s"Onchain transaction ${spendingTransaction.id} is spending offchain box $box"); None
      case None => Some(UncertainSpentOffchainBox(creationTx, creationOutIndex, None, spendingTransaction, box))
    }
  }

  override def transitionBack(toHeight: Int): Option[TrackedBox] = None

  override def makeCertain(): TrackedBox = UnspentOffchainBox(creationTx, creationOutIndex, box)
}

case class UncertainUnspentOnchainBox(override val creationTx: ErgoTransaction,
                                      override val creationOutIndex: Short,
                                      override val creationHeight: Int,
                                      override val box: ErgoBox) extends UncertainBox with UnspentOnchainTrackedBox {

  def transition(spendingTransaction: ErgoTransaction, heightOpt: Option[Height]): Option[TrackedBox] = {
    Some(heightOpt match {
      case Some(h) =>
        UncertainSpentOnchainBox(creationTx, creationOutIndex, creationHeight, spendingTransaction, h, box)
      case None =>
        UncertainSpentOffchainBox(creationTx, creationOutIndex, Some(creationHeight), spendingTransaction, box)
    })
  }

  def transition(creationHeight: Height): Option[TrackedBox] = {
    log.warn(s"Double creation of UncertainUnspentOnchainBox for $boxId")
    None
  }

  def transitionBack(toHeight: Int): Option[TrackedBox] = {
    if (creationHeight > toHeight) {
      Some(UncertainUnspentOffchainBox(creationTx, creationOutIndex, box))
    } else None
  }

  override def makeCertain(): TrackedBox = {
    UnspentOnchainBox(creationTx, creationOutIndex, creationHeight, box)
  }
}

case class UncertainSpentOffchainBox(override val creationTx: ErgoTransaction,
                                     override val creationOutIndex: Short,
                                     override val creationHeight: Option[Int],
                                     override val spendingTx: ErgoTransaction,
                                     override val box: ErgoBox) extends UncertainBox with SpentOffchainTrackedBox {

  def transition(spendingTransaction: ErgoTransaction, heightOpt: Option[Height]): Option[TrackedBox] = {
    heightOpt match {
      case Some(h) =>
        require(creationHeight.isDefined)
        Some(UncertainSpentOnchainBox(creationTx, creationOutIndex, creationHeight.get, spendingTransaction, h, box))
      case None =>
        log.warn(s"Double spending of an unconfirmed box $boxId")
        //todo: handle double-spending strategy for an unconfirmed tx
        None
    }
  }

  def transition(creationHeight: Height): Option[TrackedBox] = this.creationHeight match {
    case Some(_) => log.warn(s"Double creation of $boxId"); None
    case None => Some(this.copy(creationHeight = Some(creationHeight)))
  }

  def transitionBack(toHeight: Int): Option[TrackedBox] = creationHeight match {
    case Some(h) if h < toHeight => Some(UncertainSpentOffchainBox(creationTx, creationOutIndex, None, spendingTx, box))
    case _ => None
  }

  override def makeCertain(): TrackedBox = {
    SpentOffchainBox(creationTx, creationOutIndex, creationHeight, spendingTx, box)
  }
}

case class UncertainSpentOnchainBox(override val creationTx: ErgoTransaction,
                                    override val creationOutIndex: Short,
                                    override val creationHeight: Int,
                                    override val spendingTx: ErgoTransaction,
                                    override val spendingHeight: Int,
                                    override val box: ErgoBox) extends UncertainBox with SpentOnchainTrackedBox {
  def transition(spendingTransaction: ErgoTransaction, spendingHeightOpt: Option[Height]): Option[TrackedBox] = None

  def transition(creationHeight: Height): Option[TrackedBox] = None

  def transitionBack(toHeight: Int): Option[TrackedBox] = (toHeight < spendingHeight, toHeight < creationHeight) match {
    case (false, false) => None
    case (true, false) => Some(UncertainUnspentOnchainBox(creationTx, creationOutIndex, creationHeight, box))
    case (true, true) => Some(UncertainUnspentOffchainBox(creationTx, creationOutIndex, box))
    case (false, true) => log.warn("Wrong state"); None
  }

  override def makeCertain(): TrackedBox =
    SpentOnchainBox(creationTx, creationOutIndex, creationHeight, spendingTx, spendingHeight, box)
}


case class UnspentOffchainBox(override val creationTx: ErgoTransaction,
                              override val creationOutIndex: Short,
                              override val box: ErgoBox) extends UnspentOffchainTrackedBox with CertainBox {

  override def transition(creationHeight: Height): Option[TrackedBox] =
    Some(UnspentOnchainBox(creationTx, creationOutIndex, creationHeight, box))

  override def transition(spendingTransaction: ErgoTransaction, heightOpt: Option[Height]): Option[TrackedBox] = {
    heightOpt match {
      case Some(h) => log.warn(s"Onchain transaction ${spendingTransaction.id} is spending offchain box $box"); None
      case None => Some(SpentOffchainBox(creationTx, creationOutIndex, None, spendingTransaction, box))
    }
  }

  override def transitionBack(toHeight: Int): Option[TrackedBox] = None
}

case class UnspentOnchainBox(override val creationTx: ErgoTransaction,
                             override val creationOutIndex: Short,
                             override val creationHeight: Int,
                             override val box: ErgoBox) extends UnspentOnchainTrackedBox with CertainBox {

  def transition(spendingTransaction: ErgoTransaction, heightOpt: Option[Height]): Option[TrackedBox] = {
    Some(heightOpt match {
      case Some(h) =>
        SpentOnchainBox(creationTx, creationOutIndex, creationHeight, spendingTransaction, h, box)
      case None =>
        SpentOffchainBox(creationTx, creationOutIndex, Some(creationHeight), spendingTransaction, box)
    })
  }

  def transition(creationHeight: Height): Option[TrackedBox] = {
    log.warn(s"Double creation of UncertainUnspentOnchainBox for $boxId")
    None
  }

  def transitionBack(toHeight: Int): Option[TrackedBox] = {
    if (creationHeight > toHeight) {
      Some(UnspentOffchainBox(creationTx, creationOutIndex, box))
    } else None
  }
}

case class SpentOffchainBox(override val creationTx: ErgoTransaction,
                            override val creationOutIndex: Short,
                            override val creationHeight: Option[Int],
                            override val spendingTx: ErgoTransaction,
                            override val box: ErgoBox) extends SpentOffchainTrackedBox with CertainBox {

  def transition(spendingTransaction: ErgoTransaction, heightOpt: Option[Height]): Option[TrackedBox] = {
    heightOpt match {
      case Some(h) =>
        require(creationHeight.isDefined)
        Some(SpentOnchainBox(creationTx, creationOutIndex, creationHeight.get, spendingTransaction, h, box))
      case None =>
        log.warn(s"Double spending of an unconfirmed box $boxId")
        //todo: handle double-spending strategy for an unconfirmed tx
        None
    }
  }

  def transition(creationHeight: Height): Option[TrackedBox] = this.creationHeight match {
    case Some(_) => log.warn(s"Double creation of $boxId"); None
    case None => Some(this.copy(creationHeight = Some(creationHeight)))
  }

  def transitionBack(toHeight: Int): Option[TrackedBox] = creationHeight match {
    case Some(h) if h < toHeight => Some(SpentOffchainBox(creationTx, creationOutIndex, None, spendingTx, box))
    case _ => None
  }
}

case class SpentOnchainBox(override val creationTx: ErgoTransaction,
                           override val creationOutIndex: Short,
                           override val creationHeight: Int,
                           override val spendingTx: ErgoTransaction,
                           override val spendingHeight: Int,
                           override val box: ErgoBox) extends SpentOnchainTrackedBox with CertainBox {

  def transition(spendingTransaction: ErgoTransaction, spendingHeightOpt: Option[Height]): Option[TrackedBox] = None

  def transition(creationHeight: Height): Option[TrackedBox] = None

  def transitionBack(toHeight: Int): Option[TrackedBox] = (toHeight < spendingHeight, toHeight < creationHeight) match {
    case (false, false) => None
    case (true, false) => Some(UncertainUnspentOnchainBox(creationTx, creationOutIndex, creationHeight, box))
    case (true, true) => Some(UncertainUnspentOffchainBox(creationTx, creationOutIndex, box))
    case (false, true) => log.warn("Wrong state"); None
  }
}