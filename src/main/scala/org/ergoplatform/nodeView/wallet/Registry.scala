package org.ergoplatform.nodeView.wallet

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.nodeView.history.ErgoHistory.Height

import scala.collection.mutable

object Registry {

  private val registry = mutable.Map[ByteArrayWrapper, TrackedBox]()

  //todo: build indexes instead of iteration
  def unspentBoxes: Iterator[UnspentBox] = registry.valuesIterator.flatMap { tb =>
    tb match {
      case ub: UnspentBox => Some(ub)
      case _ => None
    }
  }

  def uncertainBoxes: Iterator[UncertainBox] = registry.valuesIterator.flatMap { tb =>
    (tb match {
      case ub: UncertainBox => Some(ub)
      case _ => None
    }): Option[UncertainBox]
  }

  //todo: extract a random element, not head
  def nextUncertain(): Option[UncertainBox] = uncertainBoxes.toSeq.headOption

  def registryContains(boxId: ByteArrayWrapper): Boolean = registry.contains(boxId)

  def putToRegistry(trackedBox: TrackedBox) = synchronized {
    registry.put(trackedBox.boxId, trackedBox)
  }

  def removeFromRegistry(boxId: ByteArrayWrapper) = registry(boxId)

  private val confirmedIndex = mutable.TreeMap[Height, Seq[ByteArrayWrapper]]()

  def putToConfirmedIndex(height: Height, boxId: ByteArrayWrapper) = synchronized {
    confirmedIndex.put(height, confirmedIndex.getOrElse(height, Seq()) :+ boxId)
  }

  private var _confirmedBalance: Long = 0
  private val _confirmedAssetBalances: mutable.Map[ByteArrayWrapper, Long] = mutable.Map()

  private var _unconfirmedBalance: Long = 0
  private val _unconfirmedAssetBalances: mutable.Map[ByteArrayWrapper, Long] = mutable.Map()

  def confirmedBalance = _confirmedBalance
  def confirmedAssetBalances = _confirmedAssetBalances

  def unconfirmedBalance = _unconfirmedBalance
  def unconfirmedAssetBalances = _unconfirmedAssetBalances


  def increaseBalances(unspentBox: UnspentBox): Unit = synchronized {
    val box = unspentBox.box
    val tokenDelta = box.value
    val assetDeltas = box.additionalTokens

    //todo: reduce boilerplate below?
    if (unspentBox.onchain) {
      _confirmedBalance += tokenDelta
      assetDeltas.foreach { case (id, amount) =>
        val wid = ByteArrayWrapper(id)
        val updBalance = _confirmedAssetBalances.getOrElse(wid, 0L) + amount
        _confirmedAssetBalances.put(wid, updBalance)
      }
    } else {
      _unconfirmedBalance += tokenDelta
      assetDeltas.foreach { case (id, amount) =>
        val wid = ByteArrayWrapper(id)
        val updBalance = _unconfirmedAssetBalances.getOrElse(wid, 0L) + amount
        _unconfirmedAssetBalances.put(wid, updBalance)
      }
    }
  }

  def decreaseBalances(unspentBox: UnspentBox): Unit = synchronized {
    val box = unspentBox.box
    val tokenDelta = box.value
    val assetDeltas = box.additionalTokens

    //todo: reduce boilerplate below?
    if (unspentBox.onchain) {
      _confirmedBalance -= tokenDelta
      assetDeltas.foreach { case (id, amount) =>
        val wid = ByteArrayWrapper(id)
        val currentBalance = _confirmedAssetBalances.getOrElse(wid, 0L)
        if (currentBalance == amount) {
          _confirmedAssetBalances.remove(wid)
        } else {
          val updBalance = currentBalance - amount
          _confirmedAssetBalances.put(wid, updBalance)
        }
      }
    } else {
      _unconfirmedBalance -= tokenDelta
      assetDeltas.foreach { case (id, amount) =>
        val wid = ByteArrayWrapper(id)
        val currentBalance = _unconfirmedAssetBalances.getOrElse(wid, 0L)
        if (currentBalance == amount) {
          _unconfirmedAssetBalances.remove(wid)
        } else {
          val updBalance = currentBalance - amount
          _unconfirmedAssetBalances.put(wid, updBalance)
        }
      }
    }
  }

  def makeTransition(boxId: ByteArrayWrapper, transition: Transition): Unit = {
    makeTransition(registry(boxId), transition)
  }

  def makeTransition(trackedBox: TrackedBox, transition: Transition): Unit = {
    val transitionResult: Option[TrackedBox] = transition match {
      case ProcessRollback(toHeight) =>
        trackedBox.transitionBack(toHeight)
      case CreationConfirmation(creationHeight) =>
        trackedBox.transition(creationHeight)
      case ProcessSpending(spendingTransaction, spendingHeightOpt) =>
        trackedBox.transition(spendingTransaction, spendingHeightOpt)
    }
    transitionResult match {
      case Some(newTrackedBox) =>
        makeTransition(trackedBox, newTrackedBox)
      case None =>
    }
  }

  def makeTransition(oldTrackedBox: TrackedBox, newTrackedBox: TrackedBox): Unit = {
    newTrackedBox.register()
    oldTrackedBox.deregister()
  }
}
