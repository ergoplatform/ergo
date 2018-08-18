package org.ergoplatform.nodeView.wallet

import org.ergoplatform.nodeView.history.ErgoHistory.Height
import org.ergoplatform.settings.Constants
import scorex.core.{ModifierId, bytesToId}

import scala.collection.mutable


//todo: declare this class thread-unsafe?

class Registry {

  private val registry = mutable.Map[ModifierId, TrackedBox]()

  private val confirmedIndex = mutable.TreeMap[Height, Seq[ModifierId]]()

  private val unspentBoxes = mutable.TreeSet[ModifierId]()

  private val initialScanValue = bytesToId(Array.fill(Constants.ModifierIdSize)(0: Byte))
  private val uncertainBoxes = mutable.TreeSet[ModifierId]()
  private var lastScanned: ModifierId = initialScanValue

  def unspentBoxesIterator: Iterator[UnspentBox] =
    unspentBoxes.iterator.flatMap(id => registry.get(id).map(_.asInstanceOf[UnspentBox]))

  def nextUncertain(): Option[TrackedBox] = synchronized {
      uncertainBoxes.from(lastScanned).headOption match {
        case Some(id) =>
          lastScanned = id
          registry.get(id)
        case None =>
          lastScanned = initialScanValue
          None
      }
  }

  def uncertainExists: Boolean = uncertainBoxes.nonEmpty

  def putToRegistry(trackedBox: TrackedBox): Option[TrackedBox] = synchronized {
    if (!trackedBox.certain) uncertainBoxes += trackedBox.boxId
    if (trackedBox.isInstanceOf[UnspentBox]) unspentBoxes += trackedBox.boxId
    registry.put(trackedBox.boxId, trackedBox)
  }

  def removeFromRegistry(boxId: ModifierId): Option[TrackedBox] = synchronized {
    registry.remove(boxId).map { trackedBox: TrackedBox =>
      if (!trackedBox.certain) uncertainBoxes -= trackedBox.boxId
      if (trackedBox.isInstanceOf[UnspentBox]) unspentBoxes -= trackedBox.boxId
      trackedBox
    }
  }

  def registryContains(boxId: ModifierId): Boolean = synchronized {
    registry.contains(boxId)
  }

  def putToConfirmedIndex(height: Height, boxId: ModifierId): Unit = synchronized {
    confirmedIndex.put(height, confirmedIndex.getOrElse(height, Seq.empty) :+ boxId)
  }

  def confirmedAt(height: Height): Seq[ModifierId] = synchronized {
    confirmedIndex.getOrElse(height, Seq.empty)
  }

  private var _confirmedBalance: Long = 0
  private val _confirmedAssetBalances: mutable.Map[ModifierId, Long] = mutable.Map()

  private var _unconfirmedBalance: Long = 0
  private val _unconfirmedAssetBalances: mutable.Map[ModifierId, Long] = mutable.Map()

  def confirmedBalance: Long = _confirmedBalance

  def confirmedAssetBalances: scala.collection.Map[ModifierId, Long] = _confirmedAssetBalances

  def unconfirmedBalance: Long = _unconfirmedBalance

  def unconfirmedAssetBalances: scala.collection.Map[ModifierId, Long] = _unconfirmedAssetBalances

  def increaseBalances(unspentBox: UnspentBox): Unit = synchronized {
    val box = unspentBox.box
    val tokenDelta = box.value
    val assetDeltas = box.additionalTokens

    //todo: reduce boilerplate below?
    if (unspentBox.onchain) {
      _confirmedBalance += tokenDelta
      assetDeltas.foreach { case (id, amount) =>
        val wid = bytesToId(id)
        val updBalance = _confirmedAssetBalances.getOrElse(wid, 0L) + amount
        _confirmedAssetBalances.put(wid, updBalance)
      }
    } else { //offchain box case
      _unconfirmedBalance += tokenDelta
      assetDeltas.foreach { case (id, amount) =>
        val wid = bytesToId(id)
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
        val wid = bytesToId(id)
        val currentBalance = _confirmedAssetBalances.getOrElse(wid, 0L)
        if (currentBalance == amount) {
          _confirmedAssetBalances.remove(wid)
        } else {
          val updBalance = currentBalance - amount
          _confirmedAssetBalances.put(wid, updBalance)
        }
      }
    } else { //offchain box case
      _unconfirmedBalance -= tokenDelta
      assetDeltas.foreach { case (id, amount) =>
        val wid = bytesToId(id)
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

  def makeTransition(boxId: ModifierId, transition: Transition): Unit = {
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
    oldTrackedBox.deregister(this)
    newTrackedBox.register(this)
  }
}