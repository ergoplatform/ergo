package org.ergoplatform.nodeView.wallet

import org.ergoplatform.nodeView.history.ErgoHistory.Height
import org.ergoplatform.settings.Constants
import scorex.core.{ModifierId, bytesToId}

import scala.collection.mutable

/**
  * Storage class for wallet entities:
  *   - all the boxes which are potentially belong to the user,
  *   - index for boxes which could belong to the user (it contains needed bytes, for example, public key bytes),
  *     but that is to be carefully checked yet (by successfully signing a test transaction which is spending the box).
  *   - index for unspent boxes
  *   - confirmed and unconfirmed balances
  *
  * This class is not thread-safe.
  */
class WalletStorage {

  private val registry = mutable.Map[ModifierId, TrackedBox]()

  private val confirmedIndex = mutable.TreeMap[Height, Seq[ModifierId]]()

  private val unspentBoxes = mutable.TreeSet[ModifierId]()

  private val initialScanValue = bytesToId(Array.fill(Constants.ModifierIdSize)(0: Byte))
  private val uncertainBoxes = mutable.TreeSet[ModifierId]()
  private var lastScanned: ModifierId = initialScanValue

  def unspentBoxesIterator: Iterator[UnspentBox] =
    unspentBoxes.iterator.flatMap(id => registry.get(id).map(_.asInstanceOf[UnspentBox]))

  def nextUncertain(): Option[TrackedBox] = {
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

  def putToRegistry(trackedBox: TrackedBox): Option[TrackedBox] = {
    if (!trackedBox.certain) uncertainBoxes += trackedBox.boxId
    if (trackedBox.isInstanceOf[UnspentBox]) unspentBoxes += trackedBox.boxId
    registry.put(trackedBox.boxId, trackedBox)
  }

  def removeFromRegistry(boxId: ModifierId): Option[TrackedBox] = {
    registry.remove(boxId).map { trackedBox: TrackedBox =>
      if (!trackedBox.certain) uncertainBoxes -= trackedBox.boxId
      if (trackedBox.isInstanceOf[UnspentBox]) unspentBoxes -= trackedBox.boxId
      trackedBox
    }
  }

  def registryContains(boxId: ModifierId): Boolean = {
    registry.contains(boxId)
  }

  def putToConfirmedIndex(height: Height, boxId: ModifierId): Unit = {
    confirmedIndex.put(height, confirmedIndex.getOrElse(height, Seq.empty) :+ boxId)
  }

  def confirmedAt(height: Height): Seq[ModifierId] = {
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

  def increaseBalances(unspentBox: UnspentBox): Unit = {
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

  def decreaseBalances(unspentBox: UnspentBox): Unit = {
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