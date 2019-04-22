package org.ergoplatform.nodeView.wallet

import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import org.ergoplatform.settings.Constants
import org.ergoplatform.wallet.boxes.BoxCertainty.{Certain, Uncertain}
import org.ergoplatform.wallet.boxes.ChainStatus.{Fork, MainChain}
import org.ergoplatform.wallet.boxes.SpendingStatus.{Spent, Unspent}
import org.ergoplatform.wallet.boxes.{ChainStatus, TrackedBox}
import scorex.util._

import scala.collection.{immutable, mutable}

/**
  * Storage class for wallet entities:
  *   - all the boxes which are potentially belong to the user,
  *   - index for boxes which could belong to the user (it contains needed bytes, for example, public key bytes),
  * but that is to be carefully checked yet (by successfully signing a test transaction which is spending the box).
  *   - index for unspent boxes
  *   - confirmed and unconfirmed balances
  *
  * This class is not thread-safe.
  */
class WalletStorage extends ScorexLogging {

  private val registry = mutable.Map[ModifierId, TrackedBox]()

  private val confirmedIndex = mutable.TreeMap[Height, Set[ModifierId]]()

  private val unspentBoxes = mutable.TreeSet[ModifierId]()

  private val initialScanValue = bytesToId(Array.fill(Constants.ModifierIdSize)(0: Byte))
  private val uncertainBoxes = mutable.TreeSet[ModifierId]()
  private var lastScanned: ModifierId = initialScanValue

  def unspentCertainBoxesIterator: Iterator[TrackedBox] =
    unspentBoxes.iterator.flatMap(id => registry.get(id))

  def nextUncertain(): Option[TrackedBox] = {
    uncertainBoxes.from(lastScanned).headOption match {
      case Some(id) =>
        lastScanned = bytesToId((BigInt(idToBytes(id)) + 1).toByteArray)
        registry.get(id)
      case None =>
        lastScanned = initialScanValue
        None
    }
  }

  def uncertainExists: Boolean = uncertainBoxes.nonEmpty

  private def put(trackedBox: TrackedBox): Option[TrackedBox] = {
    if (trackedBox.certainty == Uncertain) {
      uncertainBoxes += trackedBox.boxId
    } else if (trackedBox.spendingStatus == Unspent) {
      unspentBoxes += trackedBox.boxId
    }
    registry.put(trackedBox.boxId, trackedBox)
  }

  private def remove(boxId: ModifierId): Option[TrackedBox] = {
    registry.remove(boxId).map { trackedBox =>
      if (trackedBox.certainty == Uncertain) uncertainBoxes.remove(boxId)
      if (trackedBox.spendingStatus == Unspent) unspentBoxes.remove(boxId)
      trackedBox
    }
  }

  def byId(boxId: ModifierId): Option[TrackedBox] = {
    registry.get(boxId)
  }

  def contains(boxId: ModifierId): Boolean = {
    registry.contains(boxId)
  }

  private def putToConfirmedIndex(height: Height, boxId: ModifierId): Unit = {
    confirmedIndex.put(height, confirmedIndex.getOrElse(height, Set.empty) + boxId)
  }

  def confirmedAt(height: Height): Set[ModifierId] = {
    confirmedIndex.getOrElse(height, Set.empty)
  }

  private var _confirmedBalance: Long = 0
  private val _confirmedAssetBalances: mutable.Map[ModifierId, Long] = mutable.Map()

  private var _unconfirmedDelta: Long = 0
  private val _unconfirmedAssetDeltas: mutable.Map[ModifierId, Long] = mutable.Map()

  def confirmedBalance: Long = _confirmedBalance

  def confirmedAssetBalances: immutable.Map[ModifierId, Long] = _confirmedAssetBalances.toMap

  def balancesWithUnconfirmed: Long = _confirmedBalance + _unconfirmedDelta

  def assetBalancesWithUnconfirmed: immutable.Map[ModifierId, Long] = {
    val unconfirmedSums = _unconfirmedAssetDeltas.toMap map { case (id, value) =>
      val newValue = confirmedAssetBalances.getOrElse(id, 0L) + value
      id -> newValue
    }
    val confirmedAddition = _confirmedAssetBalances -- _unconfirmedAssetDeltas.keys
    unconfirmedSums.filterNot(_._2 == 0L) ++ confirmedAddition
  }

  private def undoBalances(trackedBox: TrackedBox): Unit = updateBalances(trackedBox, undo = true)

  private def updateBalances(trackedBox: TrackedBox, undo: Boolean = false): Unit = {
    if (trackedBox.certainty == Certain) {
      if (trackedBox.spendingStatus == Unspent) {
        increaseBalances(trackedBox.creationChainStatus, trackedBox.box, undo)
        log.debug(s"${if (undo) "Undo" else "Update"} balance with UNSPENT ${trackedBox.chainStatus} " +
                  s"${trackedBox.boxId} $trackedBox balance: $confirmedBalance $confirmedAssetBalances, " +
                  s"total: $balancesWithUnconfirmed $assetBalancesWithUnconfirmed")
      } else if (trackedBox.creationChainStatus == MainChain && trackedBox.spendingChainStatus == Fork) {
        increaseBalances(MainChain, trackedBox.box, undo)
        decreaseBalances(Fork, trackedBox.box, undo)
        log.debug(s"${if (undo) "Undo" else "Update"} balance with OFF-CHAIN SPENT ${trackedBox.boxId} " +
                  s"balance: $confirmedBalance, total: $balancesWithUnconfirmed")
      } else {
        log.debug(s"${if (undo) "Ignore undo" else "Ignore"} balances for SPENT ${trackedBox.boxId} $trackedBox")
      }
    }
  }

  private def increaseBalances(balanceStatus: ChainStatus, box: ErgoBox, undo: Boolean): Unit = {
    if (undo) decreaseBalances(balanceStatus, box) else increaseBalances(balanceStatus, box)
  }

  private def increaseBalances(balanceStatus: ChainStatus, box: ErgoBox): Unit = {
    if (balanceStatus == MainChain) _confirmedBalance += box.value else _unconfirmedDelta += box.value
    val balanceMap = if (balanceStatus == MainChain) _confirmedAssetBalances else _unconfirmedAssetDeltas
    box.additionalTokens foreach { case (tokenId, amount) =>
      val assetId = bytesToId(tokenId)
      val updBalance = balanceMap.getOrElse(assetId, 0L) + amount
      if (updBalance == 0L) {
        balanceMap.remove(assetId)
      } else {
        balanceMap.put(assetId, updBalance)
      }
    }
  }

  private def decreaseBalances(balanceStatus: ChainStatus, box: ErgoBox, undo: Boolean): Unit = {
    if (undo) increaseBalances(balanceStatus, box) else decreaseBalances(balanceStatus, box)
  }

  private def decreaseBalances(balanceStatus: ChainStatus, box: ErgoBox): Unit = {
    if (balanceStatus == MainChain) _confirmedBalance -= box.value else _unconfirmedDelta -= box.value
    val balanceMap = if (balanceStatus == MainChain) _confirmedAssetBalances else _unconfirmedAssetDeltas
    box.additionalTokens foreach { case (tokenId, amount) =>
      val assetId = bytesToId(tokenId)
      val currentBalance = balanceMap.getOrElse(assetId, 0L)
      if (currentBalance == amount) {
        balanceMap.remove(assetId)
      } else {
        val updBalance = currentBalance - amount
        balanceMap.put(assetId, updBalance)
      }
    }
  }

  def makeTransition(boxId: ModifierId, transition: Transition): Boolean = {
    registry.get(boxId).exists { trackedBox =>
      val targetBox: Option[TrackedBox] = convertBox(trackedBox, transition)
      targetBox.foreach(register)
      targetBox.nonEmpty
    }
  }

  def convertBox(trackedBox: TrackedBox, transition: Transition): Option[TrackedBox] = {
    transition match {
      case ProcessRollback(toHeight) =>
        convertBack(trackedBox, toHeight)
      case CreationConfirmation(creationHeight) =>
        convertToConfirmed(trackedBox, creationHeight)
      case ProcessSpending(spendingTransaction, spendingHeightOpt) =>
        convertToSpent(trackedBox, spendingTransaction, spendingHeightOpt)
      case MakeCertain =>
        convertToCertain(trackedBox)
    }
  }

  /**
    * Register tracked box in a wallet storage
    */
  def register(trackedBox: TrackedBox): Unit = {
    deregister(trackedBox.boxId) // we need to decrease balances if somebody registers box that already known
    put(trackedBox)
    if (trackedBox.spendingStatus == Unspent) {
      log.debug(s"New ${trackedBox.chainStatus} ${trackedBox.certainty} box arrived: " + trackedBox)
    }
    trackedBox.inclusionHeightOpt.foreach(h => putToConfirmedIndex(h, trackedBox.boxId))
    trackedBox.spendingHeightOpt.foreach(h => putToConfirmedIndex(h, trackedBox.boxId))
    updateBalances(trackedBox)
  }

  /**
    * Remove tracked box from a wallet storage
    */
  def deregister(boxId: ModifierId): Option[TrackedBox] = {
    val removedBox = remove(boxId)
    removedBox.foreach(undoBalances)
    removedBox
  }

  /**
    * Do tracked box state transition on a spending transaction (confirmed or not to come)
    *
    * @return Some(trackedBox), if box state has been changed, None otherwise
    */
  private def convertToSpent(trackedBox: TrackedBox,
                             spendingTransaction: ErgoTransaction,
                             spendingHeightOpt: Option[Height]): Option[TrackedBox] = {
    (trackedBox.spendingStatus, trackedBox.chainStatus) match {
      case _ if spendingHeightOpt.nonEmpty && trackedBox.inclusionHeightOpt.isEmpty =>
        log.error(s"Invalid state transition for ${trackedBox.boxId}: no creation height, but spent on-chain")
        None
      case (Unspent, Fork) if spendingHeightOpt.nonEmpty =>
        log.warn(s"Onchain transaction ${trackedBox.spendingTxIdOpt} is spending offchain box ${trackedBox.box}")
        None
      case (Spent, Fork) if spendingHeightOpt.isEmpty =>
        log.warn(s"Double spending of an unconfirmed box ${trackedBox.boxId}")
        //todo: handle double-spending strategy for an unconfirmed tx
        None
      case (Spent, MainChain) =>
        None
      case _ =>
        Some(trackedBox.copy(spendingTxIdOpt = Some(spendingTransaction.id), spendingHeightOpt = spendingHeightOpt))
    }
  }

  /**
    * Do tracked box state transition on a creating transaction got confirmed
    *
    * @return Some(trackedBox), if box state has been changed, None otherwise
    */
  private def convertToConfirmed(trackedBox: TrackedBox, creationHeight: Height): Option[TrackedBox] = {
    if (trackedBox.inclusionHeightOpt.isEmpty) {
      Some(trackedBox.copy(inclusionHeightOpt = Option(creationHeight)))
    } else {
      if (trackedBox.spendingStatus == Unspent || trackedBox.chainStatus == Fork) {
        log.warn(s"Double creation of tracked box for  ${trackedBox.boxId}")
      }
      None
    }
  }

  /**
    * Do tracked box state transition on a rollback to a certain height
    *
    * @param toHeight - height to roll back to
    * @return Some(trackedBox), if box state has been changed, None otherwise
    */
  private def convertBack(trackedBox: TrackedBox, toHeight: Height): Option[TrackedBox] = {
    val dropCreation = trackedBox.inclusionHeightOpt.exists(toHeight < _)
    val dropSpending = trackedBox.spendingHeightOpt.exists(toHeight < _)

    if (dropCreation && dropSpending) {
      Some(trackedBox.copy(inclusionHeightOpt = None, spendingHeightOpt = None))
    } else if (dropCreation) {
      Some(trackedBox.copy(inclusionHeightOpt = None))
    } else if (dropSpending) {
      Some(trackedBox.copy(spendingHeightOpt = None))
    } else {
      None
    }
  }

  /**
    * Handle a command to make this box "certain" (definitely hold by the user)
    *
    * @return updated box or None
    */
  def convertToCertain(trackedBox: TrackedBox): Option[TrackedBox] = {
    if (trackedBox.certainty == Certain) None else Some(trackedBox.copy(certainty = Certain))
  }

}
