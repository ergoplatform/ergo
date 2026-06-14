package org.ergoplatform.nodeView.wallet

import com.google.common.hash.BloomFilter
import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform._
import org.ergoplatform.nodeView.history.ErgoHistoryUtils.Height
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.state.{ErgoStateContext, ErgoStateReader, UtxoStateReader}
import org.ergoplatform.nodeView.wallet.ErgoWalletState.FilterFn
import org.ergoplatform.nodeView.wallet.persistence.{Balance, WalletDigest, WalletRegistry, WalletStorage}
import org.ergoplatform.settings.{ErgoSettings, Parameters}
import org.ergoplatform.wallet.Constants.{PaymentsScanId, ScanId}
import org.ergoplatform.wallet.boxes.{BoxSelector, TrackedBox}
import org.ergoplatform.wallet.secrets.JsonSecretStorage
import scorex.util.{ModifierId, ScorexLogging, bytesToId}

import scala.collection.mutable
import scala.util.Try

case class ErgoWalletState(
    storage: WalletStorage,
    secretStorageOpt: Option[JsonSecretStorage],
    registry: WalletRegistry,
    outputsFilter: Option[BloomFilter[Array[Byte]]], // Bloom filter for boxes not being spent to the moment
    walletVars: WalletVars,
    stateReaderOpt: Option[ErgoStateReader],
    mempoolReaderOpt: Option[ErgoMemPoolReader],
    utxoStateReaderOpt: Option[UtxoStateReader],
    parameters: Parameters,
    maxInputsToUse: Int,
    error: Option[String] = None,
    rescanInProgress: Boolean
  ) extends ScorexLogging {

  /**
    * Ids of boxes spent by inputs of current mempool transactions.
    */
  lazy val mempoolSpentIds: Set[ModifierId] = mempoolReaderOpt match {
    case Some(mr) => mr.spentInputs.map(bytesToId).toSet
    case None => Set.empty
  }

  /**
    * Wallet- and external-scan outputs created by current mempool transactions, excluding boxes that
    * are already confirmed on-chain (so a transaction being included in a block is not counted both as
    * confirmed and off-chain). Boxes already spent by other mempool transactions are retained here,
    * because an external scan may still want to list them (`removeOffchain = false`); each consumer
    * decides whether spent boxes are visible.
    * Computed on demand from the mempool, so it never gets stale when transactions leave the pool.
    */
  lazy val rawOffChainBoxes: Seq[TrackedBox] = mempoolReaderOpt match {
    case Some(mr) =>
      val dustLimit = walletVars.settings.walletSettings.dustLimit
      mr.getAllPrioritized
        .flatMap { u =>
          WalletScanLogic.extractWalletOutputs(u.transaction, None, walletVars, dustLimit)
        }
        .filterNot(confirmedOnChain)
        .distinct
    case None => Seq.empty
  }

  // A mempool output is no longer off-chain once its creating transaction is confirmed. The box may
  // already be gone from the registry (spent boxes are pruned unless keepSpentBoxes is set), so the
  // creating transaction is checked too, to avoid resurrecting an already-spent output from a stale
  // mempool snapshot while blocks are being applied.
  private def confirmedOnChain(tb: TrackedBox): Boolean =
    registry.getBox(tb.box.id).isDefined || registry.getTx(tb.creationTxId).isDefined

  /**
    * Off-chain boxes the wallet can spend: raw off-chain boxes minus those already spent by mempool
    * transactions. Used for the wallet balance and box selection, where a spent box must never count,
    * regardless of any scan's `removeOffchain` policy.
    */
  lazy val offChainBoxes: Seq[TrackedBox] =
    rawOffChainBoxes.filterNot(tb => mempoolSpentIds.contains(tb.boxId))

  /**
    * Whether a scan keeps its boxes visible after they are spent off-chain. Only external scans can
    * opt in (`removeOffchain = false`); the payment wallet always drops boxes spent in the mempool.
    */
  def keepsSpentOffChain(scanId: ScanId): Boolean =
    walletVars.externalScans.exists(s => s.scanId == scanId && !s.removeOffchain)

  /**
    * Wallet balance snapshot including unconfirmed transactions, computed on demand from the mempool.
    * The confirmed balance is taken from the O(1) registry digest and adjusted by the mempool delta:
    * subtract confirmed wallet boxes spent by mempool transactions, add the off-chain boxes they
    * create. This avoids scanning the whole wallet UTXO set on every balance read.
    */
  lazy val offChainDigest: WalletDigest = {
    val confirmed = registry.fetchDigest()
    // confirmed wallet boxes that mempool transactions spend (still unspent on-chain)
    val spent: Seq[Balance] = mempoolReaderOpt.toSeq
      .flatMap(_.spentInputs.flatMap(registry.getBox))
      .filter(tb => tb.spendingHeightOpt.isEmpty && tb.scans.contains(PaymentsScanId))
      .map(Balance.apply)
    // wallet balance counts payment boxes only, matching the confirmed digest; external-scan-only
    // boxes are tracked through the scan APIs, not the wallet balance
    val created: Seq[Balance] =
      offChainBoxes.filter(_.scans.contains(PaymentsScanId)).map(Balance.apply)

    val tokens = mutable.LinkedHashMap.empty[ModifierId, Long]
    def merge(assets: Seq[(ModifierId, Long)], sign: Long): Unit =
      assets.foreach { case (id, amt) => tokens += id -> (tokens.getOrElse(id, 0L) + sign * amt) }
    merge(confirmed.walletAssetBalances, 1)
    spent.foreach(b => merge(b.assets.toSeq, -1))
    created.foreach(b => merge(b.assets.toSeq, 1))

    val balance = confirmed.walletBalance - spent.map(_.value).sum + created.map(_.value).sum
    WalletDigest(confirmed.height, balance, tokens.toSeq.filter(_._2 != 0L))
  }

  /**
    * This filter selects boxes which are on-chain and not spent off-chain yet, or created off-chain.
    * This filter is used when the wallet is going through its boxes to assemble a transaction.
    */
  val walletFilter: FilterFn = (trackedBox: TrackedBox) => {
    val bid = trackedBox.box.id

    // box is not spent yet by inputs of mempool transactions
    def notInInputs: Boolean = !mempoolSpentIds.contains(bytesToId(bid))

    // box exists in UTXO set or in outputs of an off-chain transaction
    def inOutputs: Boolean = {
      utxoStateReaderOpt.forall { utxo =>
        utxo.boxById(bid).isDefined
      }
    }

    notInInputs && inOutputs
  }

  // Secret is set in form of keystore file of testMnemonic in the config
  def secretIsSet(testMnemonic: Option[String]): Boolean = secretStorageOpt.nonEmpty || testMnemonic.nonEmpty

  // State context used to sign transactions and check that coins found in the blockchain are indeed belonging
  // to the wallet (by executing testing transactions against them).
  // The state context is being updated by listening to state updates.
  def stateContext: ErgoStateContext = storage.getStateContext(parameters)

  /**
    * @return height of the last block scanned by the wallet
    */
  def getWalletHeight: Int = registry.fetchDigest().height

  /**
    * @return Height of the chain as reported by the state (i.e. height of a last block applied to the state, not the wallet). Wallet's height may be behind it.
    */
  def fullHeight: Int = stateContext.currentHeight

  def getChangeAddress(addrEncoder: ErgoAddressEncoder): Option[P2PKAddress] = {
    walletVars.proverOpt.map { prover =>
      storage.readChangeAddress.getOrElse {
        log.debug("Change address not specified. Using root address from wallet.")
        P2PKAddress(prover.hdPubKeys.head.key)(addrEncoder)
      }
    }
  }

  // Read a box from UTXO set if the node has it, otherwise, from the wallet
  def readBoxFromUtxoWithWalletFallback(boxId: BoxId): Option[ErgoBox] = {
    utxoStateReaderOpt match {
      case Some(utxoReader) =>
        utxoReader.boxById(boxId)
      case None =>
        registry.getBox(boxId).map(_.box)
    }
  }

  // expected height of a next block when the wallet is receiving a new block with the height blockHeight
  def expectedNextBlockHeight(blockHeight: Height, isFullBlocksPruned: Boolean): Height = {
    val walletHeight = getWalletHeight
    if (!isFullBlocksPruned) {
      // Node has all the full blocks and applies them sequentially
      walletHeight + 1
    } else {
      // Node has pruned blockchain
      if (walletHeight == 0) {
        blockHeight // todo: should be height of first non-pruned block
      } else {
        walletHeight + 1
      }
    }
  }

  /**
    * A helper method that returns unspent boxes
    */
  def getBoxesToSpend: Seq[TrackedBox] = {
    require(walletVars.publicKeyAddresses.nonEmpty, "No public keys in the prover to extract change address from")
    (registry.walletUnspentBoxes(maxInputsToUse * BoxSelector.ScanDepthFactor) ++ offChainBoxes).distinct
  }

}

object ErgoWalletState {

  private type FilterFn = TrackedBox => Boolean

  /**
    * This filter is not filtering out anything, used when the wallet works with externally provided boxes.
    */
  val noWalletFilter: FilterFn = (_: TrackedBox) => true

  def initial(ergoSettings: ErgoSettings, parameters: Parameters): Try[ErgoWalletState] = {
    WalletRegistry.apply(ergoSettings).map { registry =>
      val ergoStorage: WalletStorage = WalletStorage.readOrCreate(ergoSettings)
      val walletVars = WalletVars.apply(ergoStorage, ergoSettings)
      val maxInputsToUse = ergoSettings.walletSettings.maxInputs
      ErgoWalletState(
        ergoStorage,
        secretStorageOpt = None,
        registry,
        outputsFilter = None,
        walletVars,
        stateReaderOpt = None,
        mempoolReaderOpt = None,
        utxoStateReaderOpt = None,
        parameters,
        maxInputsToUse,
        rescanInProgress = false
      )
    }
  }
}
