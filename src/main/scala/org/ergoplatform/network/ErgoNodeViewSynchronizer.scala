package org.ergoplatform.network

import akka.actor.SupervisorStrategy.{Restart, Stop}
import akka.actor.{Actor, ActorInitializationException, ActorKilledException, ActorRef, ActorRefFactory, DeathPactException, OneForOneStrategy, Props}
import org.ergoplatform.modifiers.history.header.{Header, HeaderSerializer}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, ErgoTransactionSerializer, UnconfirmedTransaction}
import org.ergoplatform.modifiers.{BlockSection, ErgoNodeViewModifier, InputBlockTransactionIdsTypeId, InputBlockTypeId, ManifestTypeId, NetworkObjectTypeId, OrderingBlockAnnouncementTypeId, SnapshotsInfoTypeId, UtxoSnapshotChunkTypeId}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader, ErgoSyncInfo, ErgoSyncInfoMessageSpec, ErgoSyncInfoV1, ErgoSyncInfoV2}
import org.ergoplatform.nodeView.ErgoNodeViewHolder.BlockAppliedTransactions
import org.ergoplatform.nodeView.mempool.{ErgoMemPool, ErgoMemPoolReader}
import org.ergoplatform.settings.{Algos, ErgoSettings, NetworkSettings}
import org.ergoplatform.nodeView.ErgoNodeViewHolder._
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.{ChainIsHealthy, ChainIsStuck, GetNodeViewChanges, IsChainHealthy, ModifiersFromRemote, TransactionFromRemote}
import scorex.core.network.ModifiersStatus.Requested
import org.ergoplatform.core.idsToString
import scorex.core.network.NetworkController.ReceivableMessages.{PenalizePeer, SendToNetwork}
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages._
import org.ergoplatform.nodeView.state.{ErgoStateReader, SnapshotsInfo, UtxoSetSnapshotPersistence, UtxoStateReader}
import org.ergoplatform.network.message._
import org.ergoplatform.network.message.{InvSpec, MessageSpec, ModifiersSpec, RequestModifierSpec}
import scorex.core.network._
import scorex.core.network.{ConnectedPeer, ModifiersStatus, SendToPeer, SendToPeers}
import org.ergoplatform.network.message.{InvData, Message, ModifiersData}
import org.ergoplatform.utils.ScorexEncoder
import org.ergoplatform.validation.MalformedModifierError
import scorex.util.{ModifierId, ScorexLogging, bytesToId}
import scorex.core.network.DeliveryTracker
import org.ergoplatform.network.peer.PenaltyType
import scorex.crypto.hash.Digest32
import org.ergoplatform.nodeView.state.UtxoState.{ManifestId, SubtreeId}
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.consensus.{Equal, Fork, Nonsense, Older, Unknown, Younger}
import org.ergoplatform.modifiers.history.extension.Extension.PrevInputBlockIdKey
import org.ergoplatform.modifiers.history.{ADProofs, ADProofsSerializer, BlockTransactions, BlockTransactionsSerializer}
import org.ergoplatform.modifiers.history.extension.{Extension, ExtensionSerializer}
import org.ergoplatform.modifiers.mempool.ErgoTransaction.WeakId
import org.ergoplatform.modifiers.transaction.TooHighCostError
import org.ergoplatform.network.message.inputblocks._
import org.ergoplatform.nodeView.LocallyGeneratedOrderingBlock
import org.ergoplatform.serialization.{ErgoSerializer, ManifestSerializer, SubtreeSerializer}
import org.ergoplatform.subblocks.InputBlockInfo
import scorex.crypto.authds.avltree.batch.VersionedLDBAVLStorage.splitDigest
import sigma.VersionContext
import spire.syntax.all.cfor

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.{Failure, Random, Success}

/**
  * Contains most top-level logic for p2p networking, communicates with lower-level p2p code and other parts of the
  * client application
  */
class ErgoNodeViewSynchronizer(networkControllerRef: ActorRef,
                               viewHolderRef: ActorRef,
                               syncInfoSpec: ErgoSyncInfoMessageSpec.type,
                               settings: ErgoSettings,
                               syncTracker: ErgoSyncTracker,
                               deliveryTracker: DeliveryTracker)(implicit ex: ExecutionContext)
  extends Actor with Synchronizer with ScorexLogging {

  import org.ergoplatform.network.ErgoNodeViewSynchronizer._

  override val supervisorStrategy: OneForOneStrategy = OneForOneStrategy(
    maxNrOfRetries = 10,
    withinTimeRange = 1.minute) {
    case _: ActorKilledException => Stop
    case _: DeathPactException => Stop
    case e: ActorInitializationException =>
      log.warn(s"Stopping actor due to : $e")
      Stop
    case e: Exception =>
      log.warn(s"Restarting actor due to : $e")
      Restart
  }

  private var syncInfoV1CacheByHeadersHeight: Option[(Int, ErgoSyncInfoV1)] = Option.empty

  private var syncInfoV2CacheByHeadersHeight: Option[(Int, ErgoSyncInfoV2)] = Option.empty

  private val networkSettings: NetworkSettings = settings.scorexSettings.network

  protected val deliveryTimeout: FiniteDuration = networkSettings.deliveryTimeout

  private val minModifiersPerBucket = 8 // minimum of persistent modifiers (excl. headers) to download by single peer
  private val maxModifiersPerBucket = 12 // maximum of persistent modifiers (excl. headers) to download by single peer

  private val minHeadersPerBucket = 50 // minimum of headers to download by single peer
  private val maxHeadersPerBucket = 400 // maximum of headers to download by single peer

  // It could be the case that adversarial peers are sending sync messages to the node to cause
  // resource exhaustion. To prevent it, we do not provide an answer for sync message, if previous one was sent
  // no more than `PerPeerSyncLockTime` milliseconds ago.
  private val PerPeerSyncLockTime = 100

  // when we got last modifier, both unconfirmed transactions and block sections count
  private var lastModifierGotTime: Long = 0

  /**
    * The node stops to accept transactions if declined table reaches this max size. It prevents spam attacks trying
    * to bloat the table (or exhaust node's CPU)
    */
  private val MaxDeclined = 1000

  /**
    * No more than this number of unparsed transactions can be cached
    */
  private val MaxProcessingTransactionsCacheSize = 50

  /**
    * Max cost of transactions we are going to process between blocks
    */
  private val MempoolCostPerBlock = 12000000

  /**
    * Max cost of per-peer transactions we are going to process between blocks
    */
  private val MempoolPeerCostPerBlock = 10000000

  /**
    * Currently max transaction cost is higher but will be eventually cut down to this value
    */
  private val OptimisticMaxTransactionCost = 2000000


  /**
    * Dictionary (tx id -> checking time), which is storing transactions declined by the mempool, as mempool is not
    * storing this information. We keep declined transactions in the dictionary for few blocks just, as declined
    * transaction could become acceptable with time
    */
  private val declined = mutable.TreeMap[ModifierId, Long]()

  /**
    * Counter which contains total cost of transactions entered mempool or rejected by it since last block processed.
    * Used to avoid sudden spikes in load, limiting transactions processing time and make it comparable to block's
    * processing time
    */
  private var interblockCost = IncomingTxInfo.empty()

  /**
    * Counter which contains per-peer total cost of transactions entered mempool or rejected by it since last block
    * processed.
    */
  private val perPeerCost = mutable.Map[ConnectedPeer, IncomingTxInfo]()

  /**
    * Cache which contains bytes of transactions we received but not parsed and processed yet
    */
  private val txProcessingCache = mutable.Map[ModifierId, TransactionProcessingCacheRecord]()

  /**
    * Variable which is caching height of last header which was extracted from sync info message
    */
  private var lastSyncHeaderApplied: Option[Int] = Option.empty

  /**
    * Timestamp of last CheckModifiersToDownload command processing, used to not to process it too extensively
    */
  private var lastCheckForModifiersToDownload: Long = 0L

  /**
    * How many block sections stored in processing queue, imprecise number as updated only when
    * this actor is getting data from view holder actor
    */
  private var modifiersCacheSize: Int = 0

  /**
    * UTXO set snapshot manifests found in the p2p are stored in this table. The table is cleared when a manifest
    * is found which available for downloading from at least min number of peers required (the min is provided in
    * ergo.node.utxo.p2pUtxoSnapshots setting)
    */
  private val availableManifests = mutable.Map[ModifierId, (Height, Seq[ConnectedPeer])]()

  /**
    * Peers provided nipopow poofs
    */
  //todo: clear after bootstrapping
  private val nipopowProviders = mutable.Set[ConnectedPeer]()

  /**
    * How many peers should have a utxo set snapshot to start downloading it
    */
  private lazy val MinSnapshots = settings.nodeSettings.utxoSettings.p2pUtxoSnapshots

  // current script version (block version minus 1)
  private var activatedScriptVersion = Header.scriptFromBlockVersion(Header.InitialVersion)

  /**
    * To be called when the node is synced and new block arrives, to reset transactions cost counter
    */
  private def clearInterblockCost(): Unit = {
    interblockCost = IncomingTxInfo.empty()
  }

  /**
    * To be called when the node is synced and new block arrives, to resume transaction bytes cache processing
    */
  private def processFirstTxProcessingCacheRecord(): Unit = {
    txProcessingCache.headOption.foreach { case (txId, processingCacheRecord) =>
      parseAndProcessTransaction(txId, processingCacheRecord.txBytes, remote = processingCacheRecord.source)
      txProcessingCache -= txId
    }
  }

  /**
    * To be called when mempool reporting on finished transaction validation.
    * This method adds validation cost to counter and send another
    */
  private def processMempoolResult(processingResult: InitialTransactionCheckOutcome): Unit = {
    val FallbackCostValue = 5000

    val costOpt = processingResult.transaction.lastCost
    if (costOpt.isEmpty) {
      // should not be here, and so ReserveCostValue should not be used
      log.warn("Cost is empty in processMempoolResult")
    }

    val cost = costOpt.getOrElse(FallbackCostValue)

    val newInterblockCost = processingResult match {
      case _: FailedTransaction => interblockCost.copy(invalidatedCost = interblockCost.invalidatedCost + cost)
      case _: SuccessfulTransaction => interblockCost.copy(acceptedCost = interblockCost.acceptedCost + cost)
      case _: DeclinedTransaction => interblockCost.copy(declinedCost = interblockCost.declinedCost + cost)
    }

    log.debug(s"Old global cost info: $interblockCost, " +
      s"new: $newInterblockCost, tx processing cache size: ${txProcessingCache.size}")
    interblockCost = newInterblockCost

    val peerOpt = processingResult.transaction.source
    peerOpt match {
      case Some(peer) =>
        val peerTxInfo = perPeerCost.getOrElse(peer, IncomingTxInfo.empty())
        val newPeerCost = processingResult match {
          case _: FailedTransaction => peerTxInfo.copy(invalidatedCost = peerTxInfo.invalidatedCost + cost)
          case _: SuccessfulTransaction => peerTxInfo.copy(acceptedCost = peerTxInfo.acceptedCost + cost)
          case _: DeclinedTransaction => peerTxInfo.copy(declinedCost = peerTxInfo.declinedCost + cost)
        }
        log.debug(s"Old peer ${peer.connectionId} cost info: ${peerTxInfo.totalCost}, " +
          s"new: $newPeerCost, tx processing cache size: ${txProcessingCache.size}")
        perPeerCost.put(peer, newPeerCost)
      case _ => log.debug("No peer set, perPeerCost not updated.")
    }

    val withinGlobalLimit = interblockCost.totalCost < MempoolCostPerBlock
    val withinPeerLimit = peerOpt.isEmpty || (peerOpt.isDefined &&
      perPeerCost.getOrElse(peerOpt.get, IncomingTxInfo.empty()).totalCost < MempoolPeerCostPerBlock)

    if (withinGlobalLimit && withinPeerLimit) {
      processFirstTxProcessingCacheRecord()
    }
  }

  /**
    * Register periodic events
    */
  override def preStart(): Unit = {
    // subscribe for history, state and mempool changes
    viewHolderRef ! GetNodeViewChanges(history = true, state = true, vault = false, mempool = true)

    val toDownloadCheckInterval = networkSettings.syncInterval

    // register as a listener for peers got connected (handshaked) or disconnected
    context.system.eventStream.subscribe(self, classOf[HandshakedPeer])
    context.system.eventStream.subscribe(self, classOf[DisconnectedPeer])

    // subscribe for all the node view holder events involving modifiers and transactions
    context.system.eventStream.subscribe(self, classOf[ChangedHistory])
    context.system.eventStream.subscribe(self, classOf[ChangedMempool])
    context.system.eventStream.subscribe(self, classOf[ChangedState])
    context.system.eventStream.subscribe(self, classOf[ModificationOutcome])
    context.system.eventStream.subscribe(self, classOf[DownloadRequest])
    context.system.eventStream.subscribe(self, classOf[BlockAppliedTransactions])
    context.system.eventStream.subscribe(self, classOf[BlockSectionsProcessingCacheUpdate])

    // sub-blocks related messages
    context.system.eventStream.subscribe(self, classOf[DownloadInputBlock])
    context.system.eventStream.subscribe(self, classOf[DownloadInputBlockTransactions])
    context.system.eventStream.subscribe(self, classOf[NewBestInputBlock])
    context.system.eventStream.subscribe(self, classOf[LocallyGeneratedOrderingBlock])

    context.system.scheduler.scheduleAtFixedRate(toDownloadCheckInterval, toDownloadCheckInterval, self, CheckModifiersToDownload)

    val interval = networkSettings.syncInterval
    context.system.scheduler.scheduleWithFixedDelay(2.seconds, interval, self, SendLocalSyncInfo)

    val healthCheckDelay = settings.nodeSettings.acceptableChainUpdateDelay
    val healthCheckRate = settings.nodeSettings.acceptableChainUpdateDelay / 3
    context.system.scheduler.scheduleAtFixedRate(healthCheckDelay, healthCheckRate, viewHolderRef, IsChainHealthy)(ex, self)

    // Schedule periodic cleanup of old local input block chunks to prevent memory exhaustion
    context.system.scheduler.scheduleAtFixedRate(
      ErgoNodeViewSynchronizer.LocalInputBlockChunksCleanupInterval,
      ErgoNodeViewSynchronizer.LocalInputBlockChunksCleanupInterval,
      self,
      ErgoNodeViewSynchronizer.CleanupLocalInputBlockChunks
    )(ex, self)
  }

  protected def broadcastModifierInv(modTypeId: NetworkObjectTypeId.Value,
                                     modId: ModifierId,
                                     peersOpt: Option[Seq[ConnectedPeer]] = None): Unit = {
    val sendingStrategy = if(peersOpt.isDefined) {
      SendToPeers(peersOpt.get)
    } else {
      Broadcast
    }
    val msg = Message(InvSpec, Right(InvData(modTypeId, Seq(modId))), None)
    networkControllerRef ! SendToNetwork(msg, sendingStrategy)
  }

  protected def broadcastModifierInv(m: ErgoNodeViewModifier): Unit = {
    broadcastModifierInv(m.modifierTypeId, m.id)
  }

  /**
    * Check whether block section (modifier) with identifier `id` is not stored locally
    * (in history database available via `historyReader` interface, or delivery tracker cache, thus
    * downloading of the modifier is needed.
    */
  private def downloadRequired(historyReader: ErgoHistory)(modifierTypeId: NetworkObjectTypeId.Value, id: ModifierId): Boolean = {
    deliveryTracker.status(id, modifierTypeId, Array(historyReader)) == ModifiersStatus.Unknown
  }

  /** Get V1 sync info from cache or load it from history and add to cache */
  private def getV1SyncInfo(history: ErgoHistory): ErgoSyncInfoV1 = {
    val headersHeight = history.headersHeight
    syncInfoV1CacheByHeadersHeight
      .collect { case (height, syncInfo) if height == headersHeight => syncInfo }
      .getOrElse {
        val v1SyncInfo = history.syncInfoV1
        syncInfoV1CacheByHeadersHeight = Some(headersHeight -> v1SyncInfo)
        v1SyncInfo
      }
  }

  /** Get V2 sync info from cache or load it from history and add to cache */
  private def getV2SyncInfo(history: ErgoHistory, full: Boolean): ErgoSyncInfoV2 = {
    val headersHeight = history.headersHeight
    syncInfoV2CacheByHeadersHeight
      .collect { case (height, syncInfo) if height == headersHeight => syncInfo }
      .getOrElse {
        val v2SyncInfo = history.syncInfoV2(full)
        syncInfoV2CacheByHeadersHeight = Some(headersHeight -> v2SyncInfo)
        v2SyncInfo
      }
  }

  /**
    * Whether neighbour peer `remote` supports sync protocol V2.
    */
  def syncV2Supported(remote: ConnectedPeer): Boolean = SyncV2Filter.condition(remote)

  /**
    * Send synchronization statuses to neighbour peers
    *
    * The method sends sync messages to whether peers not received sync from the node for
    * some time (see syncStatusRefreshStable / syncStatusRefreshStable settings),
    * or peers with Unknown / Fork / Older statuses.
    *
    * Method sends V1/V2 sync messages based on neighbour version.
    *
    */
  protected def sendSync(history: ErgoHistory): Unit = {
    if (history.bestHeaderOpt.isEmpty && settings.nodeSettings.nipopowSettings.nipopowBootstrap) {
      // if no any header applied yet, and boostrapping via nipopows is ordered,
      // ask for nipopow proofs instead of sending sync signal
      requireNipopowProof(history)
    } else {
      val peers = syncTracker.peersToSyncWith()
      val (peersV2, peersV1) = peers.partition(p => syncV2Supported(p))
      log.debug(s"Syncing with ${peersV1.size} peers via sync v1, ${peersV2.size} peers via sync v2")
      if (peersV1.nonEmpty) {
        val msg = Message(syncInfoSpec, Right(getV1SyncInfo(history)), None)
        networkControllerRef ! SendToNetwork(msg, SendToPeers(peersV1))
      }
      if (peersV2.nonEmpty) {
        //todo: send only last header to peers which are equal or younger
        val v2SyncInfo = getV2SyncInfo(history, full = true)
        networkControllerRef ! SendToNetwork(Message(syncInfoSpec, Right(v2SyncInfo), None), SendToPeers(peersV2))
      }
    }
  }

  /**
    * Send sync message to a concrete peer. Used in [[processSync]] and [[processSyncV2]] methods.
    */
  protected def sendSyncToPeer(remote: ConnectedPeer, sync: ErgoSyncInfo): Unit = {
    if (sync.nonEmpty) {
      syncTracker.updateLastSyncSentTime(remote)
      networkControllerRef ! SendToNetwork(Message(syncInfoSpec, Right(sync), None), SendToPeer(remote))
    }
  }

  // Send history extension to the (less developed) peer 'remote' which does not have it.
  def sendExtension(remote: ConnectedPeer,
                    ext: Seq[(NetworkObjectTypeId.Value, ModifierId)]): Unit = {
    ext.groupBy(_._1).mapValues(_.map(_._2)).foreach {
      case (mid, mods) =>
        networkControllerRef ! SendToNetwork(Message(InvSpec, Right(InvData(mid, mods)), None), SendToPeer(remote))
    }
  }

  /**
    * Process sync message `syncInfo` got from neighbour peer `remote`
    */
  protected def processSync(hr: ErgoHistory, syncInfo: ErgoSyncInfo, remote: ConnectedPeer): Unit = {
    val diff = syncTracker.updateLastSyncGetTime(remote)
    if (diff > PerPeerSyncLockTime) {
      // process sync if sent in more than 200 ms after previous sync
      log.debug(s"Processing sync from $remote")
      val currentStatus = syncTracker.getStatus(remote)
      log.info(s"Peer ${remote.connectionId.remoteAddress} sync status before processing: $currentStatus")

      syncInfo match {
        case syncV1: ErgoSyncInfoV1 => processSyncV1(hr, syncV1, remote)
        case syncV2: ErgoSyncInfoV2 => processSyncV2(hr, syncV2, remote)
      }

      val updatedStatus = syncTracker.getStatus(remote)
      log.info(s"Peer ${remote.connectionId.remoteAddress} sync status after processing: $updatedStatus")
    } else {
      log.debug(s"Spammy sync detected from $remote")
    }
  }

  /**
    * Processing sync V1 message `syncInfo` got from neighbour peer `remote`
    */
  protected def processSyncV1(hr: ErgoHistory, syncInfo: ErgoSyncInfoV1, remote: ConnectedPeer): Unit = {
    val (status, syncSendNeeded) = syncTracker.updateStatus(remote, syncInfo, hr)

    status match {
      case Unknown =>
        // we do not know what to send to a peer with unknown status
        log.debug(s"Peer status is still unknown for $remote")
      case Nonsense =>
        // we do not know what to send to a peer with such status
        log.debug(s"Got nonsense status for $remote")
      case Younger | Fork =>
        // send extension (up to 400 header ids) to a peer which chain is less developed or forked
        val ext = hr.continuationIds(syncInfo, size = 400)
        if (ext.isEmpty) log.warn("Extension is empty while comparison is younger")
        log.debug(s"Sending extension of length ${ext.length}")
        log.debug(s"Extension ids: ${idsToString(ext)}")
        sendExtension(remote, ext)
      case Older =>
        // asking headers from older peers
        val ids = syncInfo.lastHeaderIds.reverse
        val headerIds = ids.takeWhile(hId => !hr.isInBestChain(hId))
        if (headerIds.nonEmpty) {
          val maxHeadersToDownload = deliveryTracker.headersToDownload
          log.debug(s"Requesting $maxHeadersToDownload headers from older peers after getting sync info from $remote")
          requestDownload(
            maxHeadersToDownload,
            minHeadersPerBucket,
            maxHeadersPerBucket
          )(Option(getPeersForDownloadingHeaders(remote))) { howManyPerType =>
            val modifierIds =
              headerIds
                .reverse
                .filter(mid => downloadRequired(hr)(Header.modifierTypeId, mid))
                .take(howManyPerType)
            Map(Header.modifierTypeId -> modifierIds)
          }
        }
      case Equal =>
        // does nothing for `Equal`
        log.debug(s"$remote has equal header-chain")
    }

    if (syncSendNeeded) {
      val ownSyncInfo = getV1SyncInfo(hr)
      sendSyncToPeer(remote, ownSyncInfo)
    }
  }

  /**
    * Processing sync V2 message `syncInfo` got from neighbour peer `remote` (supporting sync v2)
    */
  protected def processSyncV2(hr: ErgoHistory, syncInfo: ErgoSyncInfoV2, remote: ConnectedPeer): Unit = {
    val (status, syncSendNeeded) = syncTracker.updateStatus(remote, syncInfo, hr)

    status match {
      case Unknown =>
        // we do not know what to send to a peer with unknown status
        log.info(s"Peer status is still unknown for $remote")

      case Nonsense =>
        // Shouldn't be the case for sync V2
        log.warn(s"Got nonsense status in v2 for $remote")

      case Younger =>
        // send extension (up to 400 header ids) to a peer which chain is less developed
        val ext = hr.continuationIds(syncInfo, size = 400)
        if (ext.isEmpty) log.warn("Extension is empty while comparison is younger")
        log.debug(s"Sending extension of length ${ext.length} to younger peer $remote")
        log.debug(s"Extension ids: ${idsToString(ext)}")
        sendExtension(remote, ext)

      case Fork =>
        // send extension (up to 400 header ids) to a peer which chain is forked
        val ext = hr.continuationIds(syncInfo, size = 400)
        if (ext.isEmpty) log.warn("Extension is empty while comparison is fork")
        log.debug(s"Sending extension of length ${ext.length} to forked peer $remote")
        log.debug(s"Extension ids: ${idsToString(ext)}")
        sendExtension(remote, ext)

      case Older =>
        log.debug(s"Peer $remote is older, its height ${syncInfo.height}")
        applyValidContinuationHeaderV2(syncInfo, hr, remote)

      case Equal =>
        // does nothing for `Equal`
        log.debug(s"$remote has equal header-chain")
    }

    if (syncSendNeeded) {
      val ownSyncInfo = getV2SyncInfo(hr, full = true)
      sendSyncToPeer(remote, ownSyncInfo)
    }
  }


  /**
    * Calculates new continuation header from syncInfo message if any, validates it and sends it
    * to nodeViewHolder as a remote modifier for it to be applied
    *
    * @param syncInfo other's node sync info
    */
  private def applyValidContinuationHeaderV2(syncInfo: ErgoSyncInfoV2,
                                             history: ErgoHistory,
                                             peer: ConnectedPeer): Unit = {
    history.continuationHeaderV2(syncInfo).foreach { continuationHeader =>
      if (deliveryTracker.status(continuationHeader.id, Header.modifierTypeId, Seq.empty) == ModifiersStatus.Unknown) {
        if (continuationHeader.height > lastSyncHeaderApplied.getOrElse(0)) {
          log.info(s"Applying valid syncInfoV2 header ${continuationHeader.encodedId}")
          lastSyncHeaderApplied = Some(continuationHeader.height)
          viewHolderRef ! ModifiersFromRemote(Seq(continuationHeader))
          val modifiersToDownload = history.requiredModifiersForHeader(continuationHeader)
          log.info(s"Downloading block sections for header ${continuationHeader.encodedId}")
          modifiersToDownload.foreach {
            case (modifierTypeId, modifierId) =>
              if (deliveryTracker.status(modifierId, modifierTypeId, Seq.empty) == ModifiersStatus.Unknown) {
                requestBlockSection(modifierTypeId, Seq(modifierId), peer)
              }
          }
        }
      }
    }
  }

  /**
    * Headers should be downloaded from an Older node, it is triggered by received sync message from an older node
    *
    * @param callingPeer fallback peer that can be used to download headers, it must be Older
    * @return available peers to download headers from together with the state/origin of the peer
    */
  private def getPeersForDownloadingHeaders(callingPeer: ConnectedPeer): Iterable[ConnectedPeer] = {
    val nonFiltered: Iterable[ConnectedPeer] = syncTracker.peersByStatus.getOrElse(Older, Array(callingPeer))
    HeadersDownloadFilter.filter(nonFiltered)
  }

  /**
    * Other persistent modifiers besides headers should be downloaded from either Older or Equal node, with fallback to Unknown or Fork
    *
    * @return available peers to download persistent modifiers from together with the state/origin of the peer
    */
  private def getPeersForDownloadingBlocks: Option[Iterable[ConnectedPeer]] = {
    val peersByStatus = syncTracker.peersByStatus
    Option(peersByStatus.getOrElse(Older, mutable.WrappedArray.empty) ++ peersByStatus.getOrElse(Equal, mutable.WrappedArray.empty))
      .filter(_.nonEmpty)
      .orElse {
        Option(peersByStatus.getOrElse(Unknown, mutable.WrappedArray.empty) ++ peersByStatus.getOrElse(Fork, mutable.WrappedArray.empty))
          .filter(_.nonEmpty)
      }.map(BlockSectionsDownloadFilter.filter)
  }

  /**
    * A helper method to ask for block section from given peer
    *
    * @param modifierTypeId - block section type id
    * @param modifierIds    - ids of block section to download
    * @param peer           - peer to download from
    * @param checksDone     - how many times the block section was requested before
    *                       (non-zero if we're re-requesting the block section, in this case, there should be only
    *                       one id to request in `modifierIds`
    */
  def requestBlockSection(modifierTypeId: NetworkObjectTypeId.Value,
                          modifierIds: Seq[ModifierId],
                          peer: ConnectedPeer,
                          checksDone: Int = 0): Unit = {
    log.debug(s"Requesting block sections of type $modifierTypeId : $modifierIds")
    if (checksDone > 0 && modifierIds.length > 1) {
      log.warn(s"Incorrect state, checksDone > 0 && modifierIds.length > 1 , for $modifierIds of type $modifierTypeId")
    }
    val msg = Message(RequestModifierSpec, Right(InvData(modifierTypeId, modifierIds)), None)
    val stn = SendToNetwork(msg, SendToPeer(peer))
    networkControllerRef ! stn

    modifierIds.foreach { modifierId =>
      deliveryTracker.setRequested(modifierTypeId, modifierId, peer, checksDone) { deliveryCheck =>
        context.system.scheduler.scheduleOnce(deliveryTimeout, self, deliveryCheck)
      }
    }
  }

  /*
   * Private helper methods to request UTXO set snapshots metadata and related data (manifests, chunks) from peers
   */

  private def requestSnapshotsInfo(): Unit = {
    // ask all the peers supporting UTXO set snapshots for snapshots they have
    val msg = Message(GetSnapshotsInfoSpec, Right(()), None)
    val peers = UtxoSetNetworkingFilter.filter(syncTracker.knownPeers()).toSeq
    val peersCount = peers.size
    if (peersCount >= MinSnapshots) {
      networkControllerRef ! SendToNetwork(msg, SendToPeers(peers))
    } else {
      log.warn(s"Less UTXO-snapshot supporting peers found than required mininum ($peersCount < $MinSnapshots)")
    }
  }

  private def requestManifest(manifestId: ManifestId, peer: ConnectedPeer): Unit = {
    deliveryTracker.setRequested(ManifestTypeId.value, ModifierId @@ Algos.encode(manifestId), peer) { deliveryCheck =>
      context.system.scheduler.scheduleOnce(deliveryTimeout, self, deliveryCheck)
    }
    val msg = Message(GetManifestSpec, Right(manifestId), None)
    networkControllerRef ! SendToNetwork(msg, SendToPeer(peer))
  }

  private def requestUtxoSetChunk(subtreeId: SubtreeId, peer: ConnectedPeer): Unit = {
    // as we download multiple chunks in parallel and they can be quite large, timeout increased
    val chunkDeliveryTimeout = 4 * deliveryTimeout
    deliveryTracker.setRequested(UtxoSnapshotChunkTypeId.value, ModifierId @@ Algos.encode(subtreeId), peer) { deliveryCheck =>
      context.system.scheduler.scheduleOnce(chunkDeliveryTimeout, self, deliveryCheck)
    }
    val msg = Message(GetUtxoSnapshotChunkSpec, Right(subtreeId), None)
    networkControllerRef ! SendToNetwork(msg, SendToPeer(peer))
  }

  private def onDownloadRequest(historyReader: ErgoHistory): Receive = {
    case DownloadRequest(modifiersToFetch: Map[NetworkObjectTypeId.Value, Seq[ModifierId]]) =>
      log.debug(s"Downloading via DownloadRequest: $modifiersToFetch")
      if (modifiersToFetch.nonEmpty) {
        requestDownload(
          maxModifiers = deliveryTracker.modifiersToDownload,
          minModifiersPerBucket,
          maxModifiersPerBucket
        )(getPeersForDownloadingBlocks) { _ =>
          // leave block section ids only not touched before
          modifiersToFetch.flatMap { case (tid, mids) =>
            val updMids = mids.filter { mid =>
              deliveryTracker.status(mid, tid, Seq(historyReader)) == ModifiersStatus.Unknown
            }
            if (updMids.isEmpty) {
              None
            } else {
              Some(tid -> updMids)
            }
          }
        }
      }
    case DownloadInputBlock(sbId, remote) =>
      // processing internal request to download an input block
      requestInputBlock(sbId, remote)
    case DownloadInputBlockTransactions(req, remote) =>
      // processing internal request to download input block transactions
      val msg = Message(InputBlockTransactionsRequestMessageSpec, Right(req), None)
      networkControllerRef ! SendToNetwork(msg, SendToPeer(remote))
  }

  /**
    * Modifier download method that is given min/max constraints for modifiers to download from peers.
    * It sends requests for modifiers to given peers in optimally sized batches.
    *
    * @param maxModifiers          maximum modifiers to download
    * @param minModifiersPerBucket minimum modifiers to download per bucket
    * @param maxModifiersPerBucket maximum modifiers to download per bucket
    * @param getPeersOpt           optionally get peers to download from, all peers have the same PeerSyncState
    * @param fetchMax              function that fetches modifiers, it is passed how many of them tops
    */
  protected def requestDownload(maxModifiers: Int, minModifiersPerBucket: Int, maxModifiersPerBucket: Int)
                               (getPeersOpt: => Option[Iterable[ConnectedPeer]])
                               (fetchMax: Int => Map[NetworkObjectTypeId.Value, Seq[ModifierId]]): Unit = {
    getPeersOpt match {
      case Some(peers) if peers.nonEmpty =>
        val peersCount = peers.size
        val maxElementsToFetch = Math.min(maxModifiers, peersCount * maxModifiersPerBucket)
        val fetched = if (maxElementsToFetch <= 0) {
          Map.empty
        } else {
          fetchMax(maxElementsToFetch)
        }
        if (fetched.size == 1 && fetched.head._1 == SnapshotsInfoTypeId.value) {
          // special case when underlying logic in `fetchMax` is providing a request
          // to start downloading UTXO set snapshots
          requestSnapshotsInfo()
        } else {
          val modifiersByBucket = ElementPartitioner.distribute(peers, minModifiersPerBucket, fetched)
          // collect and log useful downloading progress information, don't worry it does not run frequently
          modifiersByBucket.headOption.foreach { _ =>
            modifiersByBucket
              .groupBy(_._1._2)
              .mapValues(_.map(_._2.size))
              .map { case (modType, batchSizes) =>
                s"Downloading from peers : type[$modType] of ${batchSizes.size} batches each of ~ size: ${batchSizes.take(2).max}"
              }.foreach(log.info(_))
          }
          // bucket represents a peer and a modifierType as we cannot send mixed types to a peer
          modifiersByBucket.foreach { case ((peer, modifierTypeId), modifierIds) =>
            requestBlockSection(modifierTypeId, modifierIds, peer)
          }
        }
      case _ =>
        log.warn("No peers available in requestDownload")
    }
  }

  private def transactionsFromRemote(requestedModifiers: Map[ModifierId, Array[Byte]],
                                     mp: ErgoMemPool,
                                     remote: ConnectedPeer): Unit = {
    // filter out transactions already in the mempool
    val notInThePool = requestedModifiers.filterKeys(id => !mp.contains(id))
    val peerCost = perPeerCost.getOrElse(remote, IncomingTxInfo.empty()).totalCost

    val (toProcess, toPutIntoCache) =
      if (peerCost < MempoolPeerCostPerBlock && interblockCost.totalCost < MempoolCostPerBlock) {
        // if we are within peer and total per-block limits, parse and process first transaction
        (notInThePool.headOption, notInThePool.tail)
      } else {
        (None, notInThePool)
      }

    toProcess.foreach { case (txId, txBytes) =>
      parseAndProcessTransaction(txId, txBytes, remote)
    }
    toPutIntoCache.foreach { case (txId, txBytes) =>
      txProcessingCache.put(txId, new TransactionProcessingCacheRecord(txBytes, remote))
    }
  }

  private def blockSectionsFromRemote(hr: ErgoHistory,
                                      typeId: NetworkObjectTypeId.Value,
                                      requestedModifiers: Map[ModifierId, Array[Byte]],
                                      remote: ConnectedPeer): Unit  = {
    modifierSerializers.get(typeId) match {
      case Some(serializer: ErgoSerializer[BlockSection]@unchecked) =>
        // parse all modifiers and put them to modifiers cache
        val parsed: Iterable[BlockSection] = parseModifiers(requestedModifiers, typeId, serializer, remote)

        // `deliveryTracker.setReceived()` called inside `validateAndSetStatus` for every correct modifier
        val valid = parsed.filter(validateAndSetStatus(hr, remote, _))
        if (valid.nonEmpty) {
          log.debug(s"Sending ${valid.size} modifiers to view holder, vh cache size: $modifiersCacheSize")
          modifiersCacheSize += valid.size // we increase estimated cache size now, before getting a precise number
          viewHolderRef ! ModifiersFromRemote(valid)
          // send sync message to the peer to get new headers quickly
          if (valid.head.isInstanceOf[Header]) {
            val syncInfo = if (syncV2Supported(remote)) {
              getV2SyncInfo(hr, full = false)
            } else {
              getV1SyncInfo(hr)
            }
            sendSyncToPeer(remote, syncInfo)
          }
        }

      case _ =>
        log.error(s"Undefined serializer for modifier of type $typeId")
    }
  }

  /**
    * Logic to process block parts got from another peer.
    * Filter out non-requested block parts (with a penalty to spamming peer),
    * parse block parts and send valid modifiers to NodeViewHolder
    */
  protected def modifiersFromRemote(hr: ErgoHistory,
                                    mp: ErgoMemPool,
                                    data: ModifiersData,
                                    remote: ConnectedPeer,
                                    blockAppliedTxsCache: FixedSizeApproximateCacheQueue): Unit = {
    val typeId = data.typeId
    val modifiers = data.modifiers
    log.info(s"Got ${modifiers.size} modifiers of type $typeId from remote connected peer: ${remote.connectionId}")
    log.debug("Modifier ids: " + modifiers.keys)

    lastModifierGotTime = System.currentTimeMillis()

    // filter out non-requested modifiers
    val requestedModifiers = processSpam(remote, typeId, modifiers, blockAppliedTxsCache)

    if (typeId == ErgoTransaction.modifierTypeId) {
      transactionsFromRemote(requestedModifiers, mp, remote)
    } else {
      blockSectionsFromRemote(hr, typeId, requestedModifiers, remote)
    }
  }

  /**
    * Parse transaction coming from remote, filtering out immediately too big one, and send parsed transaction
    * to mempool for processing
    */
  def parseAndProcessTransaction(id: ModifierId, bytes: Array[Byte], remote: ConnectedPeer): Unit = {
    if (bytes.length > settings.nodeSettings.maxTransactionSize) {
      deliveryTracker.setInvalid(id, ErgoTransaction.modifierTypeId)
      penalizeMisbehavingPeer(remote)
      log.warn(s"Transaction size ${bytes.length} from ${remote.toString} " +
                s"exceeds limit ${settings.nodeSettings.maxTransactionSize}")
    } else {
      // actual tree version is properly set in ErgoTreeSerializer inside
      val parseResult = VersionContext.withVersions(activatedScriptVersion, activatedScriptVersion)(ErgoTransactionSerializer.parseBytesTry(bytes))
      parseResult match {
        case Success(tx) if id == tx.id =>
          val utx = UnconfirmedTransaction(tx, bytes, Some(remote))
          viewHolderRef ! TransactionFromRemote(utx)
        case _ =>
          // Penalize peer and do nothing - it will be switched to correct state on CheckDelivery
          penalizeMisbehavingPeer(remote)
          log.warn(s"Failed to parse transaction with declared id ${ScorexEncoder.encodeId(id)} from ${remote.toString}")
      }
    }
  }

  /**
    * Parse block sections with serializer provided, check that its id is equal to the declared one,
    * penalize misbehaving peer for every incorrect modifier
    *
    * @return collection of parsed modifiers
    */
  def parseModifiers[M <: ErgoNodeViewModifier](modifiers: Map[ModifierId, Array[Byte]],
                                                modifierTypeId: NetworkObjectTypeId.Value,
                                                serializer: ErgoSerializer[M],
                                                remote: ConnectedPeer): Iterable[M] = {
    modifiers.flatMap { case (id, bytes) =>
      serializer.parseBytesTry(bytes) match {
        case Success(mod) if id == mod.id =>
          Some(mod)
        case _ =>
          // Penalize peer and do nothing
          // Forget about block section, so it will be redownloaded if announced again only
          deliveryTracker.setUnknown(id, modifierTypeId)
          penalizeMisbehavingPeer(remote)
          log.warn(s"Failed to parse modifier with declared id ${ScorexEncoder.encodeId(id)} from ${remote.toString}")
          None
      }
    }
  }

  /**
    *
    * Get modifiers from remote peer, filter out spam modifiers and penalize peer for spam
    *
    * @return ids and bytes of modifiers that were requested by our node
    */
  def processSpam(remote: ConnectedPeer,
                  typeId: NetworkObjectTypeId.Value,
                  modifiers: Map[ModifierId, Array[Byte]],
                  blockAppliedTxsCache: FixedSizeApproximateCacheQueue): Map[ModifierId, Array[Byte]] = {
    val modifiersByStatus =
      modifiers
        .groupBy { case (id, _) => deliveryTracker.status(id, typeId, Seq.empty) }
        .view.force

    val spam = modifiersByStatus.filterKeys(_ != Requested)

    if (spam.nonEmpty) {
      if (typeId == ErgoTransaction.modifierTypeId) {
        // penalize a peer for sending TXs that have been already applied to a block
        val spammyTxs = modifiers.filterKeys(blockAppliedTxsCache.mightContain)
        if (spammyTxs.nonEmpty) {
          log.info(s"Got spammy transactions: $spammyTxs")
          penalizeSpammingPeer(remote)
        }
      } else {
        spam.foreach { case (status, mods) =>
          log.info(s"Spam attempt: non-requested modifiers of type $typeId and status $status " +
            s"with ${mods.size} ids sent by peer $remote")
        }
        penalizeSpammingPeer(remote)
      }
    }
    modifiersByStatus.getOrElse(Requested, Map.empty)
  }

  /*
   * Private helper methods to send UTXO set snapshots related network messages
   */

  private def sendSnapshotsInfo(usr: UtxoSetSnapshotPersistence, peer: ConnectedPeer): Unit = {
    val snapshotsInfo = usr.getSnapshotInfo()
    log.debug(s"Sending snapshots info with ${snapshotsInfo.availableManifests.size} snapshots to $peer")
    val msg = Message(SnapshotsInfoSpec, Right(snapshotsInfo), None)
    networkControllerRef ! SendToNetwork(msg, SendToPeer(peer))
  }

  private def sendManifest(id: ManifestId, usr: UtxoSetSnapshotPersistence, peer: ConnectedPeer): Unit = {
    usr.getManifestBytes(id) match {
      case Some(manifestBytes) => {
        val msg = Message(ManifestSpec, Right(manifestBytes), None)
        networkControllerRef ! SendToNetwork(msg, SendToPeer(peer))
      }
      case _ => log.warn(s"No manifest ${Algos.encode(id)} available")
    }
  }

  private def sendUtxoSnapshotChunk(subtreeId: SubtreeId, usr: UtxoSetSnapshotPersistence, peer: ConnectedPeer): Unit = {
    usr.getUtxoSnapshotChunkBytes(subtreeId) match {
      case Some(snapChunk) => {
        log.debug(s"Sending utxo snapshot chunk (${Algos.encode(subtreeId)}) to $peer")
        val msg = Message(UtxoSnapshotChunkSpec, Right(snapChunk), None)
        networkControllerRef ! SendToNetwork(msg, SendToPeer(peer))
      }
      case _ => log.warn(s"No chunk ${Algos.encode(subtreeId)} available")
    }
  }

  private def requestMoreChunksIfNeeded(hr: ErgoHistory): Unit = {
    // we request more chunks if currently less than `ChunksInParallelMin` chunks are being downloading
    // we request up to `ChunksInParallelMin`, so in extreme case may download almost 2 * `ChunksInParallelMin`
    // we download new chunks from random peers, `ChunksPerPeer` chunks from a random peer (but we may ask the same
    // peer twice as choice is truly random)
    val ChunksInParallelMin = 16
    val ChunksPerPeer = 4
    hr.utxoSetSnapshotDownloadPlan() match {
      case Some(downloadPlan) =>

        if (downloadPlan.downloadingChunks < ChunksInParallelMin) {
          (1 to ChunksPerPeer).foreach { _ =>
            val toRequest = hr.getChunkIdsToDownload(howMany = ChunksInParallelMin / ChunksPerPeer)
            hr.randomPeerToDownloadChunks() match {
              case Some(remote) => toRequest.foreach(subtreeId => requestUtxoSetChunk(subtreeId, remote))
              case None =>
                log.warn(s"No peers to download chunks from")
            }
          }
        }
      case None =>
        log.warn("No download plan found in requestMoreChunksIfNeeded")
    }
  }

  /**
    * Process information about snapshots got from another peer
    */
  private def processSnapshotsInfo(hr: ErgoHistory,
                                   snapshotsInfo: SnapshotsInfo,
                                   remote: ConnectedPeer): Unit = {
    snapshotsInfo.availableManifests.foreach { case (height, manifestId: ManifestId) =>
      val encodedManifestId = ModifierId @@ Algos.encode(manifestId)
      val ownId = hr.bestHeaderAtHeight(height).map(_.stateRoot).map(stateDigest => splitDigest(stateDigest)._1)
      if (ownId.getOrElse(Array.emptyByteArray).sameElements(manifestId)) {
        log.debug(s"Discovered manifest $encodedManifestId for height $height from $remote")
        // add manifest to available manifests dictionary if it is not written there yet
        val existingOffers = availableManifests.getOrElse(encodedManifestId, (height -> Seq.empty))
        if (!existingOffers._2.contains(remote)) {
          log.info(s"Found new manifest ${Algos.encode(manifestId)} for height $height at $remote")
          availableManifests.put(encodedManifestId, height -> (existingOffers._2 :+ remote))
        } else {
          log.warn(s"Double manifest declaration for $manifestId from $remote")
        }
      } else {
        log.error(s"Got wrong manifest id $encodedManifestId from $remote for height $height, " +
                  s"their id: ${Algos.encode(manifestId)}, our id ${ownId.map(Algos.encode)}")
      }
    }
    checkUtxoSetManifests(hr) // check if we got enough manifests for the height to download manifests and chunks
  }

  // process serialized manifest got from another peer
  private def processManifest(hr: ErgoHistory, manifestBytes: Array[Byte], remote: ConnectedPeer): Unit = {
    ManifestSerializer.defaultSerializer.parseBytesTry(manifestBytes) match {
      case Success(manifest) =>
        val manifestId = ModifierId @@ Algos.encode(manifest.id)
        log.info(s"Got manifest $manifestId from $remote")
        deliveryTracker.getRequestedInfo(ManifestTypeId.value, manifestId) match {
          case Some(ri) if ri.peer == remote =>
            deliveryTracker.setUnknown(manifestId, ManifestTypeId.value)
            val manifestRecordOpt = availableManifests.get(manifestId)
            manifestRecordOpt match {
              case Some(manifestRecord) =>
                val height = manifestRecord._1
                val peersToDownload = manifestRecord._2

                // check if manifest is valid against root hash and height of AVL+ tree found in a header
                val manifestVerified = hr
                  .bestHeaderAtHeight(height)
                  .map(_.stateRoot)
                  .map(splitDigest)
                  .exists { case (expRoot, expHeight) => manifest.verify(Digest32 @@ expRoot, expHeight) }

                if(manifestVerified) {
                  log.info(s"Going to download chunks for manifest ${Algos.encode(manifest.id)} at height $height from $peersToDownload")
                  hr.registerManifestToDownload(manifest, height, peersToDownload)
                  availableManifests.clear()
                  requestMoreChunksIfNeeded(hr)
                } else {
                  log.error(s"Got invalid manifest from $remote")
                  penalizeMaliciousPeer(remote)
                }
              case None =>
                log.error(s"No height found for manifest ${Algos.encode(manifest.id)}")
            }
          case _ =>
            log.info(s"Penalizing spamming peer $remote sent non-asked manifest $manifestId")
            penalizeSpammingPeer(remote)
        }
      case Failure(e) =>
        log.info(s"Cant' restore manifest (got from $remote) from bytes ", e)
        penalizeMisbehavingPeer(remote)
    }
  }

  // process utxo set snapshot chunk got from another peer
  private def processUtxoSnapshotChunk(serializedChunk: Array[Byte], hr: ErgoHistory, remote: ConnectedPeer): Unit = {
    SubtreeSerializer.parseBytesTry(serializedChunk) match {
      case Success(subtree) =>
        val chunkId = ModifierId @@ Algos.encode(subtree.id)
        deliveryTracker.getRequestedInfo(UtxoSnapshotChunkTypeId.value, chunkId) match {
          case Some(_) =>
            log.debug(s"Got utxo snapshot chunk, id: $chunkId, size: ${serializedChunk.length}")
            deliveryTracker.setUnknown(chunkId, UtxoSnapshotChunkTypeId.value)
            hr.registerDownloadedChunk(subtree.id, serializedChunk)

            hr.utxoSetSnapshotDownloadPlan() match {
              case Some(downloadPlan) =>
                if (downloadPlan.fullyDownloaded) {
                  log.info("All the UTXO set snapshot chunks downloaded")
                  // if all the chunks of snapshot are downloaded, initialize UTXO set state with it
                  if (!hr.isUtxoSnapshotApplied) {
                    val h = downloadPlan.snapshotHeight
                    hr.bestHeaderIdAtHeight(h) match {
                      case Some(blockId) =>
                        viewHolderRef ! InitStateFromSnapshot(h, blockId)
                      case None =>
                        // shouldn't happen in principle
                        log.error("No best header found when all chunks are downloaded. Please contact developers.")
                    }
                  } else {
                    log.warn("UTXO set snapshot already applied, double application attemt")
                  }
                } else {
                  // if not all the chunks are downloaded, request more if needed
                  requestMoreChunksIfNeeded(hr)
                }
              case None =>
                log.warn(s"No download plan found when processing UTXO set snapshot chunk $chunkId")
            }

          case None =>
            log.info(s"Penalizing spamming peer $remote sent non-asked UTXO set snapshot chunk $chunkId")
            penalizeSpammingPeer(remote)
        }

      case Failure(e) =>
        log.info(s"Cant' restore snapshot chunk (got from $remote) from bytes ", e)
        penalizeMisbehavingPeer(remote)
    }
  }

  /**
    * Ask peers around for NiPoPoW proofs of the chain to bootstrap
    */
  private def requireNipopowProof(hr: ErgoHistory): Unit = {
    val m = hr.P2PNipopowProofM
    val k = hr.P2PNipopowProofK
    val msg = Message(GetNipopowProofSpec, Right(NipopowProofData(m, k, None)), None)
    val knownPeers = syncTracker.knownPeers()
    val peers = NipopowSupportFilter.filter(knownPeers).filter(cp => !nipopowProviders.contains(cp)).toSeq
    if (peers.nonEmpty) {
      networkControllerRef ! SendToNetwork(msg, SendToPeers(peers))
    } else {
      log.warn("No peers to ask NiPoPoWs from")
    }
  }

  /**
    * Send NiPoPoW proof to a peer asked it
    */
  private def sendNipopowProof(data: NipopowProofData, hr: ErgoHistory, peer: ConnectedPeer): Unit = {
    if (data.m == hr.P2PNipopowProofM && data.k == hr.P2PNipopowProofK && data.headerIdBytesOpt.isEmpty) {
      hr.readPopowProofBytesFromDb() match {
        case Some(proofBytes) =>
          val msg = Message(NipopowProofSpec, Right(proofBytes), None)
          networkControllerRef ! SendToNetwork(msg, SendToPeer(peer))
        case None =>
          log.warn("No Nipopow Proof available")
      }
    } else {
      // for now, we are serving proofs for concrete params only
      log.warn(s"Peer $peer asked for a nipopow proof which can't be served (params: $data)")
    }
  }

  /**
    * Process NiPoPoW proof got from remote peer
    */
  private def processNipopowProof(proofBytes: Array[Byte], hr: ErgoHistory, peer: ConnectedPeer): Unit = {
    if (hr.bestHeaderOpt.isEmpty) {
      if (!nipopowProviders.contains(peer)) {
        nipopowProviders += peer
        hr.nipopowSerializer.parseBytesTry(proofBytes) match {
          case Success(proof) if proof.isValid =>
            log.info(s"Got valid nipopow proof, size: ${proofBytes.length}")
            viewHolderRef ! ProcessNipopow(proof)
          case _ =>
            log.warn(s"Peer $peer sent wrong nipopow")
            penalizeMisbehavingPeer(peer)
        }
      } else {
        log.info(s"Received Nipopow proof from $peer again")
      }
    } else {
      log.warn("Got nipopow proof, but it is already applied")
    }
  }

  /**
    * Object ids coming from other node.
    * Filter out modifier ids that are already in process (requested, received or applied),
    * request unknown ids from peer and set this ids to requested state.
    */
  protected def processInv(hr: ErgoHistory,
                           mp: ErgoMemPool,
                           invData: InvData,
                           peer: ConnectedPeer,
                           blockAppliedTxsCache: FixedSizeApproximateCacheQueue): Unit = {

    val peerCost = perPeerCost.getOrElse(peer, IncomingTxInfo.empty()).totalCost

    // We download transactions only if following conditions met:
    def txAcceptanceFilter: Boolean = {
      settings.nodeSettings.stateType.holdsUtxoSet && // node holds UTXO set
        hr.headersHeight >= syncTracker.maxHeight().getOrElse(0) && // our best header is not worse than best around
        hr.fullBlockHeight == hr.headersHeight && // we have all the full blocks
      interblockCost.totalCost <= MempoolCostPerBlock * 3 / 2 && // we can download some extra to fill cache
      peerCost <= MempoolPeerCostPerBlock * 3 / 2 && // we can download some extra to fill cache
      txProcessingCache.size <= MaxProcessingTransactionsCacheSize && // txs processing cache is not overfull
        declined.size < MaxDeclined // the node is not stormed by transactions is has to decline
    }

    val modifierTypeId = invData.typeId

    val newModifierIds = modifierTypeId match {
      case ErgoTransaction.modifierTypeId =>

        if (txAcceptanceFilter) {
          val unknownMods = {
            // check that transaction is not in the mempool already or invalidated earlier
            invData.ids.filter { mid =>
              deliveryTracker.status(mid, modifierTypeId, Seq(mp)) == ModifiersStatus.Unknown &&
                !mp.isInvalidated(mid)
            }
          }
          // filter out transactions that were already applied to history
          val notApplied = unknownMods.filterNot(blockAppliedTxsCache.mightContain)
          // filter out transactions previously declined
          val notDeclined = notApplied.filter(id => !declined.contains(id))
          log.info(s"Processing ${invData.ids.length} tx invs from $peer, " +
            s"${unknownMods.size} of them are unknown, requesting $notDeclined")
          val txsToAsk = (MempoolCostPerBlock - interblockCost.totalCost) / OptimisticMaxTransactionCost
          notDeclined.take(txsToAsk)
        } else {
          Seq.empty
        }
      case _ =>
        log.info(s"Processing ${invData.ids.length} non-tx invs (of type $modifierTypeId) from $peer")
        invData.ids.filter(mid => deliveryTracker.status(mid, modifierTypeId, Seq(hr)) == ModifiersStatus.Unknown)
    }

    if (newModifierIds.nonEmpty) {
      log.debug(s"Going to request ${newModifierIds.length} modifiers of type $modifierTypeId from $peer")
      requestBlockSection(modifierTypeId, newModifierIds, peer)
    }
  }

  /**
    * If our requested list is more than half empty, enforce to request more:
    * - headers, if our headers chain is not synced yet (by sending sync message)
    * - block sections, if our headers chain is synced
    */
  protected def requestMoreModifiers(historyReader: ErgoHistory): Unit = {
    if (historyReader.isHeadersChainSynced) {
      self ! CheckModifiersToDownload
    } else {
      // headers chain is not synced yet, but our requested list is half empty - ask for more headers
      sendSync(historyReader)
    }
  }

  /**
   * Handle a request from a peer for specific modifiers by their IDs.
   *
   * This method processes requests from peers for specific modifiers (blocks, transactions, etc.)
   * by their IDs. It handles different types of modifiers differently, with special handling
   * for input blocks, input block transaction IDs, and ordering block announcements.
   * For regular modifiers, it retrieves them from history or mempool and sends them back
   * to the requesting peer in appropriately sized batches.
   *
   * Algorithm:
   * 1. Check if the requested modifier type is a special case (input block related)
   * 2. For special cases, delegate to specific handling methods
   * 3. For regular modifiers, retrieve from history or mempool based on type
   * 4. Split the response into appropriately sized batches to comply with message size limits
   * 5. Send the batches to the requesting peer
   *
   * @param hr The history reader interface
   * @param mp The mempool reader interface
   * @param invData The inventory data containing requested modifier IDs and type
   * @param remote The peer requesting the modifiers
   */
  protected def modifiersReq(hr: ErgoHistory, mp: ErgoMemPool, invData: InvData, remote: ConnectedPeer): Unit = {
    if (invData.typeId == InputBlockTypeId.value) {
      invData.ids.foreach { id =>
        processInputBlockRequest(id, hr, remote)
      }
    } else if (invData.typeId == InputBlockTransactionIdsTypeId.value) {
      invData.ids.foreach { id =>
        processInputBlockTransactionIdsRequest(id, hr, remote)
      }
    } else if (invData.typeId == OrderingBlockAnnouncementTypeId.value) {
      invData.ids.foreach { id =>
        processOrderingBlockAnnouncementRequest(id, hr, remote)
      }
    } else {
      val objs: Seq[(ModifierId, Array[Byte])] = invData.typeId match {
        case typeId: NetworkObjectTypeId.Value if typeId == ErgoTransaction.modifierTypeId =>
          mp.getAll(invData.ids).map { unconfirmedTx =>
            unconfirmedTx.transaction.id -> unconfirmedTx.transactionBytes.getOrElse(unconfirmedTx.transaction.bytes)
          }
        case expectedTypeId: NetworkObjectTypeId.Value =>
          invData.ids.flatMap { id =>
            hr.modifierTypeAndBytesById(id).flatMap { case (mTypeId, bytes) =>
              if (mTypeId == expectedTypeId) {
                Some(id -> bytes)
              } else {
                log.debug(s"Improper type for asked modifier id: $id")
                None
              }
            }
          }
      }

      log.debug(s"Requested ${invData.ids.length} modifiers ${idsToString(invData)}, " +
                s"sending ${objs.length} modifiers ${idsToString(invData.typeId, objs.map(_._1))} ")

      @tailrec
      def sendByParts(mods: Seq[(ModifierId, Array[Byte])]): Unit = {
        var size = 5 //message type id + message size
        var batch = mods.takeWhile { case (_, modBytes) =>
          size += ErgoNodeViewModifier.ModifierIdSize + 4 + modBytes.length
          size < ModifiersSpec.maxMessageSize
        }
        if (batch.isEmpty) {
          // send modifier anyway
          val ho = mods.headOption
          batch = ho.toSeq
          log.warn(s"Sending too big modifier ${ho.map(_._1)}, its size ${ho.map(_._2.length)}")
        }
        remote.handlerRef ! Message(ModifiersSpec, Right(ModifiersData(invData.typeId, batch.toMap)), None)
        val remaining = mods.drop(batch.length)
        if (remaining.nonEmpty) {
          sendByParts(remaining)
        }
      }

      if (objs.nonEmpty) {
        sendByParts(objs)
      }
    }
  }

  // PROCESS LOGIC FOR INPUT- AND ORDERING BLOCKS RELATED DATA

  /**
   * Cache to store input block transaction differences temporarily while waiting for
   * missing transactions to be received from peers.
   * Key: input block id
   * Value: InputBlockDiffData containing creation time, weak transaction IDs, and cached transactions
   *
   * Entries are automatically cleaned up after LocalInputBlockChunksTTL (10 minutes) to prevent memory exhaustion.
   */
  private val localInputBlockChunks = mutable.Map[ModifierId, ErgoNodeViewSynchronizer.InputBlockDiffData]()

  /**
   * Cleanup old entries from localInputBlockChunks cache.
   * 
   * This method removes entries that have been in the cache longer than LocalInputBlockChunksTTL.
   * It should be called periodically to prevent memory exhaustion from stale entries.
   * 
   * Algorithm:
   * 1. Calculate the cutoff time (current time - TTL)
   * 2. Filter out entries older than the cutoff time
   * 3. Log the number of cleaned entries for monitoring
   */
  private def cleanupLocalInputBlockChunks(): Unit = {
    val now = System.currentTimeMillis()
    val cutoffTime = now - ErgoNodeViewSynchronizer.LocalInputBlockChunksTTL.toMillis

    val oldEntries = localInputBlockChunks.filter { case (_, data) =>
      data.created < cutoffTime
    }

    if (oldEntries.nonEmpty) {
      oldEntries.keys.foreach { id =>
        localInputBlockChunks.remove(id)
      }
      log.debug(s"Cleaned up ${oldEntries.size} expired local input block chunk entries")
    }
  }

  private def weakIdsDiff(mp: ErgoMemPoolReader,
                          wIds: Seq[WeakId]): (Seq[WeakId], Seq[ErgoTransaction]) = {
    val mempoolTxs = wIds.flatMap(mp.transactionByWeakId)
    val diffIds = if (mempoolTxs.length == wIds.length) {
      Seq.empty[WeakId]
    } else {
      val mempoolIds = mempoolTxs.map(_.weakId)
      wIds.filter(wId => !mempoolIds.exists(mId => mId.sameElements(wId)))
    }
    diffIds -> mempoolTxs
  }

  // INPUT BLOCKS RELATED LOGIC

  /**
   * Request an input block from a peer by its ID.
   *
   * This method sends a request to the specified peer to download an input block with the given ID.
   * Input blocks are part of Ergo's two-tier blockchain architecture and contain transactions
   * that reference ordering blocks.
   *
   * @param sbId The ID of the input block to request
   * @param remote The peer to request the input block from
   */
  def requestInputBlock(sbId: ModifierId, remote: ConnectedPeer): Unit = {
    // currently we request input block only once // todo: recheck this
    val msg = Message(RequestModifierSpec, Right(InvData(InputBlockTypeId.value, Seq(sbId))), None)
    networkControllerRef ! SendToNetwork(msg, SendToPeer(remote))
  }

  /**
   * Request transaction IDs for an input block from a peer.
   *
   * This method sends a request to the specified peer to download the transaction IDs
   * associated with the given input block. This is used when an input block is received
   * without transaction IDs, allowing the node to request them separately.
   *
   * @param inputBlockInfo The input block information to request transaction IDs for
   * @param remote The peer to request the transaction IDs from
   */
  def requestInputBlockTransactionIds(inputBlockInfo: InputBlockInfo, remote: ConnectedPeer): Unit = {
    // currently we request input block transactions only once // todo: recheck this
    val data = InvData(InputBlockTransactionIdsTypeId.value, Seq(inputBlockInfo.header.id))
    val msg = Message(RequestModifierSpec, Right(data), None)
    networkControllerRef ! SendToNetwork(msg, SendToPeer(remote))
  }


  /**
   * Process an input block received from a peer.
   *
   * This method handles the validation and processing of input blocks, which are part of Ergo's
   * two-tier blockchain architecture. Input blocks contain transactions that reference ordering blocks.
   * The method performs PoW validation, processes transaction differences with the local mempool,
   * and coordinates with the node view holder to apply the input block.
   *
   * Algorithm:
   * 1. Validate the input block height against the current full block height
   * 2. Check PoW validity of the input block
   * 3. Handle different cases based on whether transaction IDs are announced:
   *    - If transaction IDs are provided, calculate difference with local mempool
   *    - If all transactions are available locally, process immediately
   *    - If some transactions are missing, request them from the peer
   *    - If no transaction IDs are provided, request them separately
   * 4. Handle edge cases where the input block references a future ordering block
   *
   * @param inputBlockInfo The input block information to process
   * @param hr The history reader interface
   * @param mp The mempool reader interface
   * @param remote The peer that sent the input block
   */
  def processInputBlock(inputBlockInfo: InputBlockInfo,
                        hr: ErgoHistoryReader,
                        mp: ErgoMemPoolReader,
                        remote: ConnectedPeer,
                        usrOpt: Option[UtxoStateReader]): Unit = {

    // Input blocks are only useful when nearly synced (within 2 blocks)
    // If we're far behind, ignore them and continue with normal header/block sync
    if (inputBlockInfo.header.height > hr.fullBlockHeight + 2) {
      //todo: change to .debug before release
      log.info(s"Ignoring input block at height ${inputBlockInfo.header.height}, our full block height is ${hr.fullBlockHeight} (gap > 2 blocks)")
      return
    }

    val subBlockHeader = inputBlockInfo.header
    val subBlockId = inputBlockInfo.id

    // apply sub-block if it is on current height // todo: relax the rule to process input-blocks for last 1-2 ordering blocks as well ?
    if (subBlockHeader.height == hr.fullBlockHeight + 1) {
      val powScheme = settings.chainSettings.powScheme
      // todo : for digest mode, input-blocks validation is skipped here, however, in digest mode they
      //  should not be broadcasted to digest mode peers and accepted by them at all
      val valid = usrOpt
        .map(_.stateContext.currentParameters)
        .map(ps => inputBlockInfo.valid(powScheme, ps))
        .getOrElse(true)
      if (valid) { // check PoW / Merkle proofs before processing todo: check diff
        val prevSbIdOpt = inputBlockInfo.prevInputBlockId // link to previous sub-block
        val weakTxIdsOpt = inputBlockInfo.weakTxIds

        log.info(s"Processing valid sub-block $subBlockId with parent sub-block $prevSbIdOpt and parent block ${subBlockHeader.parentId}, weak txs announced: ${weakTxIdsOpt.map(_.length)}")

        weakTxIdsOpt match {
          case Some(wIds) =>
            // tx ids announced, calc diff with the mempool immediately
            val (diff, mempoolTxs) = weakIdsDiff(mp, wIds)
            if (diff.isEmpty) {
              // all the txs found or wIds empty, process immediately

              // todo: make it debug before release
              log.info(s"Diff is empty $subBlockId , processing immediately")

              // write sub-block and transactions to db
              viewHolderRef ! ProcessInputBlock(inputBlockInfo, remote)
              val transactionsData = InputBlockTransactionsData(inputBlockInfo.id, mempoolTxs)
              viewHolderRef ! ProcessInputBlockTransactions(transactionsData)
            } else {
              // in the first place, ask peer announced input-block for diff

              // Store the diff in cache while waiting for missing transactions from peer
              val ibdd = InputBlockDiffData(System.currentTimeMillis(), wIds, mempoolTxs)
              localInputBlockChunks.put(subBlockId, ibdd)

              val req = InputBlockTransactionsRequest(inputBlockInfo.id, diff)

              // todo: make it debug before release
              log.info(s"Diff is abt ${diff.length} transactions, asking them from $remote")

              val msg = Message(InputBlockTransactionsRequestMessageSpec, Right(req), None)
              networkControllerRef ! SendToNetwork(msg, SendToPeer(remote))

              // write sub-block and transactions to db
              viewHolderRef ! ProcessInputBlock(inputBlockInfo, remote)
            }

          case None =>
            // input block coming with no transaction ids announced

            // write sub-block to db
            viewHolderRef ! ProcessInputBlock(inputBlockInfo, remote)

            // todo: make it debug before release
            log.info(s"No transactions announced for ${subBlockId}, asking for transacion ids from $remote")

            // ask for transaction ids
            requestInputBlockTransactionIds(inputBlockInfo, remote)
        }
      } else {
        log.warn(s"Sub-block ${subBlockHeader.id} is invalid")
        penalizeMisbehavingPeer(remote)
      }
    } else {
      if (subBlockHeader.height == hr.fullBlockHeight + 2) {
        // if we receive sub-block after ordering block which is not known but has better height than us (by one,
        // so probably child of our best block), download ordering block ASAP

        val orderingId = inputBlockInfo.header.parentId

        // todo: save input block?

        // todo: make it debug before release
        log.info(s"On processing $subBlockId, downloading its parent and unknown ordering block $orderingId from $remote")

        val hid = Header.modifierTypeId
        if (deliveryTracker.status(orderingId, hid, Seq(hr)) == ModifiersStatus.Unknown) {
          requestBlockSection(hid, Seq(orderingId), remote)
        }
      } else {
        log.info(s"Got sub-block for height ${subBlockHeader.height}, while height of our best full-block is ${hr.fullBlockHeight} : ${subBlockHeader.id}")
        // just ignore the subblock
      }
    }
  }

  /**
   * Process a request from a peer for an input block by its ID.
   *
   * This method handles requests from peers for specific input blocks. If the input block
   * exists in local storage, it is sent back to the requesting peer. Otherwise, a warning
   * is logged indicating the block was not found.
   *
   * @param subBlockId The ID of the requested input block
   * @param hr The history reader interface
   * @param remote The peer requesting the input block
   */
  def processInputBlockRequest(subBlockId: ModifierId, hr: ErgoHistoryReader, remote: ConnectedPeer): Unit = {
    hr.getInputBlock(subBlockId) match {
      case Some(sbi) =>

        // todo: make it debug before release
        log.info(s"Serving input-block data for $subBlockId requested by $remote")

        val msg = Message(InputBlockMessageSpec, Right(sbi), None)
        networkControllerRef ! SendToNetwork(msg, SendToPeer(remote))
      case None =>
        log.warn(s"Requested by $remote sub block not found: $subBlockId")
    }
  }

  /**
   * Process a request from a peer for transaction IDs associated with an input block.
   *
   * This method handles requests from peers for the weak transaction IDs associated with
   * a specific input block. If the IDs exist in local storage, they are sent back to
   * the requesting peer. Otherwise, a warning is logged.
   *
   * @param subblockId The ID of the input block to get transaction IDs for
   * @param hr The history reader interface
   * @param remote The peer requesting the transaction IDs
   */
  def processInputBlockTransactionIdsRequest(subblockId: ModifierId, hr: ErgoHistoryReader, remote: ConnectedPeer): Unit = {
    hr.getInputBlockTransactionWeakIds(subblockId) match {
      case Some(ids) =>

        // todo: make it debug before release
        log.info(s"Serving input-block tx ids for ${subblockId} requested by $remote")

        val data = InputBlockTransactionIdsData(subblockId, ids)
        val msg = Message(InputBlockTransactionIdsMessageSpec, Right(data), None)
        networkControllerRef ! SendToNetwork(msg, SendToPeer(remote))
      case None =>
        log.warn(s"Requested by $remote weak ids not found for: $subblockId")
    }
  }

  /**
   * Process a request from a peer for an ordering block announcement by its ID.
   *
   * This method handles requests from peers for specific ordering block announcements.
   * If the announcement exists in local storage, it is sent back to the requesting peer.
   * Otherwise, a warning is logged indicating the announcement was not found.
   *
   * @param id The ID of the requested ordering block announcement
   * @param hr The history reader interface
   * @param remote The peer requesting the announcement
   */
  def processOrderingBlockAnnouncementRequest(id: ModifierId, hr: ErgoHistoryReader, remote: ConnectedPeer): Unit = {
    hr.getOrderingBlockAnnouncement(id) match {
      case Some(obAnn) =>
        log.info(s"Serving ordering block announcement w. $id requested by $remote")
        val msg = Message(OrderingBlockAnnouncementMessageSpec, Right(obAnn), None)
        networkControllerRef ! SendToNetwork(msg, SendToPeer(remote))
      case None =>
        log.warn(s"Requested by $remote weak ids not found for: $id")
    }
  }

  /**
   * Process input block transaction IDs received from a peer.
   *
   * This method handles the receipt of transaction IDs for an input block from a peer.
   * It calculates the difference between the received IDs and what's available in the
   * local mempool, then either processes the input block immediately if all transactions
   * are available, or requests the missing transactions from the peer.
   *
   * @param txIds The input block transaction IDs data received from peer
   * @param mp The mempool reader interface
   * @param remote The peer that sent the transaction IDs
   */
  def processInputBlockTransactionIds(txIds: InputBlockTransactionIdsData, mp: ErgoMemPoolReader, remote: ConnectedPeer): Unit = {
    val subBlockId = txIds.inputBlockId
    val wIds = txIds.transactionIds
    val (diff, mempoolTxs) = weakIdsDiff(mp, wIds)

    // todo: make it debug before release
    log.info(s"Processing input-block tx ids for ${subBlockId}")


    // todo: the code below is similar to processInputBlock, aside of sending inputBlock to ENVH, fix boilerplate
    if (diff.isEmpty) {
      // all the txs found or wIds empty, process immediately

      // write sub-block and transactions to db
      val transactionsData = InputBlockTransactionsData(subBlockId, mempoolTxs)
      viewHolderRef ! ProcessInputBlockTransactions(transactionsData)
    } else {
      // in the first place, ask peer announced input-block for diff

      // Store the diff in cache while waiting for missing transactions from peer
      val ibdd = InputBlockDiffData(System.currentTimeMillis(), wIds, mempoolTxs)
      localInputBlockChunks.put(subBlockId, ibdd)

      val req = InputBlockTransactionsRequest(subBlockId, diff)

      val msg = Message(InputBlockTransactionsRequestMessageSpec, Right(req), None)
      networkControllerRef ! SendToNetwork(msg, SendToPeer(remote))
    }
  }

  /**
   * Process a request from a peer for specific input block transactions.
   *
   * This method handles requests from peers for specific transactions associated with
   * an input block. It retrieves the requested transactions from local storage and
   * sends them back to the requesting peer. If the transactions are not found,
   * a warning is logged.
   *
   * @param req The request containing the input block ID and transaction IDs
   * @param hr The history reader interface
   * @param remote The peer requesting the transactions
   */
  def processInputBlockTransactionsRequest(req: InputBlockTransactionsRequest, hr: ErgoHistoryReader, remote: ConnectedPeer): Unit = {
    val subBlockId = req.inputBlockId

    // todo: make it debug before release
    log.info(s"Serving input-block txs for ${subBlockId} requested by $remote")

    // other peer is sending us weak ids of transactions it doesnt have, we serve it with them
    hr.getInputBlockTransactions(subBlockId, req.txIds) match {
      case Some(transactions) =>
        val std = InputBlockTransactionsData(subBlockId, transactions)
        val msg = Message(InputBlockTransactionsMessageSpec, Right(std), None)
        networkControllerRef ! SendToNetwork(msg, SendToPeer(remote))
      case None =>
        log.warn(s"Transactions not found for requested sub block $subBlockId")
    }
  }

  /**
   * Process input block transactions received from a peer.
   *
   * This method combines input block transactions received from a peer with locally cached
   * transactions from the mempool, then sends the complete set for processing. It handles
   * the reconstruction of the full transaction set for an input block by combining locally
   * available transactions with those received from peers.
   *
   * Algorithm:
   * 1. Check if there are locally cached transaction differences for this input block
   * 2. If no local transactions are cached, process the received transactions directly
   * 3. If local transactions exist, merge them with received transactions by matching
   *    against the expected weak transaction IDs
   * 4. Verify all expected transactions are present before forwarding for processing
   *
   * @param transactionsData The input block transaction data received from peer
   * @param hr The history reader interface
   * @param remote The peer that sent the transactions
   */
  def processInputBlockTransactions(transactionsData: InputBlockTransactionsData,
                                    hr: ErgoHistoryReader,
                                    remote: ConnectedPeer): Unit = {

    // todo: check if not spam, ie transaction were requested

    // we combine input block transactionsgot from a peer with mempool (cached before), and send result for processing

    val subBlockId = transactionsData.inputBlockId
    val localTxsOpt = localInputBlockChunks.get(subBlockId)

    val localTxsLength = localTxsOpt.map(_.weakTxsIds.length).getOrElse(0)

    // todo: make it debug before release
    log.info(s"Processing input-block txs for $subBlockId , local txs: ${localTxsLength}, external txs: ${transactionsData.transactions.length}")

    if (localTxsLength == 0) {
      viewHolderRef ! ProcessInputBlockTransactions(transactionsData)
    } else {
      val localTxsData = localTxsOpt.get // get is safe when localTxsLength > 0
      val weakTxIds = localTxsData.weakTxsIds
      val totalTxs = weakTxIds.length
      val resTxs = new Array[ErgoTransaction](totalTxs)

      var allTxs = mutable.Seq[ErgoTransaction]()
      allTxs ++= localTxsData.txs // mempoool txs
      allTxs ++= transactionsData.transactions // peer txs

      var allFound = true
      cfor(0)(_ < totalTxs, _ + 1) { i =>
        val weakId = weakTxIds(i)
        allTxs.find(_.weakId.sameElements(weakId)) match {
          case Some(tx) =>
            resTxs(i) = tx
          case None =>
            log.warn(s"Transaction with weakId ${Algos.encode(weakId)} not found for input block $subBlockId")
            allFound = false
        }
      }

      if (allFound) {
        val res = InputBlockTransactionsData(subBlockId, resTxs)
        viewHolderRef ! ProcessInputBlockTransactions(res)
      } else {
        log.warn(s"Not all transactions found for input block $subBlockId, skipping processing")
        // todo: penalizeMisbehavingPeer(remote)
      }
    }
  }

  /**
   * Process an ordering block announcement received from a peer.
   *
   * This method handles ordering block announcements, which are part of Ergo's two-tier
   * blockchain architecture. It validates the announcement, stores it locally, and forwards
   * it to appropriate peers. The method also determines whether to process the ordering block
   * directly or request the full block depending on whether referenced input blocks are available.
   *
   * Algorithm:
   * 1. Check if we're nearly synced (ordering blocks are only useful when within 2 blocks of sync)
   * 2. Validate the ordering block announcement against the PoW scheme
   * 3. Store the announcement in the history reader
   * 4. Forward the announcement to peers that support sub-blocks and have compatible status
   * 5. Check if referenced input blocks are available in local storage
   * 6. If input blocks are available, process the ordering block directly
   * 7. If input blocks are missing, request the full block sections instead
   *
   * @param oba The ordering block announcement to process
   * @param hr The history reader interface
   * @param remote The peer that sent the announcement
   */
  private def processOrderingBlockAnnouncement(oba: OrderingBlockAnnouncement,
                                               hr: ErgoHistoryReader,
                                               remote: ConnectedPeer): Unit = {

    // Ordering blocks are only useful when nearly synced (within 2 blocks)
    // If we're far behind, ignore the announcement and continue with normal header/block sync
    if (oba.header.height > hr.fullBlockHeight + 2) {
      log.debug(s"Ignoring ordering block announcement at height ${oba.header.height}, our full block height is ${hr.fullBlockHeight} (gap > 2 blocks)")
      return
    }

    //todo : make debug
    log.info(s"Processing ordering block announcement for ${oba.header.id}")

    if (!hr.contains(oba.header.id)) {

      if (!oba.valid(settings.chainSettings.powScheme)) {
        penalizeMisbehavingPeer(remote)
        return
      }

      hr.storeOrderingBlockAnnouncement(oba)

      // Send ordering block announcement to peers supporting sub-blocks and having equal or forked status
      // Also check that peers are nearly synced (within 2 blocks)
      val peers = syncTracker.statuses.filter { s =>
        val status = s._2.status
        val peerHeight = s._2.height
        // send ordering block announcement to peers on same height and also supporting sub-blocks
        // Don't send to peers that are far behind (> 2 blocks gap)
        SubBlocksFilter.condition(s._1) && 
          (status == Equal || status == Fork) &&
          (peerHeight <= hr.fullBlockHeight + 2)
      }.keys.toSeq
      
      if (peers.nonEmpty) {
        // announce id via inv message
        val invData = InvData(OrderingBlockAnnouncementTypeId.value, Seq(oba.header.id))
        val msg = Message(InvSpec, Right(invData), None)
        networkControllerRef ! SendToNetwork(msg, SendToPeers(peers))
      }

      // todo: for now, we just check if referenced input block is stored
      // todo: if so, input blocks are used, otherwise, full block is downloaded
      // todo: instead, missing input blocks should be downloaded

      val prevInputBlockIdOpt = oba.extensionFields.find(_._1.sameElements(PrevInputBlockIdKey))

      log.info(s"On processing ordering block ${oba.header.id}, it is last input block ${prevInputBlockIdOpt}")

      val inputBlockStored = prevInputBlockIdOpt.map { t =>
        hr.getInputBlockTransactions(bytesToId(t._2)).isDefined
      }.getOrElse(true)

      if (inputBlockStored) {
        log.info(s"Processing ordering block ${oba.header.id}") // todo: make it .debug
        viewHolderRef ! ProcessOrderingBlock(oba)
      } else {
        // todo: request full block for now, see todo notes above
        log.info(s"Requesting all the block transactions for ${oba.header.id} as prev input block not found")
        val ext = Extension(oba.header.id, oba.extensionFields)
        viewHolderRef ! ModifiersFromRemote(Seq(ext))
        requestBlockSection(BlockTransactions.modifierTypeId, Array(oba.header.transactionsId), remote)
      }
    } else {
      // todo: make .debug before release
      log.info(s"Ignoring ordering block announcement as it is already known: ${oba.header.id}")
    }
  }

  /**
    * Move `pmod` to `Invalid` if it is permanently invalid, to `Received` otherwise
    */
  @SuppressWarnings(Array("org.wartremover.warts.IsInstanceOf"))
  def validateAndSetStatus(hr: ErgoHistory, remote: ConnectedPeer, pmod: BlockSection): Boolean = {
    hr.applicableTry(pmod) match {
      case Failure(e) if e.isInstanceOf[MalformedModifierError] =>
        log.warn(s"Modifier ${pmod.encodedId} is permanently invalid", e)
        deliveryTracker.setInvalid(pmod.id, pmod.modifierTypeId)
        penalizeMisbehavingPeer(remote)
        false
      case _ =>
        deliveryTracker.setReceived(pmod.id, pmod.modifierTypeId, remote)
        true
    }
  }

  /**
   * Scheduler asking node view synchronizer to check whether requested modifiers have been delivered.
   * Do nothing, if modifier is already in a different state (it might be already received, applied, etc.),
   * wait for delivery until the number of checks exceeds the maximum if the peer sent `Inv` for this modifier
   * re-request modifier from a different random peer, if our node does not know a peer who have it
   */
  protected def checkDelivery(hr: ErgoHistory): Receive = {
    case CheckDelivery(peer, modifierTypeId, modifierId) =>
      if (deliveryTracker.status(modifierId, modifierTypeId, Seq.empty) == ModifiersStatus.Requested) {
        // If transaction not delivered on time, we just forget about it.
        // It could be removed from other peer's mempool, so no reason to penalize the peer.
        if (modifierTypeId == ErgoTransaction.modifierTypeId) {
          deliveryTracker.clearStatusForModifier(modifierId, modifierTypeId, ModifiersStatus.Requested)
        } else {
          // A block section is not delivered on time.
          log.info(s"Peer ${peer.toString} has not delivered network object " +
                   s"$modifierTypeId : ${ScorexEncoder.encodeId(modifierId)} on time")

          // Number of delivery checks for a block section, utxo set snapshot chunk or manifest
          // increased or initialized, except the case where we can have issues with connectivity,
          // which is currently defined by comparing request time with time the
          // node got last modifier (in future we may consider more precise method)
          val checksDone = deliveryTracker.getRequestedInfo(modifierTypeId, modifierId) match {
            case Some(ri) if ri.requestTime < lastModifierGotTime =>
              ri.checks
            case Some(ri) =>
              penalizeNonDeliveringPeer(peer)
              ri.checks + 1
            case None => 0
          }

          val maxDeliveryChecks = networkSettings.maxDeliveryChecks
          if (checksDone < maxDeliveryChecks) {
            if (modifierTypeId == UtxoSnapshotChunkTypeId.value) {
              // randomly choosing a peer to download UTXO set snapshot chunk
              val newPeerOpt = hr.randomPeerToDownloadChunks()
              log.info(s"Rescheduling request for UTXO set chunk $modifierId , new peer $newPeerOpt")
              deliveryTracker.setUnknown(modifierId, modifierTypeId)
              newPeerOpt match {
                case Some(newPeer) => requestUtxoSetChunk(Digest32 @@ Algos.decode(modifierId).get, newPeer)
                case None => log.warn(s"No peer found to download UTXO set chunk $modifierId")
              }
            } else if (modifierTypeId == InputBlockTypeId.value ||
                        modifierTypeId == InputBlockTransactionIdsTypeId.value ||
                        modifierTypeId == OrderingBlockAnnouncementTypeId.value) {
              deliveryTracker.setUnknown(modifierId, modifierTypeId)
              if (modifierTypeId == InputBlockTypeId.value && checksDone < 2) {
                log.info(s"re-requesting input block $modifierId")
                requestInputBlock(modifierId, peer)
              } else {
                log.info(s"re-requesting input txs $modifierId")
                hr.getInputBlock(modifierId).foreach { ibi =>
                  requestInputBlockTransactionIds(ibi, peer)
                }
              }
            } else {
              // randomly choose a peer for another block sections download attempt
              val newPeerCandidates: Seq[ConnectedPeer] = if (modifierTypeId == Header.modifierTypeId) {
                getPeersForDownloadingHeaders(peer).toSeq
              } else {
                getPeersForDownloadingBlocks.map(_.toSeq).getOrElse(Seq(peer))
              }
              val newPeerIndex = scala.util.Random.nextInt(newPeerCandidates.size)
              val newPeer = newPeerCandidates(newPeerIndex)
              log.info(s"Rescheduling request for $modifierId , new peer $newPeer")
              deliveryTracker.setUnknown(modifierId, modifierTypeId)
              requestBlockSection(modifierTypeId, Seq(modifierId), newPeer, checksDone)
            }
          } else {
            log.error(s"Exceeded max delivery attempts($maxDeliveryChecks) limit for $modifierId")
            if (modifierTypeId == Header.modifierTypeId) {
              // if we can not get header after max number of attempts, invalidate it
              log.info(s"Marking header as invalid: $modifierId")
              deliveryTracker.setInvalid(modifierId, modifierTypeId)
            } else {
              // we will stop to ask for non-header block section automatically after some time,
              // see how `nextModifiersToDownload` done in `ToDownloadProcessor`
              deliveryTracker.setUnknown(modifierId, modifierTypeId)
            }
          }
        }
      }
  }

  protected def penalizeNonDeliveringPeer(peer: ConnectedPeer): Unit = {
    networkControllerRef ! PenalizePeer(peer.connectionId.remoteAddress, PenaltyType.NonDeliveryPenalty)
  }

  protected def penalizeSpammingPeer(peer: ConnectedPeer): Unit = {
    networkControllerRef ! PenalizePeer(peer.connectionId.remoteAddress, PenaltyType.SpamPenalty)
  }

  protected def penalizeMisbehavingPeer(peer: ConnectedPeer): Unit = {
    networkControllerRef ! PenalizePeer(peer.connectionId.remoteAddress, PenaltyType.MisbehaviorPenalty)
  }

  override protected def penalizeMaliciousPeer(peer: ConnectedPeer): Unit = {
    networkControllerRef ! PenalizePeer(peer.connectionId.remoteAddress, PenaltyType.PermanentPenalty)
  }

  protected def peerManagerEvents: Receive = {
    case HandshakedPeer(remote) =>
      syncTracker.updateStatus(remote, status = Unknown, height = None)

    case DisconnectedPeer(connectedPeer) =>
      syncTracker.clearStatus(connectedPeer)
  }

  protected def sendLocalSyncInfo(historyReader: ErgoHistory): Receive = {
    case SendLocalSyncInfo =>
      sendSync(historyReader)
  }


  protected def processDataFromPeer(msgHandlers: PartialFunction[(MessageSpec[_], _, ConnectedPeer), Unit]): Receive = {
    case Message(spec, Left(msgBytes), Some(source)) => parseAndHandle(msgHandlers, spec, msgBytes, source)
  }

  // helper method to clear declined transactions after some off, so the node may accept them again
  private def clearDeclined(): Unit = {
    val clearTimeout = FiniteDuration(20, MINUTES)
    val now = System.currentTimeMillis()

    val toRemove = declined.filter { case (_, time) =>
      (now - time) > clearTimeout.toMillis
    }
    log.debug(s"Declined transactions to be cleared: ${toRemove.size}")
    toRemove.foreach { case (id, _) =>
      declined.remove(id)
    }
  }

  // check if we have enough UTXO set snapshots for some height
  // if so, request manifest from a random peer announced it
  private def checkUtxoSetManifests(historyReader: ErgoHistory): Unit = {

    if (settings.nodeSettings.utxoSettings.utxoBootstrap &&
          historyReader.fullBlockHeight == 0 &&
          availableManifests.nonEmpty &&
          historyReader.utxoSetSnapshotDownloadPlan().isEmpty) {
      val res = availableManifests.filter { case (_, (_, peers)) => peers.length >= MinSnapshots }
      if (res.nonEmpty) {
        val(encModifierId, (height, peers)) = res.maxBy(_._2._1)
        log.info(s"Downloading manifest for height $height from ${peers.size} peers")
        val manifestId = Digest32 @@ Algos.decode(encModifierId).get
        val randomPeer = peers(Random.nextInt(peers.length))
        requestManifest(manifestId, randomPeer)
      } else {
        log.info("No manifests to download found ")
      }
    }
  }


  /**
   * Handler for messages from the node view holder, coordinating the synchronization of node state.
   *
   * This method handles various events from the node view holder including block applications,
   * transaction processing results, state changes, and cache updates. It manages the coordination
   * between the network layer and the node's internal state, including requesting more modifiers
   * when needed, broadcasting new blocks, and maintaining transaction caches.
   *
   * Key responsibilities:
   * - Requesting more modifiers when the download queue is low
   * - Broadcasting locally generated blocks to appropriate peers
   * - Processing transaction acceptance/rejection outcomes
   * - Handling state changes and cache updates
   * - Managing input block broadcasting for sub-blocks architecture
   * - Coordinating with delivery tracker for modifier status updates
   *
   * @param historyReader Interface to read historical blockchain data
   * @param mempoolReader Interface to read mempool data
   * @param utxoStateReaderOpt Optional interface to read UTXO state data
   * @param blockAppliedTxsCache Cache of recently applied transaction IDs
   * @return A partial function handling various node view holder messages
   */
  private def viewHolderEvents(historyReader: ErgoHistory,
                               mempoolReader: ErgoMemPool,
                               utxoStateReaderOpt: Option[UtxoStateReader],
                               blockAppliedTxsCache: FixedSizeApproximateCacheQueue): Receive = {
    // Requests BlockSections with `Unknown` status that are defined by block headers but not downloaded yet.
    // Trying to keep size of requested queue equals to `desiredSizeOfExpectingQueue`.
    case CheckModifiersToDownload =>
      val now = System.currentTimeMillis()
      if (now - lastCheckForModifiersToDownload >= 50) { // do not process command more often than every 50 ms
        lastCheckForModifiersToDownload = now
        requestDownload(
          maxModifiers = deliveryTracker.modifiersToDownload,
          minModifiersPerBucket,
          maxModifiersPerBucket
        )(getPeersForDownloadingBlocks) { howManyPerType =>
          historyReader.nextModifiersToDownload(howManyPerType, downloadRequired(historyReader))
        }
      }

    // If new enough semantically valid ErgoFullBlock was applied:
    // 1) send inv for block header and all its sections to peers not supporting input/ordering blocks
    // 2) send ordering block announcement to peers supporting input/ordering blocks
    // Note: Ordering blocks are only broadcast when nearly synced (within 2 blocks)
    case LocallyGeneratedOrderingBlock(efb, orderingBlockTransactions) =>
      val knownPeers = syncTracker.fullInfo()
      val sendOrderingTo = knownPeers.filter { peerStatus =>
        val peerHeight = peerStatus.height
        if (peerStatus.status == Equal || peerStatus.status == Fork) {
          peerStatus.peer.peerInfo.exists(_.peerSpec.protocolVersion >= Version.SubblocksVersion) &&
            peerHeight <= historyReader.fullBlockHeight + 2
        } else {
          false
        }
      }.map(_.peer)
      val header = efb.header
      // broadcast subblock announcement
      val ot = orderingBlockTransactions
      val ext = efb.extension

      //todo: make debug before release
      log.info(s"Sending locally generated ordering block ${efb.header.id} to ${sendOrderingTo.size} peers")

      // todo: send ids for previously broadcasted txs, not .empty
      val obAnn = {
        OrderingBlockAnnouncement(header, ot, Seq.empty, ext.fields)
      }
      historyReader.storeOrderingBlockAnnouncement(obAnn)
      val msg = Message(OrderingBlockAnnouncementMessageSpec, Right(obAnn), None)
      networkControllerRef ! SendToNetwork(msg, SendToPeers(sendOrderingTo.toSeq))

    case FullBlockApplied(header) =>
      if (historyReader.bestHeaderOpt.exists(_.height <= header.height)) {
        val knownPeers = syncTracker.fullInfo()

        // Split known peers into ones supporting input/ordering blocks and ones not
        val sendFullToStatuses = knownPeers.filter { peerStatus =>
          if (peerStatus.status == Equal || peerStatus.status == Fork) {
            peerStatus.peer.peerInfo.exists(_.peerSpec.protocolVersion >= Version.SubblocksVersion)
          } else {
            false
          }
        }

        val sendFullTo = sendFullToStatuses.map(_.peer)

        // todo: make debug
        log.info(s"Sending old format block sections to $sendFullTo")

        // send block sections in full for older peers not supporting sub-blocks
        if (sendFullTo.nonEmpty) {
          val peersOpt = Some(sendFullTo.toSeq)
          broadcastModifierInv(Header.modifierTypeId, header.id, peersOpt)
          header.sectionIds.foreach { case (mtId, id) => broadcastModifierInv(mtId, id, peersOpt) }
        }
      }
      clearDeclined()
      clearInterblockCost()
      perPeerCost.clear()
      processFirstTxProcessingCacheRecord() // resume cache processing

    case st@SuccessfulTransaction(utx) =>
      val tx = utx.transaction
      deliveryTracker.setHeld(tx.id, ErgoTransaction.modifierTypeId)
      processMempoolResult(st)
      broadcastModifierInv(tx)

    case dt@DeclinedTransaction(utx: UnconfirmedTransaction) =>
      declined.put(utx.id, System.currentTimeMillis())
      processMempoolResult(dt)

    case ft@FailedTransaction(utx, error) =>
      val id = utx.id
      processMempoolResult(ft)

      utx.source.foreach { peer =>
        // no need to call deliveryTracker.setInvalid, as mempool will consider invalidated tx in contains()
        error match {
          case TooHighCostError(_, _) =>
            log.info(s"Penalize spamming peer $peer for too costly transaction $id")
            penalizeSpammingPeer(peer)
          case _ =>
            log.info(s"Penalize peer $peer for too costly transaction $id (reason: $error)")
            penalizeMisbehavingPeer(peer)
        }
      }

    case FailedOnRecheckTransaction(id, _) =>
      declined.put(id, System.currentTimeMillis())

    case SyntacticallySuccessfulModifier(modTypeId, modId) =>
      deliveryTracker.setHeld(modId, modTypeId)

    case RecoverableFailedModification(modTypeId, modId, e) =>
      logger.debug(s"Setting recoverable failed modifier $modId as Unknown", e)
      deliveryTracker.setUnknown(modId, modTypeId)

    case SyntacticallyFailedModification(modTypeId, modId, e) =>
      logger.debug(s"Invalidating syntactically failed modifier $modId", e)
      deliveryTracker.setInvalid(modId, modTypeId).foreach(penalizeMisbehavingPeer)

    case SemanticallyFailedModification(modTypeId, modId, e) =>
      logger.debug(s"Invalidating semantically failed modifier $modId", e)
      deliveryTracker.setInvalid(modId, modTypeId).foreach(penalizeMisbehavingPeer)

    case ChangedHistory(newHistoryReader: ErgoHistory) =>
      context.become(initialized(newHistoryReader, mempoolReader, utxoStateReaderOpt, blockAppliedTxsCache))

    case ChangedMempool(newMempoolReader: ErgoMemPool) =>
      context.become(initialized(historyReader, newMempoolReader, utxoStateReaderOpt, blockAppliedTxsCache))

    case ChangedState(reader: ErgoStateReader) =>
      activatedScriptVersion = Header.scriptFromBlockVersion(reader.stateContext.blockVersion)
      reader match {
        case utxoStateReader: UtxoStateReader =>
          context.become(initialized(historyReader, mempoolReader, Some(utxoStateReader), blockAppliedTxsCache))
        case _ =>
      }

    case BlockSectionsProcessingCacheUpdate(headersCacheSize, blockSectionsCacheSize, cleared) =>
      val HeadersCacheSizeToDownloadMore = 3184
      val BlockSectionsCacheSizeToDownloadMore = 96

      //download more block sections if processing cache has enough space
      def downloadMore: Boolean = {
        headersCacheSize < HeadersCacheSizeToDownloadMore ||
          (modifiersCacheSize < BlockSectionsCacheSizeToDownloadMore &&
            (System.currentTimeMillis() - lastCheckForModifiersToDownload >= 50))
      }

      // stop processing for cleared modifiers
      // applied modifiers state was already changed at `SyntacticallySuccessfulModifier`
      val modTypeId = cleared._1
      cleared._2.foreach(mId => deliveryTracker.setUnknown(mId, modTypeId))
      modifiersCacheSize = blockSectionsCacheSize
      if (downloadMore) {
        requestMoreModifiers(historyReader)
      }

    case BlockAppliedTransactions(transactionIds: Seq[ModifierId]) =>
      // We collect applied TXs to history in order to avoid banning peers that sent these afterwards
      logger.debug("Caching applied transactions")
      context.become(initialized(historyReader, mempoolReader, utxoStateReaderOpt, blockAppliedTxsCache.putAll(transactionIds)))

    case ChainIsHealthy =>
      // good news
      logger.debug("Chain is good")

    case ChainIsStuck(error) =>
      if (historyReader.fullBlockHeight > 0) {
        log.warn(s"Chain is stuck! $error\nDelivery tracker State:\n$deliveryTracker\nSync tracker state:\n$syncTracker")
        deliveryTracker.reset()
      } else {
        log.debug("Got ChainIsStuck signal when no full-blocks applied yet")
      }

    // todo: broadcast only locally generated new best input block?
    case NewBestInputBlock(Some(id), local) =>
      historyReader.getInputBlock(id) match {
        case Some(preIbi) =>
          if(local) {
            log.debug(s"Sending locally generated input block $id out")

            // we propagate input block with transactions immediately if it has no more than 3 transactions
            // todo: check number of transactions on retrieval
            // todo: improve high/low bandwidth rules
            val ibi = if (preIbi.weakTxIds.size <= 3) {
              preIbi
            } else {
              preIbi.copy(weakTxIds = None)
            }
            val peers = syncTracker.statuses.filter { s =>
              val status = s._2.status
              // todo: send to ones in utxo mode only, send to height of ours minues one
              // send input block to peers on same height and also supporting sub-blocks
              SubBlocksFilter.condition(s._1) && (status == Equal || status == Fork)
            }.keys.toSeq
            val msg = Message(InputBlockMessageSpec, Right(ibi), None)
            networkControllerRef ! SendToNetwork(msg, SendToPeers(peers))
          } else {
            // todo: send only id out
          }
        case None =>
          // shouldnt be there by input block processing logic
          log.error(s"NewBestInputBlock arrived for unknown input block $id")
      }

    case NewBestInputBlock(None, _) =>
      // this signal is sent on ordering block application, nothing p2p layer should do
  }

  /** handlers of messages coming from peers */
  private def msgHandlers(hr: ErgoHistory,
                          mp: ErgoMemPool,
                          usrOpt: Option[UtxoStateReader],
                          blockAppliedTxsCache: FixedSizeApproximateCacheQueue
                         ): PartialFunction[(MessageSpec[_], _, ConnectedPeer), Unit] = {
    case (_: ErgoSyncInfoMessageSpec.type @unchecked, data: ErgoSyncInfo @unchecked, remote) =>
      processSync(hr, data, remote)
    case (_: InvSpec.type, data: InvData, remote) =>
      processInv(hr, mp, data, remote, blockAppliedTxsCache)
    case (_: RequestModifierSpec.type, data: InvData, remote) =>
      modifiersReq(hr, mp, data, remote)
    case (_: ModifiersSpec.type, data: ModifiersData, remote) =>
      modifiersFromRemote(hr, mp, data, remote, blockAppliedTxsCache)
    // UTXO snapshot related messages
    case (spec: MessageSpec[_], _, remote) if spec.messageCode == GetSnapshotsInfoSpec.messageCode =>
      usrOpt match {
        case Some(usr) => sendSnapshotsInfo(usr, remote)
        case None => log.warn(s"Asked for snapshot when UTXO set is not supported, remote: $remote")
      }
    case (spec: MessageSpec[_], data: SnapshotsInfo, remote) if spec.messageCode == SnapshotsInfoSpec.messageCode =>
      processSnapshotsInfo(hr, data, remote)
    case (_: GetManifestSpec.type, id: Array[Byte], remote) =>
      usrOpt match {
        case Some(usr) => sendManifest(Digest32 @@ id, usr, remote)
        case None => log.warn(s"Asked for snapshot when UTXO set is not supported, remote: $remote")
      }
    case (_: ManifestSpec.type, manifestBytes: Array[Byte], remote) =>
      processManifest(hr, manifestBytes, remote)
    case (_: GetUtxoSnapshotChunkSpec.type, subtreeId: Array[Byte], remote) =>
      usrOpt match {
        case Some(usr) => sendUtxoSnapshotChunk(Digest32 @@ subtreeId, usr, remote)
        case None => log.warn(s"Asked for snapshot when UTXO set is not supported, remote: $remote")
      }
    case (_: UtxoSnapshotChunkSpec.type, serializedChunk: Array[Byte], remote) =>
      usrOpt match {
        case Some(_) => processUtxoSnapshotChunk(serializedChunk, hr, remote)
        case None => log.warn(s"Asked for snapshot when UTXO set is not supported, remote: $remote")
      }
    // Nipopows related messages
    case (_: GetNipopowProofSpec.type, data: NipopowProofData, remote) =>
      sendNipopowProof(data, hr, remote)
    case (_: NipopowProofSpec.type , proofBytes: Array[Byte], remote) =>
      processNipopowProof(proofBytes, hr, remote)
    // Sub-blocks related messages
    case (_: InputBlockMessageSpec.type, subBlockInfo: InputBlockInfo, remote) =>
      processInputBlock(subBlockInfo, hr, mp, remote, usrOpt)
    case (_: InputBlockTransactionIdsMessageSpec.type, transactionIds: InputBlockTransactionIdsData, remote) =>
      processInputBlockTransactionIds(transactionIds, mp, remote)
    case (_: InputBlockTransactionsRequestMessageSpec.type, req: InputBlockTransactionsRequest, remote) =>
      processInputBlockTransactionsRequest(req, hr, remote)
    case (_: InputBlockTransactionsMessageSpec.type, transactions: InputBlockTransactionsData, remote) =>
      processInputBlockTransactions(transactions, hr, remote)
    case (_: OrderingBlockAnnouncementMessageSpec.type, oba: OrderingBlockAnnouncement, remote) =>
      processOrderingBlockAnnouncement(oba, hr, remote)
  }

  def initialized(hr: ErgoHistory,
                  mp: ErgoMemPool,
                  usr: Option[UtxoStateReader],
                  blockAppliedTxsCache: FixedSizeApproximateCacheQueue): PartialFunction[Any, Unit] = {
    processDataFromPeer(msgHandlers(hr, mp, usr, blockAppliedTxsCache)) orElse
      onDownloadRequest(hr) orElse
      sendLocalSyncInfo(hr) orElse
      viewHolderEvents(hr, mp, usr, blockAppliedTxsCache) orElse
      peerManagerEvents orElse
      checkDelivery(hr) orElse {
      case CleanupLocalInputBlockChunks =>
        cleanupLocalInputBlockChunks()
      case a: Any => log.error("Strange input: " + a)
    }
  }

  /** Wait until both historyReader and mempoolReader instances are received so actor can be operational */
  def initializing(hr: Option[ErgoHistory],
                   mp: Option[ErgoMemPool],
                   usr: Option[UtxoStateReader],
                   blockAppliedTxsCache: FixedSizeApproximateCacheQueue): PartialFunction[Any, Unit] = {
    case ChangedHistory(historyReader: ErgoHistory) =>
      mp match {
        case Some(mempoolReader) =>
          context.become(initialized(historyReader, mempoolReader, usr, blockAppliedTxsCache))
        case _ =>
          context.become(initializing(Option(historyReader), mp, usr, blockAppliedTxsCache))
      }
    case ChangedMempool(mempoolReader: ErgoMemPool) =>
      hr match {
        case Some(historyReader) =>
          context.become(initialized(historyReader, mempoolReader, usr, blockAppliedTxsCache))
        case _ =>
          context.become(initializing(hr, Option(mempoolReader), usr, blockAppliedTxsCache))
      }
    case ChangedState(reader: ErgoStateReader) =>
      reader match {
        case utxoStateReader: UtxoStateReader =>
          context.become(initializing(hr, mp, Some(utxoStateReader), blockAppliedTxsCache))
        case _ =>
          context.become(initializing(hr, mp, None, blockAppliedTxsCache))
      }
    case msg =>
      // Actor not initialized yet, scheduling message until it is
      context.system.scheduler.scheduleOnce(1.second, self, msg)
  }

  override def receive: Receive = initializing(None, None, None, FixedSizeApproximateCacheQueue.empty(cacheQueueSize = 5))

}

object ErgoNodeViewSynchronizer {

  private def props(networkControllerRef: ActorRef,
            viewHolderRef: ActorRef,
            syncInfoSpec: ErgoSyncInfoMessageSpec.type,
            settings: ErgoSettings,
            syncTracker: ErgoSyncTracker,
            deliveryTracker: DeliveryTracker)
           (implicit ex: ExecutionContext): Props =
    Props(new ErgoNodeViewSynchronizer(networkControllerRef, viewHolderRef, syncInfoSpec,
      settings, syncTracker, deliveryTracker))

  def make(viewHolderRef: ActorRef,
            syncInfoSpec: ErgoSyncInfoMessageSpec.type,
            settings: ErgoSettings,
            syncTracker: ErgoSyncTracker,
            deliveryTracker: DeliveryTracker)
           (implicit context: ActorRefFactory, ex: ExecutionContext): ActorRef => ActorRef =
    networkControllerRef => context.actorOf(props(networkControllerRef, viewHolderRef, syncInfoSpec, settings, syncTracker, deliveryTracker))

  /**
    * Container for aggregated costs of accepted, declined or invalidated transactions. Can be used to track global
    * state of total cost of transactions received (since last block processed), or per-peer state
    */
  case class IncomingTxInfo(acceptedCost: Int, declinedCost: Int, invalidatedCost: Int) {
    val totalCost: Int = acceptedCost + declinedCost + invalidatedCost
  }

  object IncomingTxInfo {
    def empty(): IncomingTxInfo = IncomingTxInfo(0, 0, 0)
  }

  /**
    * Transaction bytes and source peer to be recorded in a cache and processed later
    */
  class TransactionProcessingCacheRecord(val txBytes: Array[Byte], val source: ConnectedPeer)

  case object CheckModifiersToDownload

  /**
   * Message to trigger cleanup of old local input block chunks
   */
  case object CleanupLocalInputBlockChunks

  /**
   * TTL for local input block chunks cache.
   * Entries older than this will be cleaned up to prevent memory exhaustion.
   */
  val LocalInputBlockChunksTTL: FiniteDuration = 10.minutes

  /**
   * How often to run cleanup of old local input block chunks
   */
  val LocalInputBlockChunksCleanupInterval: FiniteDuration = 5.minutes

  /**
   * Data class for caching input block transaction differences
   * @param created timestamp when the entry was created
   * @param weakTxsIds weak transaction IDs
   * @param txs cached transactions
   */
  case class InputBlockDiffData(created: Long, weakTxsIds: Seq[ErgoTransaction.WeakId], txs: Seq[ErgoTransaction])

  /**
   * Serializers for block sections and transactions
   */
  val modifierSerializers: Map[NetworkObjectTypeId.Value, ErgoSerializer[_ <: ErgoNodeViewModifier]] =
    Map(Header.modifierTypeId -> HeaderSerializer,
      Extension.modifierTypeId -> ExtensionSerializer,
      BlockTransactions.modifierTypeId -> BlockTransactionsSerializer,
      ADProofs.modifierTypeId -> ADProofsSerializer,
      ErgoTransaction.modifierTypeId -> ErgoTransactionSerializer)
}


