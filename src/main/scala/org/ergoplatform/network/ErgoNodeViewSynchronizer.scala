package org.ergoplatform.network

import akka.actor.SupervisorStrategy.{Restart, Stop}
import akka.actor.{Actor, ActorInitializationException, ActorKilledException, ActorRef, ActorRefFactory, DeathPactException, OneForOneStrategy, Props}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.{ErgoTransactionSerializer, UnconfirmedTransaction}
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock}
import org.ergoplatform.nodeView.history.{ErgoSyncInfoV1, ErgoSyncInfoV2}
import org.ergoplatform.nodeView.history._
import org.ergoplatform.network.ErgoNodeViewSynchronizer.CheckModifiersToDownload
import org.ergoplatform.nodeView.ErgoNodeViewHolder.BlockAppliedTransactions
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfo, ErgoSyncInfoMessageSpec}
import org.ergoplatform.nodeView.mempool.{ErgoMemPool, ErgoMemPoolReader}
import org.ergoplatform.settings.{Constants, ErgoSettings}
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages._
import org.ergoplatform.nodeView.ErgoNodeViewHolder._
import scorex.core.consensus.{Equal, Fork, Nonsense, Older, Unknown, Younger}
import scorex.core.network.ModifiersStatus.Requested
import scorex.core.{ModifierTypeId, NodeViewModifier, PersistentNodeViewModifier, idsToString}
import scorex.core.network.NetworkController.ReceivableMessages.{PenalizePeer, RegisterMessageSpecs, SendToNetwork}
import org.ergoplatform.network.ErgoNodeViewSynchronizer.ReceivableMessages._
import org.ergoplatform.nodeView.state.ErgoStateReader
import org.ergoplatform.nodeView.wallet.ErgoWalletReader
import scorex.core.network.message.{InvSpec, MessageSpec, ModifiersSpec, RequestModifierSpec}
import scorex.core.network._
import scorex.core.network.message.{InvData, Message, ModifiersData}
import scorex.core.serialization.ScorexSerializer
import scorex.core.settings.NetworkSettings
import scorex.core.transaction.Transaction
import scorex.core.utils.{NetworkTimeProvider, ScorexEncoding}
import scorex.core.validation.MalformedModifierError
import scorex.util.{ModifierId, ScorexLogging}
import scorex.core.network.DeliveryTracker
import scorex.core.network.peer.PenaltyType
import scorex.core.transaction.state.TransactionValidation.TooHighCostError
import ErgoNodeViewSynchronizer.{IncomingTxInfo, TransactionProcessingCacheRecord}

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
                               timeProvider: NetworkTimeProvider,
                               syncTracker: ErgoSyncTracker,
                               deliveryTracker: DeliveryTracker
                              )(implicit ex: ExecutionContext)
  extends Actor with Synchronizer with ScorexLogging with ScorexEncoding {

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

  private val blockSectionsDownloadFilter = BlockSectionsDownloadFilter(settings.nodeSettings.stateType)

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
  private val MaxDeclined = 400

  /**
    * No more than this number of unparsed transactions can be cached
    */
  private val MaxProcessingTransactionsCacheSize = 50

  /**
    * Max cost of transactions we are going to process between blocks
    */
  private val MempoolCostPerBlock = 12000000

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
    * Cache which contains bytes of transactions we received but not parsed and processed yet
    */
  private val txProcessingCache = mutable.Map[ModifierId, TransactionProcessingCacheRecord]()

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
      parseAndProcessTransaction(txId, processingCacheRecord.txBytes, processingCacheRecord.source)
      txProcessingCache -= txId
    }
  }

  /**
    * To be called when mempool reporting on finished transaction validation.
    * This method adds validation cost to counter and send another
    */
  private def processMempoolResult(processingResult: InitialTransactionCheckOutcome): Unit = {
    val ReserveCostValue = 5000

    val costOpt = processingResult.transaction.lastCost
    if (costOpt.isEmpty) {
      // should not be here, and so ReserveCostValue should not be used
      log.warn("Cost is empty in processMempoolResult")
    }
    val cost = costOpt.getOrElse(ReserveCostValue)
    val ng = processingResult match {
      case _: FailedTransaction => interblockCost.copy(invalidatedCost = interblockCost.invalidatedCost + cost)
      case _: SuccessfulTransaction => interblockCost.copy(acceptedCost = interblockCost.acceptedCost + cost)
      case _: DeclinedTransaction => interblockCost.copy(declinedCost = interblockCost.declinedCost + cost)
    }

    log.debug(s"Old global cost info: $interblockCost, new $ng, tx processing cache size: ${txProcessingCache.size}")
    interblockCost = ng

    if (interblockCost.totalCost < MempoolCostPerBlock) {
      processFirstTxProcessingCacheRecord()
    }
  }

  /**
    * Register periodic events
    */
  override def preStart(): Unit = {
    // subscribe for history and mempool changes
    viewHolderRef ! GetNodeViewChanges(history = true, state = false, vault = false, mempool = true)

    val toDownloadCheckInterval = networkSettings.syncInterval

    // register as a handler for synchronization-specific types of messages
    val messageSpecs: Seq[MessageSpec[_]] = Seq(InvSpec, RequestModifierSpec, ModifiersSpec, syncInfoSpec)
    networkControllerRef ! RegisterMessageSpecs(messageSpecs, self)

    // register as a listener for peers got connected (handshaked) or disconnected
    context.system.eventStream.subscribe(self, classOf[HandshakedPeer])
    context.system.eventStream.subscribe(self, classOf[DisconnectedPeer])

    // subscribe for all the node view holder events involving modifiers and transactions
    context.system.eventStream.subscribe(self, classOf[ChangedHistory])
    context.system.eventStream.subscribe(self, classOf[ChangedMempool])
    context.system.eventStream.subscribe(self, classOf[ModificationOutcome])
    context.system.eventStream.subscribe(self, classOf[DownloadRequest])
    context.system.eventStream.subscribe(self, classOf[BlockAppliedTransactions])
    context.system.eventStream.subscribe(self, classOf[ModifiersRemovedFromCache])

    context.system.scheduler.scheduleAtFixedRate(toDownloadCheckInterval, toDownloadCheckInterval, self, CheckModifiersToDownload)

    val interval = networkSettings.syncInterval
    context.system.scheduler.scheduleWithFixedDelay(2.seconds, interval, self, SendLocalSyncInfo)

    val healthCheckDelay = settings.nodeSettings.acceptableChainUpdateDelay
    val healthCheckRate = settings.nodeSettings.acceptableChainUpdateDelay / 3
    context.system.scheduler.scheduleAtFixedRate(healthCheckDelay, healthCheckRate, viewHolderRef, IsChainHealthy)(ex, self)
  }

  protected def broadcastModifierInv(m: NodeViewModifier): Unit = {
    val msg = Message(InvSpec, Right(InvData(m.modifierTypeId, Seq(m.id))), None)
    networkControllerRef ! SendToNetwork(msg, Broadcast)
  }

  /**
    * Check whether block section (modifier) with identifier `id` is not stored locally
    * (in history database available via `historyReader` interface, or delivery tracker cache, thus
    * downloading of the modifier is needed.
    */
  private def downloadRequired(historyReader: ErgoHistory)(modifierTypeId: ModifierTypeId, id: ModifierId): Boolean = {
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
                    ext: Seq[(ModifierTypeId, ModifierId)]): Unit = {
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
      syncInfo match {
        case syncV1: ErgoSyncInfoV1 => processSyncV1(hr, syncV1, remote)
        case syncV2: ErgoSyncInfoV2 => processSyncV2(hr, syncV2, remote)
      }
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
    * Variable which is caching height of last header which was extracted from sync info message
    */
  private var lastSyncHeaderApplied: Option[Int] = Option.empty

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
          modifiersToDownload.foreach {
            case (modifierTypeId, modifierId) =>
              if (deliveryTracker.status(modifierId, modifierTypeId, Seq.empty) == ModifiersStatus.Unknown) {
                log.info(s"Downloading block section for header ${continuationHeader.encodedId} : ($modifierId, $modifierTypeId)")
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
    * @param callingPeer that can be used to download headers, it must be Older
    * @return available peers to download headers from together with the state/origin of the peer
    */
  private def getPeersForDownloadingHeaders(callingPeer: ConnectedPeer): Iterable[ConnectedPeer] = {
    syncTracker.peersByStatus
      .get(Older)
      .getOrElse(Array(callingPeer))
  }

  /**
    * @return a peer to download block sections from.
    */
  private def getPeerForDownloadingBlocks(peerToAvoid: Option[ConnectedPeer]): Option[ConnectedPeer] = {

    def filterOutFn(cp: ConnectedPeer) = {
      blockSectionsDownloadFilter.condition(cp) && !peerToAvoid.contains(cp)
    }

    // helper function to take a peer from a group of peers of the same status (e.g. older than us)
    def peerFrom(peers: Seq[ConnectedPeer]): Option[ConnectedPeer] = {
      // first, we are choosing random peer
      // if the peer is not ok (e.g. of some old version having problems)
      // choose first peer which is okay
      // so usually returns randomized peer, with fallback to deterministic one
      val randomPeer = peers(Random.nextInt(peers.size))
      if (filterOutFn(randomPeer)) {
        Some(randomPeer)
      } else {
        peers.find(filterOutFn)
      }
    }

    val peersByStatus = syncTracker.peersByStatus

    val olderOrEqual = peersByStatus.getOrElse(Older, Seq.empty) ++ peersByStatus.getOrElse(Equal, Seq.empty)

    peerFrom(olderOrEqual).orElse {
      log.warn("No peers which are equal or older are found when trying to download a block section")
      val unknownOrFork = peersByStatus.getOrElse(Unknown, Seq.empty) ++ peersByStatus.getOrElse(Fork, Seq.empty)
      peerFrom(unknownOrFork)
    }
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
      }.map(blockSectionsDownloadFilter.filter)
  }

  /**
    * A helper method to ask for block section from given peer
    *
    * @param modifierTypeId - block section type id
    * @param modifierIds - ids of block section to download
    * @param peer - peer to download from
    * @param checksDone - how many times the block section was requested before
    *                    (non-zero if we're re-requesting the block section, in this case, there should be only
    *                     one id to request in `modifierIds`
    */
  def requestBlockSection(modifierTypeId: ModifierTypeId,
                          modifierIds: Seq[ModifierId],
                          peer: ConnectedPeer,
                          checksDone: Int = 0): Unit = {
    if(checksDone > 0 && modifierIds.length > 1) {
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

  /**
    * Our node needs block sections of type `modifierTypeId` with id `modifierId`.
    * Request this modifier from random peer.
    */
  def requestBlockSection(modifierTypeId: ModifierTypeId,
                          modifierId: ModifierId,
                          checksDone: Int,
                          previousPeer: Option[ConnectedPeer]): Unit = {
    getPeerForDownloadingBlocks(previousPeer) match {
      case Some(peerToAsk) =>
        log.debug(s"Going to download $modifierId from $peerToAsk , previous attempts: $checksDone")
        requestBlockSection(modifierTypeId, Seq(modifierId), peerToAsk, checksDone)
      case None =>
        log.error("No peer found to download a block section from. " +
                  "DeliveryTracker: " + deliveryTracker + " SyncTracker: " + syncTracker)
    }
  }

  def onDownloadRequest(historyReader: ErgoHistory): Receive = {
    case DownloadRequest(modifierTypeId: ModifierTypeId, modifierId: ModifierId) =>
      if (deliveryTracker.status(modifierId, modifierTypeId, Seq(historyReader)) == ModifiersStatus.Unknown) {
        requestBlockSection(modifierTypeId, modifierId, checksDone = 0, None)
      }
  }

  /**
    * Modifier download method that is given min/max constraints for modifiers to download from peers.
    * It sends requests for modifiers to given peers in optimally sized batches.
    *
    * @param maxModifiers maximum modifiers to download
    * @param minModifiersPerBucket minimum modifiers to download per bucket
    * @param maxModifiersPerBucket maximum modifiers to download per bucket
    * @param getPeersOpt optionally get peers to download from, all peers have the same PeerSyncState
    * @param fetchMax function that fetches modifiers, it is passed how many of them tops
    */
  protected def requestDownload(maxModifiers: Int, minModifiersPerBucket: Int, maxModifiersPerBucket: Int)
                               (getPeersOpt: => Option[Iterable[ConnectedPeer]])
                               (fetchMax: Int => Map[ModifierTypeId, Seq[ModifierId]]): Unit =
    getPeersOpt
      .foreach { peers =>
        val modifiersByBucket = ElementPartitioner.distribute(peers, maxModifiers, minModifiersPerBucket, maxModifiersPerBucket)(fetchMax)
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

    if (typeId == Transaction.ModifierTypeId) {
      // filter out transactions already in the mempool
      val notInThePool = requestedModifiers.filterKeys(id => !mp.contains(id))
      val (toProcess, toPutIntoCache) = if (interblockCost.totalCost < MempoolCostPerBlock) {
        // if we are within per-block limits, parse and process first transaction
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
    } else {
      Constants.modifierSerializers.get(typeId) match {
        case Some(serializer: ScorexSerializer[BlockSection]@unchecked) =>
          // parse all modifiers and put them to modifiers cache
          val parsed: Iterable[BlockSection] = parseModifiers(requestedModifiers, typeId, serializer, remote)

          // `deliveryTracker.setReceived()` called inside `validateAndSetStatus` for every correct modifier
          val valid = parsed.filter(validateAndSetStatus(hr, remote, _))
          if (valid.nonEmpty) {
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
  }

  /**
    * Parse transaction coming from remote, filtering out immediately too big one, and send parsed transaction
    * to mempool for processing
    */
  def parseAndProcessTransaction(id: ModifierId, bytes: Array[Byte], remote: ConnectedPeer): Unit = {
    if (bytes.length > settings.nodeSettings.maxTransactionSize) {
      deliveryTracker.setInvalid(id, Transaction.ModifierTypeId)
      penalizeMisbehavingPeer(remote)
      log.warn(s"Transaction size ${bytes.length} from ${remote.toString} " +
                s"exceeds limit ${settings.nodeSettings.maxTransactionSize}")
    } else {
      ErgoTransactionSerializer.parseBytesTry(bytes) match {
        case Success(tx) if id == tx.id =>
          val utx = UnconfirmedTransaction(tx, bytes, Some(remote))
          viewHolderRef ! TransactionFromRemote(utx)
        case _ =>
          // Penalize peer and do nothing - it will be switched to correct state on CheckDelivery
          penalizeMisbehavingPeer(remote)
          log.warn(s"Failed to parse transaction with declared id ${encoder.encodeId(id)} from ${remote.toString}")
      }
    }
  }

  /**
    * Parse block sections with serializer provided, check that its id is equal to the declared one,
    * penalize misbehaving peer for every incorrect modifier
    *
    * @return collection of parsed modifiers
    */
  def parseModifiers[M <: NodeViewModifier](modifiers: Map[ModifierId, Array[Byte]],
                                            typeId: ModifierTypeId,
                                            serializer: ScorexSerializer[M],
                                            remote: ConnectedPeer): Iterable[M] = {
    modifiers.flatMap { case (id, bytes) =>
      serializer.parseBytesTry(bytes) match {
        case Success(mod) if id == mod.id =>
          Some(mod)
        case _ =>
          // Penalize peer and do nothing - it will be switched to correct state on CheckDelivery
          penalizeMisbehavingPeer(remote)
          log.warn(s"Failed to parse modifier with declared id ${encoder.encodeId(id)} from ${remote.toString}")
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
                  typeId: ModifierTypeId,
                  modifiers: Map[ModifierId, Array[Byte]],
                  blockAppliedTxsCache: FixedSizeApproximateCacheQueue): Map[ModifierId, Array[Byte]] = {
    val modifiersByStatus =
      modifiers
        .groupBy { case (id, _) => deliveryTracker.status(id, typeId, Seq.empty) }
        .view.force

    val spam = modifiersByStatus.filterKeys(_ != Requested)

    if (spam.nonEmpty) {
      if (typeId == Transaction.ModifierTypeId) {
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

    // We download transactions only if following conditions met:
    def txAcceptanceFilter: Boolean = {
      settings.nodeSettings.stateType.holdsUtxoSet && // node holds UTXO set
        hr.headersHeight >= syncTracker.maxHeight().getOrElse(0) && // our best header is not worse than best around
        hr.fullBlockHeight == hr.headersHeight && // we have all the full blocks
        interblockCost.totalCost <= MempoolCostPerBlock * 3 / 2 && // we can download some extra to fill cache
        txProcessingCache.size <= MaxProcessingTransactionsCacheSize && // txs processing cache is not overfull
        declined.size < MaxDeclined // the node is not stormed by transactions is has to decline
    }

    val modifierTypeId = invData.typeId

    val newModifierIds = modifierTypeId match {
      case Transaction.ModifierTypeId =>

        if (txAcceptanceFilter) {
          val unknownMods =
            invData.ids.filter(mid => deliveryTracker.status(mid, modifierTypeId, Seq(mp)) == ModifiersStatus.Unknown)
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
      // our requested list is half empty - request more missed modifiers
      self ! CheckModifiersToDownload
    } else {
      // headers chain is not synced yet, but our requested list is half empty - ask for more headers
      sendSync(historyReader)
    }
  }

  //other node asking for objects by their ids
  protected def modifiersReq(hr: ErgoHistory, mp: ErgoMemPool, invData: InvData, remote: ConnectedPeer): Unit = {
      val objs: Seq[(ModifierId, Array[Byte])] = invData.typeId match {
        case typeId: ModifierTypeId if typeId == Transaction.ModifierTypeId =>
          mp.getAll(invData.ids).map { unconfirmedTx =>
            unconfirmedTx.transaction.id -> unconfirmedTx.transactionBytes.getOrElse(unconfirmedTx.transaction.bytes)
          }
        case _: ModifierTypeId =>
          invData.ids.flatMap(id => hr.modifierBytesById(id).map(bytes => (id, bytes)))
      }

      log.debug(s"Requested ${invData.ids.length} modifiers ${idsToString(invData)}, " +
                s"sending ${objs.length} modifiers ${idsToString(invData.typeId, objs.map(_._1))} ")

    @tailrec
    def sendByParts(mods: Seq[(ModifierId, Array[Byte])]): Unit = {
      var size = 5 //message type id + message size
      var batch = mods.takeWhile { case (_, modBytes) =>
        size += NodeViewModifier.ModifierIdSize + 4 + modBytes.length
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
  protected def checkDelivery: Receive = {
    case CheckDelivery(peer, modifierTypeId, modifierId) =>
      if (deliveryTracker.status(modifierId, modifierTypeId, Seq.empty) == ModifiersStatus.Requested) {
        // If transaction not delivered on time, we just forget about it.
        // It could be removed from other peer's mempool, so no reason to penalize the peer.
        if (modifierTypeId == Transaction.ModifierTypeId) {
          deliveryTracker.clearStatusForModifier(modifierId, modifierTypeId, ModifiersStatus.Requested)
        } else {
          // A block section is not delivered on time.
          log.info(s"Peer ${peer.toString} has not delivered modifier " +
                   s"$modifierTypeId : ${encoder.encodeId(modifierId)} on time, status tracker: $syncTracker")

          // Number of block section delivery checks increased or initialized,
          // except the case where we can have issues with connectivity,
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
            log.info(s"Rescheduling request for $modifierId")
            deliveryTracker.setUnknown(modifierId, modifierTypeId)
            requestBlockSection(modifierTypeId, modifierId, checksDone, Some(peer))
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

  protected def broadcastInvForNewModifier(mod: PersistentNodeViewModifier): Unit = {
    mod match {
      case fb: ErgoFullBlock if fb.header.isNew(timeProvider, 1.hour) =>
        fb.toSeq.foreach(s => broadcastModifierInv(s))
      case _ =>
    }
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
    val clearTimeout = FiniteDuration(10, MINUTES)
    val now = System.currentTimeMillis()

    val toRemove = declined.filter { case (_, time) =>
      (now - time) > clearTimeout.toMillis
    }
    log.debug(s"Declined transactions to be cleared: ${toRemove.size}")
    toRemove.foreach { case (id, _) =>
      declined.remove(id)
    }
  }

  protected def viewHolderEvents(historyReader: ErgoHistory, mempoolReader: ErgoMemPool, blockAppliedTxsCache: FixedSizeApproximateCacheQueue): Receive = {
    // Requests BlockSections with `Unknown` status that are defined by block headers but not downloaded yet.
    // Trying to keep size of requested queue equals to `desiredSizeOfExpectingQueue`.

    case CheckModifiersToDownload =>
      val maxModifiersToDownload = deliveryTracker.modifiersToDownload
      requestDownload(
        maxModifiersToDownload,
        minModifiersPerBucket,
        maxModifiersPerBucket
      )(getPeersForDownloadingBlocks) { howManyPerType =>
        historyReader.nextModifiersToDownload(howManyPerType, downloadRequired(historyReader))
      }

    // If new enough semantically valid ErgoFullBlock was applied, send inv for block header and all its sections
    case SemanticallySuccessfulModifier(mod) =>
      broadcastInvForNewModifier(mod)
      if (mod.isInstanceOf[ErgoFullBlock]) {
        clearDeclined()
        clearInterblockCost()
        processFirstTxProcessingCacheRecord() // resume cache processing
      }

    case st@SuccessfulTransaction(utx) =>
      val tx = utx.transaction
      deliveryTracker.setHeld(tx.id, Transaction.ModifierTypeId)
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
          case TooHighCostError(_) =>
            log.info(s"Penalize spamming peer $peer for too costly transaction $id")
            penalizeSpammingPeer(peer)
          case _ =>
            log.info(s"Penalize peer $peer for too costly transaction $id (reason: $error)")
            penalizeMisbehavingPeer(peer)
        }
      }

    case FailedOnRecheckTransaction(_, _) =>
      // do nothing for now

    case SyntacticallySuccessfulModifier(mod) =>
      deliveryTracker.setHeld(mod.id, mod.modifierTypeId)

    case RecoverableFailedModification(mod, e) =>
      logger.debug(s"Setting recoverable failed modifier ${mod.id} as Unknown", e)
      deliveryTracker.setUnknown(mod.id, mod.modifierTypeId)

    case SyntacticallyFailedModification(mod, e) =>
      logger.debug(s"Invalidating syntactically failed modifier ${mod.id}", e)
      deliveryTracker.setInvalid(mod.id, mod.modifierTypeId).foreach(penalizeMisbehavingPeer)

    case SemanticallyFailedModification(mod, e) =>
      logger.debug(s"Invalidating semantically failed modifier ${mod.id}", e)
      deliveryTracker.setInvalid(mod.id, mod.modifierTypeId).foreach(penalizeMisbehavingPeer)

    case ChangedHistory(newHistoryReader: ErgoHistory) =>
      context.become(initialized(newHistoryReader, mempoolReader, blockAppliedTxsCache))

    case ChangedMempool(newMempoolReader: ErgoMemPool) =>
      context.become(initialized(historyReader, newMempoolReader, blockAppliedTxsCache))

    case ModifiersRemovedFromCache(cleared: Seq[BlockSection]) =>
      // stop processing for cleared modifiers
      // applied modifiers state was already changed at `SyntacticallySuccessfulModifier`
      cleared.foreach(m => deliveryTracker.setUnknown(m.id, m.modifierTypeId))
      requestMoreModifiers(historyReader)

    case BlockAppliedTransactions(transactionIds: Seq[ModifierId]) =>
      // We collect applied TXs to history in order to avoid banning peers that sent these afterwards
      logger.debug("Caching applied transactions")
      context.become(initialized(historyReader, mempoolReader, blockAppliedTxsCache.putAll(transactionIds)))

    case ChainIsHealthy =>
      // good news
      logger.debug("Chain is good")

    case ChainIsStuck(error) =>
      log.warn(s"Chain is stuck! $error\nDelivery tracker State:\n$deliveryTracker\nSync tracker state:\n$syncTracker")
      deliveryTracker.reset()
  }

  /** get handlers of messages coming from peers */
  private def msgHandlers(hr: ErgoHistory,
                          mp: ErgoMemPool,
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
  }

  def initialized(hr: ErgoHistory, mp: ErgoMemPool, blockAppliedTxsCache: FixedSizeApproximateCacheQueue): PartialFunction[Any, Unit] = {
    processDataFromPeer(msgHandlers(hr, mp, blockAppliedTxsCache)) orElse
      onDownloadRequest(hr) orElse
      sendLocalSyncInfo(hr) orElse
      viewHolderEvents(hr, mp, blockAppliedTxsCache) orElse
      peerManagerEvents orElse
      checkDelivery orElse {
      case a: Any => log.error("Strange input: " + a)
    }
  }

  /** Wait until both historyReader and mempoolReader instances are received so actor can be operational */
  def initializing(hr: Option[ErgoHistory], mp: Option[ErgoMemPool], blockAppliedTxsCache: FixedSizeApproximateCacheQueue): PartialFunction[Any, Unit] = {
    case ChangedHistory(historyReader: ErgoHistory) =>
      mp match {
        case Some(mempoolReader) =>
          context.become(initialized(historyReader, mempoolReader, blockAppliedTxsCache))
        case _ =>
          context.become(initializing(Option(historyReader), mp, blockAppliedTxsCache))
      }
    case ChangedMempool(mempoolReader: ErgoMemPool) =>
      hr match {
        case Some(historyReader) =>
          context.become(initialized(historyReader, mempoolReader, blockAppliedTxsCache))
        case _ =>
          context.become(initializing(hr, Option(mempoolReader), blockAppliedTxsCache))
      }
    case msg =>
      // Actor not initialized yet, scheduling message until it is
      context.system.scheduler.scheduleOnce(1.second, self, msg)
  }

  override def receive: Receive = initializing(None, None, FixedSizeApproximateCacheQueue.empty(cacheQueueSize = 5))

}

object ErgoNodeViewSynchronizer {

  def props(networkControllerRef: ActorRef,
            viewHolderRef: ActorRef,
            syncInfoSpec: ErgoSyncInfoMessageSpec.type,
            settings: ErgoSettings,
            timeProvider: NetworkTimeProvider,
            syncTracker: ErgoSyncTracker,
            deliveryTracker: DeliveryTracker)
           (implicit ex: ExecutionContext): Props =
    Props(new ErgoNodeViewSynchronizer(networkControllerRef, viewHolderRef, syncInfoSpec, settings,
      timeProvider, syncTracker, deliveryTracker))

  def apply(networkControllerRef: ActorRef,
            viewHolderRef: ActorRef,
            syncInfoSpec: ErgoSyncInfoMessageSpec.type,
            settings: ErgoSettings,
            timeProvider: NetworkTimeProvider,
            syncTracker: ErgoSyncTracker,
            deliveryTracker: DeliveryTracker)
           (implicit context: ActorRefFactory, ex: ExecutionContext): ActorRef =
    context.actorOf(props(networkControllerRef, viewHolderRef, syncInfoSpec, settings, timeProvider, syncTracker, deliveryTracker))

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

  object ReceivableMessages {

    // getLocalSyncInfo messages
    case object SendLocalSyncInfo

    case class ResponseFromLocal(source: ConnectedPeer, modifierTypeId: ModifierTypeId, localObjects: Seq[(ModifierId, Array[Byte])])

    /**
      * Check delivery of modifier with type `modifierTypeId` and id `modifierId`.
      * `source` may be defined if we expect modifier from concrete peer or None if
      * we just need some modifier, but don't know who have it
      *
      */
    case class CheckDelivery(source: ConnectedPeer,
                             modifierTypeId: ModifierTypeId,
                             modifierId: ModifierId)

    trait PeerManagerEvent

    case class HandshakedPeer(remote: ConnectedPeer) extends PeerManagerEvent

    case class DisconnectedPeer(peer: ConnectedPeer) extends PeerManagerEvent

    trait NodeViewHolderEvent

    trait NodeViewChange extends NodeViewHolderEvent

    case class ChangedHistory(reader: ErgoHistoryReader) extends NodeViewChange

    case class ChangedMempool(mempool: ErgoMemPoolReader) extends NodeViewChange

    case class ChangedVault(reader: ErgoWalletReader) extends NodeViewChange

    case class ChangedState(reader: ErgoStateReader) extends NodeViewChange

    //todo: consider sending info on the rollback

    case object RollbackFailed extends NodeViewHolderEvent

    case class StartingPersistentModifierApplication(modifier: BlockSection) extends NodeViewHolderEvent

    /**
      * After application of batch of modifiers from cache to History, NodeViewHolder sends this message,
      * which contains modifiers cleared from cache
      */
    case class ModifiersRemovedFromCache(cleared: Seq[BlockSection])

    // hierarchy of events regarding modifiers application outcome
    trait ModificationOutcome extends NodeViewHolderEvent

    trait InitialTransactionCheckOutcome extends ModificationOutcome {
      val transaction: UnconfirmedTransaction
    }

    case class FailedTransaction(transaction: UnconfirmedTransaction, error: Throwable) extends InitialTransactionCheckOutcome

    case class SuccessfulTransaction(transaction: UnconfirmedTransaction) extends InitialTransactionCheckOutcome

    /**
      * Transaction declined by the mempool (not permanently invalidated, so pool can accept it in future)
      */
    case class DeclinedTransaction(transaction: UnconfirmedTransaction) extends InitialTransactionCheckOutcome

    /**
      * Transaction which was failed not immediately but after sitting for some time in the mempool or during block
      * candidate generation
      */
    case class FailedOnRecheckTransaction(id : ModifierId, error: Throwable) extends ModificationOutcome

    case class RecoverableFailedModification(modifier: BlockSection, error: Throwable) extends ModificationOutcome

    case class SyntacticallyFailedModification(modifier: BlockSection, error: Throwable) extends ModificationOutcome

    case class SemanticallyFailedModification(modifier: BlockSection, error: Throwable) extends ModificationOutcome

    case class SyntacticallySuccessfulModifier(modifier: BlockSection) extends ModificationOutcome

    case class SemanticallySuccessfulModifier(modifier: BlockSection) extends ModificationOutcome

  }

}
