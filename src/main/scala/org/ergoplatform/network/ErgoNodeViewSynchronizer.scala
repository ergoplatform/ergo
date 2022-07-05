package org.ergoplatform.network

import akka.actor.SupervisorStrategy.{Restart, Stop}
import java.net.InetSocketAddress

import akka.actor.{Actor, ActorInitializationException, ActorKilledException, ActorRef, ActorRefFactory, DeathPactException, OneForOneStrategy, Props}
import org.ergoplatform.modifiers.history.ADProofs
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.{ErgoSyncInfoV1, ErgoSyncInfoV2}
import org.ergoplatform.nodeView.history._
import org.ergoplatform.network.ErgoNodeViewSynchronizer.{CheckModifiersToDownload, PeerSyncState}
import org.ergoplatform.nodeView.ErgoNodeViewHolder.BlockAppliedTransactions
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfo, ErgoSyncInfoMessageSpec}
import org.ergoplatform.nodeView.mempool.{ErgoMemPool, ErgoMemPoolReader}
import org.ergoplatform.settings.{Constants, ErgoSettings}
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.{ChainIsHealthy, ChainIsStuck, GetNodeViewChanges, IsChainHealthy, ModifiersFromRemote, TransactionsFromRemote}
import org.ergoplatform.nodeView.ErgoNodeViewHolder._
import scorex.core.consensus.{Equal, Fork, Nonsense, Older, Unknown, Younger}
import scorex.core.network.ModifiersStatus.Requested
import scorex.core.{ModifierTypeId, NodeViewModifier, PersistentNodeViewModifier, idsToString}
import scorex.core.network.NetworkController.ReceivableMessages.{PenalizePeer, RegisterMessageSpecs}
import org.ergoplatform.network.ErgoNodeViewSynchronizer.ReceivableMessages._
import org.ergoplatform.nodeView.state.{ErgoStateReader, StateType}
import org.ergoplatform.nodeView.wallet.ErgoWalletReader
import scorex.core.network.message.{InvSpec, MessageSpec, ModifiersSpec, RequestModifierSpec}
import scorex.core.network._
import scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork
import scorex.core.network.message.{InvData, Message, ModifiersData}
import scorex.core.network.{ConnectedPeer, ModifiersStatus, SendToPeer, SendToPeers}
import scorex.core.serialization.ScorexSerializer
import scorex.core.settings.NetworkSettings
import scorex.core.transaction.Transaction
import scorex.core.utils.{NetworkTimeProvider, ScorexEncoding}
import scorex.core.validation.MalformedModifierError
import scorex.util.{ModifierId, ScorexLogging}
import scorex.core.network.DeliveryTracker
import scorex.core.network.peer.PenaltyType
import scorex.core.transaction.state.TransactionValidation.TooHighCostError


import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.{Failure, Success}

/**
  * Tweaks on top of Scorex' NodeViewSynchronizer made to optimize Ergo network
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

  private var syncInfoV1CacheByHeadersHeight: Option[(Int, ErgoSyncInfoV1)] = Option.empty

  private var syncInfoV2CacheByHeadersHeight: Option[(Int, ErgoSyncInfoV2)] = Option.empty

  private val networkSettings: NetworkSettings = settings.scorexSettings.network

  protected val deliveryTimeout: FiniteDuration = networkSettings.deliveryTimeout

  protected val invSpec = new InvSpec(networkSettings.maxInvObjects)
  protected val requestModifierSpec = new RequestModifierSpec(networkSettings.maxInvObjects)
  protected val modifiersSpec = new ModifiersSpec(networkSettings.maxPacketSize)

  private val minModifiersPerBucket = 8 // minimum of persistent modifiers (excl. headers) to download by single peer
  private val maxModifiersPerBucket = 12 // maximum of persistent modifiers (excl. headers) to download by single peer

  private val minHeadersPerBucket = 50 // minimum of headers to download by single peer
  private val maxHeadersPerBucket = 400 // maximum of headers to download by single peer

  // It could be the case that adversarial peers are sending sync messages to the node to cause
  // resource exhaustion. To prevent it, we do not answer on sync message, if previous one was sent
  // no more than `GlobalSyncLockTime` milliseconds ago. There's also per-peer limit `PerPeerSyncLockTime`
  private val GlobalSyncLockTime = 50
  private val PerPeerSyncLockTime = 100

  /**
    * Register periodic events
    */
  override def preStart(): Unit = {
    // subscribe for history and mempool changes
    viewHolderRef ! GetNodeViewChanges(history = true, state = false, vault = false, mempool = true)

    val toDownloadCheckInterval = networkSettings.syncInterval

    // register as a handler for synchronization-specific types of messages
    val messageSpecs: Seq[MessageSpec[_]] = Seq(invSpec, requestModifierSpec, modifiersSpec, syncInfoSpec)
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
    val msg = Message(invSpec, Right(InvData(m.modifierTypeId, Seq(m.id))), None)
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
                    ext: Seq[(ModifierTypeId, ModifierId)]): Unit =
    ext.groupBy(_._1).mapValues(_.map(_._2)).foreach {
      case (mid, mods) =>
        networkControllerRef ! SendToNetwork(Message(invSpec, Right(InvData(mid, mods)), None), SendToPeer(remote))
    }

  var globalSyncGot = 0L
  /**
    * Process sync message `syncInfo` got from neighbour peer `remote`
    */
  protected def processSync(hr: ErgoHistory, syncInfo: ErgoSyncInfo, remote: ConnectedPeer): Unit = {
    val newGlobal = timeProvider.time()
    val globalDiff = newGlobal - globalSyncGot

    if(globalDiff > GlobalSyncLockTime) {
      globalSyncGot = newGlobal

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
    } else {
      log.debug("Global sync violation")
    }
  }

  /**
    * Processing sync V1 message `syncInfo` got from neighbour peer `remote`
    */
  protected def processSyncV1(hr: ErgoHistory, syncInfo: ErgoSyncInfoV1, remote: ConnectedPeer): Unit = {
    val comparison = hr.compare(syncInfo)
    log.debug(s"Comparison with $remote having starting points ${syncInfo.lastHeaderIds}. " +
      s"Comparison result is $comparison.")

    val oldStatus = syncTracker.getStatus(remote).getOrElse(Unknown)
    val status = comparison
    syncTracker.updateStatus(remote, status, height = None)

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

    if ((oldStatus != status) || syncTracker.notSyncedOrOutdated(remote) || status == Older || status == Fork) {
      val ownSyncInfo = getV1SyncInfo(hr)
      sendSyncToPeer(remote, ownSyncInfo)
    }
  }

  /**
    * Processing sync V2 message `syncInfo` got from neighbour peer `remote` (supporting sync v2)
    */
  protected def processSyncV2(hr: ErgoHistory, syncInfo: ErgoSyncInfoV2, remote: ConnectedPeer): Unit = {
    val oldStatus = syncTracker.getStatus(remote).getOrElse(Unknown)
    val status = hr.compare(syncInfo)
    syncTracker.updateStatus(remote, status, syncInfo.height)

    log.debug(s"Comparison with $remote having starting points ${syncInfo.lastHeaders}. " +
      s"Comparison result is $status.")

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

    if ((oldStatus != status) || syncTracker.notSyncedOrOutdated(remote) || status == Older || status == Fork) {
      val ownSyncInfo = getV2SyncInfo(hr, full = true)
      sendSyncToPeer(remote, ownSyncInfo)
    }
  }

  /**
    * Calculates new continuation header from syncInfo message if any, validates it and sends it
    * to nodeViewHolder as a remote modifier for it to be applied
    * @param syncInfo other's node sync info
    */
  private def applyValidContinuationHeaderV2(syncInfo: ErgoSyncInfoV2, history: ErgoHistory, peer: ConnectedPeer): Unit =
    history.continuationHeaderV2(syncInfo).foreach { continuationHeader =>
      history.applicableTry(continuationHeader) match {
        case Failure(e) if e.isInstanceOf[MalformedModifierError] =>
          log.warn(s"Header from syncInfoV2 ${continuationHeader.encodedId} is invalid", e)
        case _ =>
          log.info(s"Applying valid syncInfoV2 header ${continuationHeader.encodedId}")
          viewHolderRef ! ModifiersFromRemote(Seq(continuationHeader))
          val modifiersToDownload = history.requiredModifiersForHeader(continuationHeader)
          modifiersToDownload.foreach {
            case (modifierTypeId, modifierId) =>
              if (deliveryTracker.status(modifierId, modifierTypeId, Seq.empty) == ModifiersStatus.Unknown) {
                log.info(s"Downloading block section for header ${continuationHeader.encodedId} : ($modifierId, $modifierTypeId)")
                downloadModifiers(Seq(modifierId), modifierTypeId, peer)
              }
          }
      }
    }

  /**
    * Headers should be downloaded from an Older node, it is triggered by received sync message from an older node
    * @param callingPeer that can be used to download headers, it must be Older
    * @return available peers to download headers from together with the state/origin of the peer
    */
  private def getPeersForDownloadingHeaders(callingPeer: ConnectedPeer): (PeerSyncState, Iterable[ConnectedPeer]) = {
    syncTracker.peersByStatus
      .get(Older)
      .map(PeerSyncState.Older -> _)
      .getOrElse(PeerSyncState.OlderCalling -> Array(callingPeer))
  }

  /**
    * Other persistent modifiers besides headers should be downloaded from either Older or Equal node, with fallback to Unknown or Fork
    * @return available peers to download persistent modifiers from together with the state/origin of the peer
    */
  private def getPeersForDownloadingBlocks: Option[(PeerSyncState, Iterable[ConnectedPeer])] = {
    val peersByStatus = syncTracker.peersByStatus
    Option(peersByStatus.getOrElse(Older, mutable.WrappedArray.empty) ++ peersByStatus.getOrElse(Equal, mutable.WrappedArray.empty))
      .filter(_.nonEmpty)
      .map(PeerSyncState.OlderOrEqual -> _)
      .orElse {
        Option(peersByStatus.getOrElse(Unknown, mutable.WrappedArray.empty) ++ peersByStatus.getOrElse(Fork, mutable.WrappedArray.empty))
          .filter(_.nonEmpty)
          .map(PeerSyncState.UnknownOrFork -> _)
      }.map { case (syncState, peers) =>
        val peersFiltered =
          if (settings.nodeSettings.stateType == StateType.Digest) {
            DigestModeFilter.filter(peers)
          } else {
            BrokenModifiersFilter.filter(peers)
          }
        syncState -> peersFiltered
      }
  }

  /**
    * Set modifiers of particular type as Requested and download them from given peer and periodically check for delivery
    */
  private def downloadModifiers(modifierIds: Seq[ModifierId], modifierTypeId: ModifierTypeId, peer: ConnectedPeer): Unit = {
    deliveryTracker.setRequested(modifierIds, modifierTypeId, Some(peer)) { deliveryCheck =>
      context.system.scheduler.scheduleOnce(deliveryTimeout, self, deliveryCheck)
    }
    val msg = Message(requestModifierSpec, Right(InvData(modifierTypeId, modifierIds)), None)
    networkControllerRef ! SendToNetwork(msg, SendToPeer(peer))
  }

  /**
    * Modifier download method that is given min/max constraints for modifiers to download from peers.
    * It sends requests for modifiers to given peers in optimally sized batches.
    * @param maxModifiers maximum modifiers to download
    * @param minModifiersPerBucket minimum modifiers to download per bucket
    * @param maxModifiersPerBucket maximum modifiers to download per bucket
    * @param getPeersOpt optionally get peers to download from, all peers have the same PeerSyncState
    * @param fetchMax function that fetches modifiers, it is passed how many of them tops
    */
  protected def requestDownload(maxModifiers: Int, minModifiersPerBucket: Int, maxModifiersPerBucket: Int)
                               (getPeersOpt: => Option[(PeerSyncState, Iterable[ConnectedPeer])])
                               (fetchMax: Int => Map[ModifierTypeId, Seq[ModifierId]]): Unit =
    getPeersOpt
      .foreach { case (peerStatus, peers) =>
        val modifiersByBucket = ElementPartitioner.distribute(peers, maxModifiers, minModifiersPerBucket, maxModifiersPerBucket)(fetchMax)
        // collect and log useful downloading progress information, don't worry it does not run frequently
        modifiersByBucket.headOption.foreach { _ =>
          modifiersByBucket
            .groupBy(_._1._2)
            .mapValues(_.map(_._2.size))
            .map { case (modType, batchSizes) =>
              s"Downloading from $peerStatus peers : type[$modType] of ${batchSizes.size} batches each of ~ size: ${batchSizes.take(2).max}"
            }.foreach(log.info(_))
        }
        // bucket represents a peer and a modifierType as we cannot send mixed types to a peer
        modifiersByBucket.foreach { case ((peer, modifierTypeId), modifierIds) =>
          downloadModifiers(modifierIds, modifierTypeId, peer)
        }
      }

  /**
    * Logic to process block parts got from another peer.
    * Filter out non-requested block parts (with a penalty to spamming peer),
    * parse block parts and send valid modifiers to NodeViewHolder
    */
  protected def modifiersFromRemote(
                                     hr: ErgoHistory,
                                     data: ModifiersData,
                                     remote: ConnectedPeer,
                                     blockAppliedTxsCache: FixedSizeApproximateCacheQueue): Unit = {
    val typeId = data.typeId
    val modifiers = data.modifiers
    log.info(s"Got ${modifiers.size} modifiers of type $typeId from remote connected peer: ${remote.connectionId}")
    log.debug("Modifier ids: " + modifiers.keys)

    // filter out non-requested modifiers
    val requestedModifiers = processSpam(remote, typeId, modifiers, blockAppliedTxsCache)

    Constants.modifierSerializers.get(typeId) match {
      case Some(serializer: ScorexSerializer[ErgoTransaction]@unchecked) if typeId == Transaction.ModifierTypeId =>
        // parse all transactions and send them to node view holder
        val parsed: Iterable[ErgoTransaction] = parseModifiers(requestedModifiers, typeId, serializer, remote)
        viewHolderRef ! TransactionsFromRemote(parsed)

      case Some(serializer: ScorexSerializer[ErgoPersistentModifier]@unchecked) =>
        // parse all modifiers and put them to modifiers cache
        val parsed: Iterable[ErgoPersistentModifier] = parseModifiers(requestedModifiers, typeId, serializer, remote)
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

  /**
    *
    * Parse modifiers using specified serializer, check that its id is equal to the declared one,
    * penalize misbehaving peer for every incorrect modifier,
    * call deliveryTracker.onReceive() for every correct modifier to update its status
    *
    * @return collection of parsed modifiers
    */
  def parseModifiers[M <: NodeViewModifier](modifiers: Map[ModifierId, Array[Byte]],
                                            typeId: ModifierTypeId,
                                            serializer: ScorexSerializer[M],
                                            remote: ConnectedPeer): Iterable[M] = {
    modifiers.flatMap { case (id, bytes) =>
      if (typeId == Transaction.ModifierTypeId && bytes.length > settings.nodeSettings.maxTransactionSize) {
        deliveryTracker.setInvalid(id, typeId)
        penalizeMisbehavingPeer(remote)
        log.warn(s"Transaction size ${bytes.length} from ${remote.toString} exceeds limit ${settings.nodeSettings.maxTransactionSize}")
        None
      } else {
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
    val modifierTypeId = invData.typeId
    val newModifierIds = modifierTypeId match {
      case Transaction.ModifierTypeId =>
        // We download transactions only if the node is not needed for externally provided proofs
        // (so having UTXO set, and the chain is synced
        if (!settings.nodeSettings.stateType.requireProofs &&
          hr.isHeadersChainSynced &&
          hr.fullBlockHeight == hr.headersHeight) {
          val unknownMods =
            invData.ids.filter(mid => deliveryTracker.status(mid, modifierTypeId, Seq(mp)) == ModifiersStatus.Unknown)
          // filter out transactions that were already applied to history
          val notApplied = unknownMods.filterNot(blockAppliedTxsCache.mightContain)
          log.info(s"Processing ${invData.ids.length} tx invs from $peer, " +
            s"${unknownMods.size} of them are unknown, requesting $notApplied")
          notApplied
        } else {
          Seq.empty
        }
      case _ =>
        log.info(s"Processing ${invData.ids.length} non-tx invs (of type $modifierTypeId) from $peer")
        invData.ids.filter(mid => deliveryTracker.status(mid, modifierTypeId, Seq(hr)) == ModifiersStatus.Unknown)
    }

    if (newModifierIds.nonEmpty) {
      log.debug(s"Going to request ${newModifierIds.length} modifiers of type $modifierTypeId from $peer")
      val msg = Message(requestModifierSpec, Right(InvData(modifierTypeId, newModifierIds)), None)
      peer.handlerRef ! msg
      deliveryTracker.setRequested(newModifierIds, modifierTypeId, Some(peer)) { deliveryCheck =>
        context.system.scheduler.scheduleOnce(deliveryTimeout, self, deliveryCheck)
      }
    }
  }

  /**
    * If our requested list is more than half empty, enforce to request more:
    * - headers, if our headers chain is not synced yet (by sending sync message)
    * - block sections, if our headers chain is synced
    */
  protected def requestMoreModifiers(historyReader: ErgoHistory): Unit = {
    if (historyReader.isHeadersChainSynced) {
      // our requested list is is half empty - request more missed modifiers
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
          mp.getAll(invData.ids).map(tx => tx.id -> tx.bytes)
        case _: ModifierTypeId =>
          invData.ids.flatMap(id => hr.modifierBytesById(id).map(bytes => (id, bytes)))
      }

      log.whenDebugEnabled {
        log.debug(s"Requested ${invData.ids.length} modifiers ${idsToString(invData)}, " +
          s"sending ${objs.length} modifiers ${idsToString(invData.typeId, objs.map(_._1))} ")
      }

    @tailrec
    def sendByParts(mods: Seq[(ModifierId, Array[Byte])]): Unit = {
      var size = 5 //message type id + message size
      var batch = mods.takeWhile { case (_, modBytes) =>
        size += NodeViewModifier.ModifierIdSize + 4 + modBytes.length
        size < networkSettings.maxPacketSize
      }
      if (batch.isEmpty) {
        // send modifier anyway
        val ho = mods.headOption
        batch = ho.toSeq
        log.warn(s"Sending too big modifier ${ho.map(_._1)}, its size ${ho.map(_._2.length)}")
      }
      remote.handlerRef ! Message(modifiersSpec, Right(ModifiersData(invData.typeId, batch.toMap)), None)
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
  def validateAndSetStatus(hr: ErgoHistory, remote: ConnectedPeer, pmod: ErgoPersistentModifier): Boolean = {
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
    case CheckDelivery(peerOpt, modifierTypeId, modifierId) =>
      if (deliveryTracker.status(modifierId, modifierTypeId, Seq.empty) == ModifiersStatus.Requested) {
        // If transaction not delivered on time, we just forget about it.
        // It could be removed from other peer's mempool, so no reason to penalize the peer.
        if (modifierTypeId == Transaction.ModifierTypeId) {
          deliveryTracker.clearStatusForModifier(modifierId, modifierTypeId, ModifiersStatus.Requested)
        } else {
          // A persistent modifier is not delivered on time.
          peerOpt match {
            case Some(peer) =>
              log.info(s"Peer ${peer.toString} has not delivered asked modifier $modifierTypeId : ${encoder.encodeId(modifierId)} on time")
              penalizeNonDeliveringPeer(peer)
              deliveryTracker.onStillWaiting(peer, modifierTypeId, modifierId) { deliveryCheck =>
                context.system.scheduler.scheduleOnce(deliveryTimeout, self, deliveryCheck)
              }
            case None =>
              // Random peer has not delivered modifier we need, ask another peer
              // We need this modifier - no limit for number of attempts
              log.info(s"Modifier $modifierTypeId : ${encoder.encodeId(modifierId)} has not delivered on time")
              deliveryTracker.setUnknown(modifierId, modifierTypeId)

              val sendingStrategy =
                if (modifierTypeId == ADProofs.modifierTypeId) {
                  getPeersForDownloadingBlocks
                    .map { case (_, peers) => SendToRandomFromChosen(peers.toSeq) }
                    .getOrElse(SendToRandom)
                } else {
                  SendToRandom
                }
              requestDownload(modifierTypeId, Seq(modifierId), sendingStrategy)
          }
        }
      }
  }


  /**
    * Our node needs modifiers of type `modifierTypeId` with ids `modifierIds`
    * but peer that can deliver it is unknown.
    * Request this modifier from random peer.
    */
  def requestDownload(modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId], strategy: SendingStrategy = SendToRandom): Unit = {
    deliveryTracker.setRequested(modifierIds, modifierTypeId, None) { deliveryCheck =>
      context.system.scheduler.scheduleOnce(deliveryTimeout, self, deliveryCheck)
    }
    val msg = Message(requestModifierSpec, Right(InvData(modifierTypeId, modifierIds)), None)
    networkControllerRef ! SendToNetwork(msg, strategy)
  }

  def onDownloadRequest(historyReader: ErgoHistory): Receive = {
    case DownloadRequest(modifierTypeId: ModifierTypeId, modifierId: ModifierId) =>
      if (deliveryTracker.status(modifierId, modifierTypeId, Seq(historyReader)) == ModifiersStatus.Unknown) {
        requestDownload(modifierTypeId, Seq(modifierId))
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

  protected def penalizeMaliciousPeer(peer: ConnectedPeer): Unit = {
    networkControllerRef ! PenalizePeer(peer.connectionId.remoteAddress, PenaltyType.PermanentPenalty)
  }

  protected def broadcastInvForNewModifier(mod: PersistentNodeViewModifier): Unit = {
    mod match {
      case fb: ErgoFullBlock if fb.header.isNew(timeProvider, 1.hour) => fb.toSeq.foreach(s => broadcastModifierInv(s))
      case _ =>
    }
  }

  protected def peerManagerEvents: Receive = {
    case HandshakedPeer(remote) =>
      syncTracker.updateStatus(remote, status = Unknown, height = None)

    case DisconnectedPeer(remote) =>
      syncTracker.clearStatus(remote)
  }

  protected def getLocalSyncInfo(historyReader: ErgoHistory): Receive = {
    case SendLocalSyncInfo =>
      sendSync(historyReader)
  }


  protected def processDataFromPeer(msgHandlers: PartialFunction[(MessageSpec[_], _, ConnectedPeer), Unit]): Receive = {
    case Message(spec, Left(msgBytes), Some(source)) => parseAndHandle(msgHandlers, spec, msgBytes, source)
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

    case SuccessfulTransaction(tx) =>
      deliveryTracker.setHeld(tx.id, Transaction.ModifierTypeId)
      broadcastModifierInv(tx)

    case FailedTransaction(id, error, immediateFailure) =>
      if (immediateFailure) {
        // penalize sender only in case transaction was invalidated at first validation.
        deliveryTracker.setInvalid(id, Transaction.ModifierTypeId).foreach { peer =>
          error match {
            case TooHighCostError(_) =>
              log.info(s"Penalize spamming peer $peer for too costly transaction $id")
              penalizeSpammingPeer(peer)
            case _ =>
              penalizeMisbehavingPeer(peer)
          }
        }
      }

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

    case ModifiersRemovedFromCache(cleared: Seq[ErgoPersistentModifier]) =>
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
    case (_: InvSpec, data: InvData, remote) =>
      processInv(hr, mp, data, remote, blockAppliedTxsCache)
    case (_: RequestModifierSpec, data: InvData, remote) =>
      modifiersReq(hr, mp, data, remote)
    case (_: ModifiersSpec, data: ModifiersData, remote) =>
      modifiersFromRemote(hr, data, remote, blockAppliedTxsCache)
  }

  def initialized(hr: ErgoHistory, mp: ErgoMemPool, blockAppliedTxsCache: FixedSizeApproximateCacheQueue): PartialFunction[Any, Unit] = {
    processDataFromPeer(msgHandlers(hr, mp, blockAppliedTxsCache)) orElse
      onDownloadRequest(hr) orElse
      getLocalSyncInfo(hr) orElse
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
    case class CheckDelivery(source: Option[ConnectedPeer],
                             modifierTypeId: ModifierTypeId,
                             modifierId: ModifierId)

    trait PeerManagerEvent

    case class HandshakedPeer(remote: ConnectedPeer) extends PeerManagerEvent

    case class DisconnectedPeer(remote: InetSocketAddress) extends PeerManagerEvent

    trait NodeViewHolderEvent

    trait NodeViewChange extends NodeViewHolderEvent

    case class ChangedHistory(reader: ErgoHistoryReader) extends NodeViewChange

    case class ChangedMempool(mempool: ErgoMemPoolReader) extends NodeViewChange

    case class ChangedVault(reader: ErgoWalletReader) extends NodeViewChange

    case class ChangedState(reader: ErgoStateReader) extends NodeViewChange

    //todo: consider sending info on the rollback

    case object RollbackFailed extends NodeViewHolderEvent

    case class StartingPersistentModifierApplication(modifier: ErgoPersistentModifier) extends NodeViewHolderEvent

    /**
      * After application of batch of modifiers from cache to History, NodeViewHolder sends this message,
      * which contains modifiers cleared from cache
      */
    case class ModifiersRemovedFromCache(cleared: Seq[ErgoPersistentModifier])

    // hierarchy of events regarding modifiers application outcome
    trait ModificationOutcome extends NodeViewHolderEvent

    /**
      * @param immediateFailure - a flag indicating whether a transaction was invalid by the moment it was received.
      */
    case class FailedTransaction(transactionId: ModifierId, error: Throwable, immediateFailure: Boolean) extends ModificationOutcome

    case class SuccessfulTransaction(transaction: ErgoTransaction) extends ModificationOutcome

    case class RecoverableFailedModification(modifier: ErgoPersistentModifier, error: Throwable) extends ModificationOutcome

    case class SyntacticallyFailedModification(modifier: ErgoPersistentModifier, error: Throwable) extends ModificationOutcome

    case class SemanticallyFailedModification(modifier: ErgoPersistentModifier, error: Throwable) extends ModificationOutcome

    case class SyntacticallySuccessfulModifier(modifier: ErgoPersistentModifier) extends ModificationOutcome

    case class SemanticallySuccessfulModifier(modifier: ErgoPersistentModifier) extends ModificationOutcome

  }

  /** Alternative Peer Status dedicated only for peer syncing */
  sealed trait PeerSyncState
  object PeerSyncState {
    /** Peer for downloading headers must be older */
    case object Older extends PeerSyncState
    /** Peer downloading blocks can be older or equal */
    case object OlderOrEqual extends PeerSyncState
    /** Calling peer is always older */
    case object OlderCalling extends PeerSyncState
    /** Better Unknown or Fork than no peers */
    case object UnknownOrFork extends PeerSyncState
  }

}
