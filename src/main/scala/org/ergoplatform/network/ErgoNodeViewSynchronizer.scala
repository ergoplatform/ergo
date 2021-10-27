package org.ergoplatform.network

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.{ErgoSyncInfoV1, ErgoSyncInfoV2}
import org.ergoplatform.nodeView.history._
import org.ergoplatform.network.ErgoNodeViewSynchronizer.{CheckModifiersToDownload, PeerSyncState}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfo, ErgoSyncInfoMessageSpec}
import org.ergoplatform.nodeView.mempool.{ErgoMemPool, ErgoMemPoolReader}
import org.ergoplatform.settings.{Constants, ErgoSettings}
import scorex.core.NodeViewHolder.ReceivableMessages.{GetNodeViewChanges, ModifiersFromRemote, TransactionsFromRemote}
import scorex.core.NodeViewHolder._
import scorex.core.app.Version
import scorex.core.consensus.History.{Equal, Fork, Nonsense, Older, Unknown, Younger}
import scorex.core.consensus.{HistoryReader, SyncInfo}
import scorex.core.network.ModifiersStatus.Requested
import scorex.core.{ModifierTypeId, NodeViewModifier, PersistentNodeViewModifier, idsToString}
import scorex.core.network.NetworkController.ReceivableMessages.{PenalizePeer, RegisterMessageSpecs}
import org.ergoplatform.network.ErgoNodeViewSynchronizer.ReceivableMessages._
import scorex.core.network.message.{InvSpec, MessageSpec, ModifiersSpec, RequestModifierSpec}
import scorex.core.network._
import scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork
import scorex.core.network.message.{InvData, Message, ModifiersData}
import scorex.core.network.{ConnectedPeer, ModifiersStatus, SendToPeer, SendToPeers}
import scorex.core.serialization.ScorexSerializer
import scorex.core.settings.NetworkSettings
import scorex.core.transaction.{MempoolReader, Transaction}
import scorex.core.utils.{NetworkTimeProvider, ScorexEncoding}
import scorex.core.validation.MalformedModifierError
import scorex.util.{ModifierId, ScorexLogging}
import scorex.core.network.DeliveryTracker
import scorex.core.network.peer.PenaltyType
import scorex.core.transaction.state.StateReader
import scorex.core.transaction.wallet.VaultReader

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
                               syncTracker: ErgoSyncTracker
                              )(implicit ex: ExecutionContext)
  extends Actor with Synchronizer with ScorexLogging with ScorexEncoding {

  private val networkSettings: NetworkSettings = settings.scorexSettings.network

  protected val deliveryTimeout: FiniteDuration = networkSettings.deliveryTimeout
  protected val maxDeliveryChecks: Int = networkSettings.maxDeliveryChecks

  protected val invSpec = new InvSpec(networkSettings.maxInvObjects)
  protected val requestModifierSpec = new RequestModifierSpec(networkSettings.maxInvObjects)
  protected val modifiersSpec = new ModifiersSpec(networkSettings.maxPacketSize)

  protected val msgHandlers: PartialFunction[(MessageSpec[_], _, ConnectedPeer), Unit] = {
    case (_: ErgoSyncInfoMessageSpec.type @unchecked, data: ErgoSyncInfo @unchecked, remote) => processSync(data, remote)
    case (_: InvSpec, data: InvData, remote)              => processInv(data, remote)
    case (_: RequestModifierSpec, data: InvData, remote)  => modifiersReq(data, remote)
    case (_: ModifiersSpec, data: ModifiersData, remote)  => modifiersFromRemote(data, remote)
  }

  protected val deliveryTracker: DeliveryTracker =
    DeliveryTracker.empty(context.system, deliveryTimeout, maxDeliveryChecks, self, settings)

  protected var historyReaderOpt: Option[ErgoHistory] = None
  protected var mempoolReaderOpt: Option[ErgoMemPool] = None

  /**
    * Approximate number of modifiers to be downloaded simultaneously, headers are much faster to process
    */
  private val desiredSizeOfExpectingModifierQueue: Int = networkSettings.desiredInvObjects
  private val desiredSizeOfExpectingHeaderQueue: Int = desiredSizeOfExpectingModifierQueue * 5

  private val minModifiersPerBucket = 5 // minimum of persistent modifiers (excl. headers) to download by single peer
  private val maxModifiersPerBucket = 20 // maximum of persistent modifiers (excl. headers) to download by single peer

  private val minHeadersPerBucket = 50 // minimum of headers to download by single peer
  private val maxHeadersPerBucket = 400 // maximum of headers to download by single peer

  /**
    * Register periodic events
    */
  override def preStart(): Unit = {
    val toDownloadCheckInterval = networkSettings.syncInterval

    // register as a handler for synchronization-specific types of messages
    val messageSpecs: Seq[MessageSpec[_]] = Seq(invSpec, requestModifierSpec, modifiersSpec, syncInfoSpec)
    networkControllerRef ! RegisterMessageSpecs(messageSpecs, self)

    // register as a listener for peers got connected (handshaked) or disconnected
    context.system.eventStream.subscribe(self, classOf[HandshakedPeer])
    context.system.eventStream.subscribe(self, classOf[DisconnectedPeer])

    // subscribe for all the node view holder events involving modifiers and transactions
    context.system.eventStream.subscribe(self, classOf[ChangedHistory[ErgoHistoryReader]])
    context.system.eventStream.subscribe(self, classOf[ChangedMempool[ErgoMemPoolReader]])
    context.system.eventStream.subscribe(self, classOf[ModificationOutcome])
    context.system.eventStream.subscribe(self, classOf[DownloadRequest])
    context.system.eventStream.subscribe(self, classOf[ModifiersProcessingResult])

    // subscribe for history and mempool changes
    viewHolderRef ! GetNodeViewChanges(history = true, state = false, vault = false, mempool = true)

    context.system.scheduler.scheduleAtFixedRate(toDownloadCheckInterval, toDownloadCheckInterval, self, CheckModifiersToDownload)

    val interval = networkSettings.syncInterval
    context.system.scheduler.scheduleWithFixedDelay(2.seconds, interval, self, SendLocalSyncInfo)
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
  private def downloadRequired(historyReader: ErgoHistory)(id: ModifierId): Boolean = {
    deliveryTracker.status(id, Array(historyReader)) == ModifiersStatus.Unknown
  }

  /**
    * Whether neighbour peer `remote` supports sync protocol V2.
    */
  def syncV2Supported(remote: ConnectedPeer): Boolean = {
    // If neighbour version is >= 4.0.16, the neighbour supports sync V2
    val syncV2Version = Version(4, 0, 16)
    remote.peerInfo.exists(_.peerSpec.protocolVersion >= syncV2Version)
  }

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
      networkControllerRef ! SendToNetwork(Message(syncInfoSpec, Right(history.syncInfoV1), None), SendToPeers(peersV1))
    }
    if (peersV2.nonEmpty) {
      //todo: send only last header to peers which are equal or younger
      val v2SyncInfo = history.syncInfoV2(full = true)
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

  /**
    * Process sync message `syncInfo` got from neighbour peer `remote`
    */
  protected def processSync(syncInfo: ErgoSyncInfo, remote: ConnectedPeer): Unit = {
    syncInfo match {
      case syncV1: ErgoSyncInfoV1 => processSyncV1(syncV1, remote)
      case syncV2: ErgoSyncInfoV2 => processSyncV2(syncV2, remote)
    }
  }

  /**
    * Processing sync V1 message `syncInfo` got from neighbour peer `remote`
    */
  protected def processSyncV1(syncInfo: ErgoSyncInfoV1, remote: ConnectedPeer): Unit = {

    historyReaderOpt match {
      case Some(historyReader) =>

        val comparison = historyReader.compare(syncInfo)
        log.debug(s"Comparison with $remote having starting points ${syncInfo.lastHeaderIds}. " +
          s"Comparison result is $comparison.")

        val oldStatus = syncTracker.getStatus(remote).getOrElse(Unknown)
        val status = comparison
        syncTracker.updateStatus(remote, status, height = None)

        status match {
          case Unknown =>
            // we do not know what to send to a peer with unknown status
            log.info(s"Peer status is still unknown for $remote")
          case Nonsense =>
            // we do not know what to send to a peer with such status
            log.info(s"Got nonsense status for $remote")
          case Younger | Fork =>
            // send extension (up to 400 header ids) to a peer which chain is less developed or forked
            val ext = historyReader.continuationIds(syncInfo, size = 400)
            if (ext.isEmpty) log.warn("Extension is empty while comparison is younger")
            log.info(s"Sending extension of length ${ext.length}")
            log.debug(s"Extension ids: ${idsToString(ext)}")
            sendExtension(remote, ext)
          case Older =>
            // asking headers from older peers
            val ids = syncInfo.lastHeaderIds.reverse
            val headerIds = ids.takeWhile(hId => !historyReader.isInBestChain(hId))
            if (headerIds.nonEmpty) {
              val maxModifiers = desiredSizeOfExpectingHeaderQueue - deliveryTracker.requestedSize
              log.debug(s"Requesting $maxModifiers headers from older peers after getting sync info from $remote")
              requestDownload(
                maxModifiers,
                minHeadersPerBucket,
                maxHeadersPerBucket
              )(Option(getPeersForDownloadingHeaders(remote))) { howManyPerType =>
                Map(Header.modifierTypeId -> headerIds.reverse.filter(downloadRequired(historyReader)).take(howManyPerType))
              }
            }
          case Equal =>
            // does nothing for `Equal`
            log.debug(s"$remote has equal header-chain")
        }

        if ((oldStatus != status) || syncTracker.isOutdated(remote) || status == Older || status == Fork) {
          val ownSyncInfo = historyReader.syncInfoV1
          sendSyncToPeer(remote, ownSyncInfo)
        }

      case _ =>
        // historyReader not initialized yet, it should not happen
        log.error("historyReader not initialized when processing syncInfo")
    }
  }

  /**
    * Processing sync V2 message `syncInfo` got from neighbour peer `remote` (supporting sync v2)
    */
  protected def processSyncV2(syncInfo: ErgoSyncInfoV2, remote: ConnectedPeer): Unit = {
    historyReaderOpt match {
      case Some(historyReader) =>

        val oldStatus = syncTracker.getStatus(remote).getOrElse(Unknown)
        val status = historyReader.compare(syncInfo)
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
            // send extension (up to 400 header ids) to a peer which chain is less developed or forked
            val ext = historyReader.continuationIds(syncInfo, size = 400)
            if (ext.isEmpty) log.warn("Extension is empty while comparison is younger")
            log.info(s"Sending extension of length ${ext.length}")
            log.debug(s"Extension ids: ${idsToString(ext)}")
            sendExtension(remote, ext)

          case Fork =>
            log.info(s"Fork detected with peer $remote, its sync message $syncInfo")

          case Older =>
            log.info(s"Peer $remote is older, its height ${syncInfo.height}")

          case Equal =>
            // does nothing for `Equal`
            log.debug(s"$remote has equal header-chain")
        }

        if ((oldStatus != status) || syncTracker.isOutdated(remote) || status == Older || status == Fork) {
          val ownSyncInfo = historyReader.syncInfoV2(full = true)
          sendSyncToPeer(remote, ownSyncInfo)
        }

      case _ =>
        // historyReader not initialized yet, it should not happen
        log.error("historyReader not initialized when processing syncInfo")
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
      }
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
          deliveryTracker.setRequested(modifierIds, modifierTypeId, Some(peer))
          val msg = Message(requestModifierSpec, Right(InvData(modifierTypeId, modifierIds)), None)
          networkControllerRef ! SendToNetwork(msg, SendToPeer(peer))
        }
      }

  /**
    * Logic to process block parts got from another peer.
    * Filter out non-requested block parts (with a penalty to spamming peer),
    * parse block parts and send valid modifiers to NodeViewHolder
    *
    */
  protected def modifiersFromRemote(data: ModifiersData, remote: ConnectedPeer): Unit = {
    val typeId = data.typeId
    val modifiers = data.modifiers
    log.info(s"Got ${modifiers.size} modifiers of type $typeId from remote connected peer: $remote")
    log.debug("Modifier ids: " + modifiers.keys)

    // filter out non-requested modifiers
    val requestedModifiers = processSpam(remote, typeId, modifiers)

    Constants.modifierSerializers.get(typeId) match {
      case Some(serializer: ScorexSerializer[ErgoTransaction]@unchecked) if typeId == Transaction.ModifierTypeId =>
        // parse all transactions and send them to node view holder
        val parsed: Iterable[ErgoTransaction] = parseModifiers(requestedModifiers, serializer, remote)
        viewHolderRef ! TransactionsFromRemote(parsed)

      case Some(serializer: ScorexSerializer[ErgoPersistentModifier]@unchecked) =>
        // parse all modifiers and put them to modifiers cache
        val parsed: Iterable[ErgoPersistentModifier] = parseModifiers(requestedModifiers, serializer, remote)
        val valid = parsed.filter(validateAndSetStatus(remote, _))
        if (valid.nonEmpty) {
          viewHolderRef ! ModifiersFromRemote(valid)

          // send sync message to the peer to get new headers quickly
          if (valid.head.isInstanceOf[Header] && historyReaderOpt.isDefined) {
            val historyReader = historyReaderOpt.get
            val syncInfo = if (syncV2Supported(remote)) {
              historyReader.syncInfoV2(full = false)
            } else {
              historyReader.syncInfoV1
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
                          modifiers: Map[ModifierId, Array[Byte]]): Map[ModifierId, Array[Byte]] = {
    val (requested, spam) = modifiers.partition { case (id, _) =>
      deliveryTracker.status(id) == Requested
    }

    if (spam.nonEmpty) {
      if (typeId == Transaction.ModifierTypeId) {
        log.info(s"Got spammy transactions: $modifiers")
        // todo: consider rules for penalizing peers for spammy transactions
      } else {
        log.info(s"Spam attempt: peer $remote has sent a non-requested modifiers of type $typeId with ids" +
          s": ${spam.keys.map(encoder.encodeId)}")
        penalizeSpammingPeer(remote)
      }
    }
    requested
  }

  /**
    * Object ids coming from other node.
    * Filter out modifier ids that are already in process (requested, received or applied),
    * request unknown ids from peer and set this ids to requested state.
    */
  protected def processInv(invData: InvData, peer: ConnectedPeer): Unit = {
    (mempoolReaderOpt, historyReaderOpt) match {
      case (Some(mempool), Some(history)) =>

        val modifierTypeId = invData.typeId

        val newModifierIds = modifierTypeId match {
          case Transaction.ModifierTypeId =>
            // We download transactions only if the node is not needed for externally provided proofs
            // (so having UTXO set, and the chain is synced
            if (!settings.nodeSettings.stateType.requireProofs &&
              history.isHeadersChainSynced &&
              history.fullBlockHeight == history.headersHeight) {
              invData.ids.filter(mid => deliveryTracker.status(mid, mempool) == ModifiersStatus.Unknown)
            } else {
              Seq.empty
            }
          case _ =>
            invData.ids.filter(mid => deliveryTracker.status(mid, history) == ModifiersStatus.Unknown)
        }

        log.info(s"Going to request ${newModifierIds.length} modifiers of type $modifierTypeId from $peer")
        if (newModifierIds.nonEmpty) {
          val msg = Message(requestModifierSpec, Right(InvData(modifierTypeId, newModifierIds)), None)
          peer.handlerRef ! msg
          deliveryTracker.setRequested(newModifierIds, modifierTypeId, Some(peer))
        }

      case _ =>
        log.warn(s"Got data from peer while readers are not ready ${(mempoolReaderOpt, historyReaderOpt)}")
    }
  }

  /**
    * If our requested list is more than half empty, enforce to request more:
    * - headers, if our headers chain is not synced yet (by sending sync message)
    * - block sections, if our headers chain is synced
    */
  protected def requestMoreModifiers(applied: Seq[ErgoPersistentModifier]): Unit = {
    historyReaderOpt foreach { h =>
      if (h.isHeadersChainSynced) {
        // our requested list is is half empty - request more missed modifiers
        self ! CheckModifiersToDownload
      } else {
        // headers chain is not synced yet, but our requested list is half empty - ask for more headers
        sendSync(h)
      }
    }
  }

  //other node asking for objects by their ids
  protected def modifiersReq(invData: InvData, remote: ConnectedPeer): Unit = {
      val objs: Seq[NodeViewModifier] = invData.typeId match {
        case typeId: ModifierTypeId if typeId == Transaction.ModifierTypeId =>
          mempoolReaderOpt.toSeq.flatMap {mp =>
            mp.getAll(invData.ids)
          }
        case _: ModifierTypeId =>
          historyReaderOpt.toSeq.flatMap { h =>
            invData.ids.flatMap(id => h.modifierById(id))
          }
      }

      log.debug(s"Requested ${invData.ids.length} modifiers ${idsToString(invData)}, " +
        s"sending ${objs.length} modifiers ${idsToString(invData.typeId, objs.map(_.id))} ")
      self ! ResponseFromLocal(remote, invData.typeId, objs)
  }

  /**
    * Move `pmod` to `Invalid` if it is permanently invalid, to `Received` otherwise
    */
  @SuppressWarnings(Array("org.wartremover.warts.IsInstanceOf"))
  def validateAndSetStatus(remote: ConnectedPeer, pmod: ErgoPersistentModifier): Boolean = {
    historyReaderOpt match {
      case Some(hr) =>
        hr.applicableTry(pmod) match {
          case Failure(e) if e.isInstanceOf[MalformedModifierError] =>
            log.warn(s"Modifier ${pmod.encodedId} is permanently invalid", e)
            deliveryTracker.setInvalid(pmod.id)
            penalizeMisbehavingPeer(remote)
            false
          case _ =>
            deliveryTracker.setReceived(pmod.id, remote)
            true
        }
      case None =>
        log.error("Got modifier while history reader is not ready")
        deliveryTracker.setReceived(pmod.id, remote)
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
      if (deliveryTracker.status(modifierId) == ModifiersStatus.Requested) {
        // If transaction not delivered on time, we just forget about it.
        // It could be removed from other peer's mempool, so no reason to penalize the peer.
        if (modifierTypeId == Transaction.ModifierTypeId) {
          deliveryTracker.clearStatusForModifier(modifierId, ModifiersStatus.Requested)
        } else {
          // A persistent modifier is not delivered on time.
          peerOpt match {
            case Some(peer) =>
              log.info(s"Peer ${peer.toString} has not delivered asked modifier ${encoder.encodeId(modifierId)} on time")
              penalizeNonDeliveringPeer(peer)
              deliveryTracker.onStillWaiting(peer, modifierTypeId, modifierId)
            case None =>
              // Random peer has not delivered modifier we need, ask another peer
              // We need this modifier - no limit for number of attempts
              log.info(s"Modifier ${encoder.encodeId(modifierId)} has not delivered on time")
              deliveryTracker.setUnknown(modifierId)
              requestDownload(modifierTypeId, Seq(modifierId))
          }
        }
      }
  }


  /**
    * Local node sending out objects requested to remote
    */
  protected def responseFromLocal: Receive = {
    case ResponseFromLocal(peer, _, modifiers: Seq[NodeViewModifier]) =>
      modifiers.headOption.foreach { head =>
        val modType = head.modifierTypeId

        @tailrec
        def sendByParts(mods: Seq[(ModifierId, Array[Byte])]): Unit = {
          var size = 5 //message type id + message size
          val batch = mods.takeWhile { case (_, modBytes) =>
            size += NodeViewModifier.ModifierIdSize + 4 + modBytes.length
            size < networkSettings.maxPacketSize
          }
          peer.handlerRef ! Message(modifiersSpec, Right(ModifiersData(modType, batch.toMap)), None)
          val remaining = mods.drop(batch.length)
          if (remaining.nonEmpty) {
            sendByParts(remaining)
          }
        }

        Constants.modifierSerializers.get(modType) match {
          case Some(serializer: ScorexSerializer[NodeViewModifier]) =>
            sendByParts(modifiers.map(m => m.id -> serializer.toBytes(m)))
          case _ =>
            log.error(s"Undefined serializer for modifier of type $modType")
        }
      }
  }

  /**
    * Our node needs modifiers of type `modifierTypeId` with ids `modifierIds`
    * but peer that can deliver it is unknown.
    * Request this modifier from random peer.
    */
  def requestDownload(modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId]): Unit = {
    deliveryTracker.setRequested(modifierIds, modifierTypeId, None)
    val msg = Message(requestModifierSpec, Right(InvData(modifierTypeId, modifierIds)), None)
    networkControllerRef ! SendToNetwork(msg, SendToRandom)
  }

  def onDownloadRequest: Receive = {
    case DownloadRequest(modifierTypeId: ModifierTypeId, modifierId: ModifierId) =>
      if (deliveryTracker.status(modifierId, historyReaderOpt.toSeq) == ModifiersStatus.Unknown) {
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

  protected def getLocalSyncInfo: Receive = {
    case SendLocalSyncInfo =>
      historyReaderOpt.foreach(sendSync(_))
  }


  protected def processDataFromPeer: Receive = {
    case Message(spec, Left(msgBytes), Some(source)) => parseAndHandle(spec, msgBytes, source)
  }

  protected def viewHolderEvents: Receive = {
    // Requests BlockSections with `Unknown` status that are defined by block headers but not downloaded yet.
    // Trying to keep size of requested queue equals to `desiredSizeOfExpectingQueue`.

    case CheckModifiersToDownload =>
      historyReaderOpt.foreach { h =>
        val maxModifiers = desiredSizeOfExpectingModifierQueue - deliveryTracker.requestedSize
        log.debug(s"Going to download $maxModifiers non-header modifiers")
        requestDownload(
          maxModifiers,
          minModifiersPerBucket,
          maxModifiersPerBucket
        )(getPeersForDownloadingBlocks) { howManyPerType =>
          h.nextModifiersToDownload(howManyPerType, downloadRequired(h))
        }
      }

    // If new enough semantically valid ErgoFullBlock was applied, send inv for block header and all its sections
    case SemanticallySuccessfulModifier(mod) =>
      broadcastInvForNewModifier(mod)

    case SuccessfulTransaction(tx) =>
      deliveryTracker.setHeld(tx.id)
      broadcastModifierInv(tx)

    case FailedTransaction(id, _, immediateFailure) =>
      val senderOpt = deliveryTracker.setInvalid(id)
      // penalize sender only in case transaction was invalidated at first validation.
      if (immediateFailure) senderOpt.foreach(penalizeMisbehavingPeer)

    case SyntacticallySuccessfulModifier(mod) =>
      deliveryTracker.setHeld(mod.id)

    case SyntacticallyFailedModification(mod, _) =>
      deliveryTracker.setInvalid(mod.id).foreach(penalizeMisbehavingPeer)

    case SemanticallyFailedModification(mod, _) =>
      deliveryTracker.setInvalid(mod.id).foreach(penalizeMisbehavingPeer)

    case ChangedHistory(reader: ErgoHistory) =>
      historyReaderOpt = Some(reader)

    case ChangedMempool(reader: ErgoMemPool) =>
      mempoolReaderOpt = Some(reader)

    case ModifiersProcessingResult(applied: Seq[ErgoPersistentModifier], cleared: Seq[ErgoPersistentModifier]) =>
      // stop processing for cleared modifiers
      // applied modifiers state was already changed at `SyntacticallySuccessfulModifier`
      cleared.foreach(m => deliveryTracker.setUnknown(m.id))
      requestMoreModifiers(applied)
  }


  override def receive: Receive =
    processDataFromPeer orElse
      onDownloadRequest orElse
      getLocalSyncInfo orElse
      responseFromLocal orElse
      viewHolderEvents orElse
      peerManagerEvents orElse
      checkDelivery orElse {
      case a: Any => log.error("Strange input: " + a)
    }

}

object ErgoNodeViewSynchronizer {

  def props(networkControllerRef: ActorRef,
            viewHolderRef: ActorRef,
            syncInfoSpec: ErgoSyncInfoMessageSpec.type,
            settings: ErgoSettings,
            timeProvider: NetworkTimeProvider,
            syncTracker: ErgoSyncTracker)
           (implicit ex: ExecutionContext): Props =
    Props(new ErgoNodeViewSynchronizer(networkControllerRef, viewHolderRef, syncInfoSpec, settings,
      timeProvider, syncTracker))

  def apply(networkControllerRef: ActorRef,
            viewHolderRef: ActorRef,
            syncInfoSpec: ErgoSyncInfoMessageSpec.type,
            settings: ErgoSettings,
            timeProvider: NetworkTimeProvider,
            syncTracker: ErgoSyncTracker)
           (implicit context: ActorRefFactory, ex: ExecutionContext): ActorRef =
    context.actorOf(props(networkControllerRef, viewHolderRef, syncInfoSpec, settings, timeProvider, syncTracker))

  case object CheckModifiersToDownload

  object Events {

    trait NodeViewSynchronizerEvent

    case object NoBetterNeighbour extends NodeViewSynchronizerEvent

    case object BetterNeighbourAppeared extends NodeViewSynchronizerEvent

  }

  object ReceivableMessages {

    // getLocalSyncInfo messages
    case object SendLocalSyncInfo

    case class ResponseFromLocal[M <: NodeViewModifier](source: ConnectedPeer, modifierTypeId: ModifierTypeId, localObjects: Seq[M])

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

    case class ChangedHistory[HR <: HistoryReader[_ <: PersistentNodeViewModifier, _ <: SyncInfo]](reader: HR) extends NodeViewChange

    case class ChangedMempool[MR <: MempoolReader[_ <: Transaction]](mempool: MR) extends NodeViewChange

    case class ChangedVault[VR <: VaultReader](reader: VR) extends NodeViewChange

    case class ChangedState[SR <: StateReader](reader: SR) extends NodeViewChange

    //todo: consider sending info on the rollback

    case object RollbackFailed extends NodeViewHolderEvent

    case class NewOpenSurface(newSurface: Seq[ModifierId]) extends NodeViewHolderEvent

    case class StartingPersistentModifierApplication(modifier: ErgoPersistentModifier) extends NodeViewHolderEvent

    /**
      * After application of batch of modifiers from cache to History, NodeViewHolder sends this message,
      * containing all just applied modifiers and cleared from cache
      */
    case class ModifiersProcessingResult(applied: Seq[ErgoPersistentModifier], cleared: Seq[ErgoPersistentModifier])

    // hierarchy of events regarding modifiers application outcome
    trait ModificationOutcome extends NodeViewHolderEvent

    /**
      * @param immediateFailure - a flag indicating whether a transaction was invalid by the moment it was received.
      */
    case class FailedTransaction(transactionId: ModifierId, error: Throwable, immediateFailure: Boolean) extends ModificationOutcome

    case class SuccessfulTransaction(transaction: ErgoTransaction) extends ModificationOutcome

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
