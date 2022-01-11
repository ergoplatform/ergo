package scorex.core.network

import akka.actor.{ActorRef, ActorSystem, Cancellable}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.network.ErgoNodeViewSynchronizer.ReceivableMessages.CheckDelivery
import org.ergoplatform.nodeView.mempool.ExpiringApproximateCache
import org.ergoplatform.settings.{ErgoSettings, NetworkCacheSettings}
import scorex.core.consensus.ContainsModifiers
import scorex.core.network.DeliveryTracker._
import scorex.core.network.ModifiersStatus._
import scorex.core.utils.ScorexEncoding
import scorex.core.ModifierTypeId
import scorex.util.{ModifierId, ScorexLogging}

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Try}

/**
  * This class tracks modifier statuses.
  * Modifier can be in one of the following states: Unknown, Requested, Received, Held, Invalid.
  * See ModifiersStatus for states description.
  * Modifiers in `Requested` state are kept in `requested` map containing info about peer and number of retries.
  * Modifiers in `Received` state are kept in `received` set.
  * Modifiers in `Invalid` state are kept in `invalid` set to prevent this modifier download and processing.
  * Modifiers in `Held` state are not kept in this class - we can get this status from object, that contains
  * these modifiers (History for PersistentNodeViewModifier, Mempool for EphemerealNodeViewModifier).
  * If we can't identify modifiers status based on the rules above, it's status is Unknown.
  *
  * In success path modifier changes his statuses `Unknown`->`Requested`->`Received`->`Held`.
  * If something went wrong (e.g. modifier was not delivered) it goes back to `Unknown` state
  * (if we are going to receive it in future) or to `Invalid` state (if we are not going to receive
  * this modifier anymore)
  * Locally generated modifiers may go to `Held` or `Invalid` states at any time.
  * These rules are also described in `isCorrectTransition` function.
  *
  * This class is not thread-save so it should be used only as a local field of an actor
  * and its methods should not be called from lambdas, Future, Future.map, etc.

  * @param deliveryTimeout of a single check for transition of modifier from Requested to Received
  * @param maxDeliveryChecks how many times to check whether modifier was delivered in given timeout
  * @param cacheSettings network cache settings
  * @param desiredSizeOfExpectingModifierQueue Approximate number of modifiers to be downloaded simultaneously,
  *                                            headers are much faster to process
  * @param nvsRef nodeViewSynchronizer actor reference
  */
class DeliveryTracker(system: ActorSystem,
                      deliveryTimeout: FiniteDuration,
                      maxDeliveryChecks: Int,
                      cacheSettings: NetworkCacheSettings,
                      desiredSizeOfExpectingModifierQueue: Int,
                      nvsRef: ActorRef) extends ScorexLogging with ScorexEncoding {

  protected case class RequestedInfo(peer: Option[ConnectedPeer], cancellable: Cancellable, checks: Int)

  // when a remote peer is asked for a modifier we add the requested data to `requested`
  protected val requested: mutable.Map[ModifierTypeId, Map[ModifierId, RequestedInfo]] = mutable.Map()

  // when our node received a modifier we put it to `received`
  protected val received: mutable.Map[ModifierTypeId, Map[ModifierId, ConnectedPeer]] = mutable.Map()

  private val desiredSizeOfExpectingHeaderQueue: Int = desiredSizeOfExpectingModifierQueue * 5

  /** Bloom Filter with invalid modifier ids */
  private var invalidModifierBF = {
    val bloomFilterCapacity = cacheSettings.invalidModifiersBloomFilterCapacity
    val bloomFilterExpirationRate = cacheSettings.invalidModifiersBloomFilterExpirationRate
    val frontCacheSize = cacheSettings.invalidModifiersCacheSize
    val frontCacheExpiration = cacheSettings.invalidModifiersCacheExpiration
    ExpiringApproximateCache.empty(bloomFilterCapacity, bloomFilterExpirationRate, frontCacheSize, frontCacheExpiration)
  }

  /**
    * @return how many header modifiers to download
    */
  def headersToDownload: Int =
    Math.max(0, desiredSizeOfExpectingHeaderQueue - requested.get(Header.modifierTypeId).fold(0)(_.size))

  /**
    * @return how many modifiers to download
    */
  def modifiersToDownload: Int = {
    val nonHeaderModifiersCount =
      requested.foldLeft(0) {
        case (sum, (modTypeId, _)) if modTypeId == Header.modifierTypeId =>
          sum
        case (sum, (_, mid)) =>
          sum + mid.size
      }
    Math.max(0, desiredSizeOfExpectingModifierQueue - nonHeaderModifiersCount)
  }

  /**
    * @return status of modifier `id`.
    *         Since this class do not keep statuses for modifiers that are already in NodeViewHolder,
    *         `modifierKeepers` are required here to check that modifier is in `Held` status
    */
  def status(modifierId: ModifierId, modifierTypeId: ModifierTypeId, modifierKeepers: Seq[ContainsModifiers[_]]): ModifiersStatus =
    if (received.get(modifierTypeId).exists(_.contains(modifierId))) Received
    else if (requested.get(modifierTypeId).exists(_.contains(modifierId))) Requested
    else if (invalidModifierBF.mightContain(modifierId)) Invalid
    else if (modifierKeepers.exists(_.contains(modifierId))) Held
    else Unknown

  def requireStatus(oldStatus: ModifiersStatus, expectedStatues: ModifiersStatus): Unit = {
    require(isCorrectTransition(oldStatus, expectedStatues), s"Illegal status transition: $oldStatus -> $expectedStatues")
  }

  /**
    *
    * Our node have requested a modifier, but did not received it yet.
    * Stops processing and if the number of checks did not exceed the maximum continue to waiting.
    *
    * @return `true` if number of checks was not exceed, `false` otherwise
    */
  def onStillWaiting(cp: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierId: ModifierId)
                    (implicit ec: ExecutionContext): Try[Unit] =
    tryWithLogging {
      val checks = requested(modifierTypeId)(modifierId).checks + 1
      setUnknown(modifierId, modifierTypeId)
      if (checks < maxDeliveryChecks) setRequested(modifierId, modifierTypeId,  Some(cp), checks)
      else throw new StopExpectingError(modifierId, modifierTypeId, checks)
    }

  /**
    * Set status of modifier with id `id` to `Requested`
    */
  def setRequested(id: ModifierId, typeId: ModifierTypeId, supplierOpt: Option[ConnectedPeer], checksDone: Int = 0)
                  (implicit ec: ExecutionContext): Unit =
    tryWithLogging {
      requireStatus(status(id, typeId, Seq.empty), Requested)
      val cancellable = system.scheduler.scheduleOnce(deliveryTimeout, nvsRef, CheckDelivery(supplierOpt, typeId, id))
      val requestedInfo = RequestedInfo(supplierOpt, cancellable, checksDone)
      requested.adjust(typeId)(_.fold(Map(id -> requestedInfo))(_.updated(id, requestedInfo)))
    }

  def setRequested(ids: Seq[ModifierId], typeId: ModifierTypeId, cp: Option[ConnectedPeer])
                  (implicit ec: ExecutionContext): Unit = ids.foreach(setRequested(_, typeId, cp))

  /**
    * Modified with id `id` is permanently invalid - set its status to `Invalid`
    * and return [[ConnectedPeer]] which sent bad modifier.
    */
  def setInvalid(id: ModifierId, modifierTypeId: ModifierTypeId): Option[ConnectedPeer] = {
    val oldStatus: ModifiersStatus = status(id, modifierTypeId, Seq.empty)
    val transitionCheck = tryWithLogging {
      requireStatus(oldStatus, Invalid)
    }
    transitionCheck
      .toOption
      .flatMap { _ =>
        val senderOpt = oldStatus match {
          case Requested =>
            requested.get(modifierTypeId).flatMap { infoById =>
              infoById.get(id) match {
                case None =>
                  log.warn(s"Requested modifier $id of type $modifierTypeId not found while invalidating it")
                  None
                case Some(info) =>
                  info.cancellable.cancel()
                  requested.flatAdjust(modifierTypeId)(_.map(_ - id))
                  info.peer
              }
            }
          case Received =>
            received.get(modifierTypeId).flatMap { peerById =>
              peerById.get(id) match {
                case None =>
                  log.warn(s"Received modifier $id of type $modifierTypeId not found while invalidating it")
                  None
                case Some(sender) =>
                  received.flatAdjust(modifierTypeId)(_.map(_ - id))
                  Option(sender)
              }
            }
          case _ =>
            None
        }
        invalidModifierBF = invalidModifierBF.put(id)
        senderOpt
      }
  }

  /**
    * Modifier with id `id` was successfully applied to history - set its status to `Held`.
    */
  def setHeld(id: ModifierId, modifierTypeId: ModifierTypeId): Unit =
    tryWithLogging {
      val oldStatus = status(id, modifierTypeId, Seq.empty)
      requireStatus(oldStatus, Held)
      clearStatusForModifier(id, modifierTypeId, oldStatus) // clear old status
    }

  /**
    * Set status of modifier with id `id` to `Unknown`.
    *
    * We're not trying to process modifier anymore in this case.
    * This may happen when received modifier bytes does not correspond to declared modifier id,
    * this modifier was removed from cache because cache is overfull or
    * we stop trying to download this modifiers due to exceeded number of retries
    */
  def setUnknown(id: ModifierId, modifierTypeId: ModifierTypeId): Unit =
    tryWithLogging {
      val oldStatus = status(id, modifierTypeId, Seq.empty)
      requireStatus(oldStatus, Unknown)
      clearStatusForModifier(id, modifierTypeId, oldStatus) // clear old status
    }

  /**
    * Modifier with id `id`  was received from remote peer - set its status to `Received`.
    */
  def setReceived(id: ModifierId, modifierTypeId: ModifierTypeId, sender: ConnectedPeer): Unit =
    tryWithLogging {
      val oldStatus = status(id, modifierTypeId, Seq.empty)
      requireStatus(oldStatus, Received)
      if (oldStatus != Received) {
        requested.flatAdjust(modifierTypeId)(_.map { infoById =>
          infoById.get(id) match {
            case None =>
              log.warn(s"Requested modifier $id of type $modifierTypeId not found while receiving it")
              infoById
            case Some(info) =>
              info.cancellable.cancel()
              infoById - id
          }
        })
        received.adjust(modifierTypeId)(_.fold(Map(id -> sender))(_.updated(id, sender)))
      }
    }

  /**
    * Self-check that transition between states is correct.
    *
    * Modifier may stay in current state,
    * go to Requested state form Unknown
    * go to Received state from Requested
    * go to Invalid state from any state (this may happen on invalid locally generated modifier)
    * go to Unknown state from Requested and Received states
    */
  private def isCorrectTransition(oldStatus: ModifiersStatus, newStatus: ModifiersStatus): Boolean =
    oldStatus match {
      case old if old == newStatus => true
      case _ if newStatus == Invalid || newStatus == Held => true
      case Unknown => newStatus == Requested
      case Requested => newStatus == Unknown || newStatus == Received
      case Received => newStatus == Unknown
      case _ => false
    }

  def clearStatusForModifier(id: ModifierId, modifierTypeId: ModifierTypeId, oldStatus: ModifiersStatus): Unit =
    oldStatus match {
      case Requested =>
        requested.flatAdjust(modifierTypeId)(_.map { infoById =>
          infoById.get(id) match {
            case None =>
              log.warn(s"Requested modifier $id of type $modifierTypeId not found while clearing status")
              infoById
            case Some(info) =>
              info.cancellable.cancel()
              infoById - id
          }
        })
      case Received =>
        received.flatAdjust(modifierTypeId)(_.map { peerById =>
          peerById.get(id) match {
            case None =>
              log.warn(s"Received modifier $id of type $modifierTypeId not found while clearing status")
              peerById
            case Some(_) =>
              peerById - id
          }
        })
      case _ =>
        ()
    }

  class StopExpectingError(mid: ModifierId, mType: ModifierTypeId, checks: Int)
    extends Error(s"Stop expecting ${encoder.encodeId(mid)} of type $mType due to exceeded number of retries $checks")

  private def tryWithLogging[T](fn: => T): Try[T] =
    Try(fn).recoverWith {
      case e: StopExpectingError =>
        log.warn(e.getMessage)
        Failure(e)
      case e =>
        log.warn("Unexpected error", e)
        Failure(e)
    }
}

object DeliveryTracker {
  def empty(system: ActorSystem,
            deliveryTimeout: FiniteDuration,
            maxDeliveryChecks: Int,
            nvsRef: ActorRef,
            settings: ErgoSettings): DeliveryTracker = {
    new DeliveryTracker(
      system,
      deliveryTimeout,
      maxDeliveryChecks,
      settings.cacheSettings.network,
      settings.scorexSettings.network.desiredInvObjects,
      nvsRef
    )
  }

  implicit class MapPimp[K, V](underlying: mutable.Map[K, V]) {
    /**
      * One liner for updating a Map with the possibility to handle case of missing Key
      * @param k map key
      * @param f function that is passed Option depending on Key being present or missing, returning new Value
      * @return Option depending on map being updated or not
      */
    def adjust(k: K)(f: Option[V] => V): Option[V] = underlying.put(k, f(underlying.get(k)))

    /**
      * One liner for updating a Map with the possibility to handle case of missing Key
      * @param k map key
      * @param f function that is passed Option depending on Key being present or missing,
      *          returning Option signaling whether to update or not
      * @return new Map with value updated under given key
      */
    def flatAdjust(k: K)(f: Option[V] => Option[V]): Option[V] =
      f(underlying.get(k)) match {
        case None    => None
        case Some(v) => underlying.put(k, v)
      }
  }
}
