package scorex.core.network

import akka.actor.Cancellable
import io.circe.{Encoder, Json}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.network.ErgoNodeViewSynchronizer.ReceivableMessages.CheckDelivery
import org.ergoplatform.nodeView.mempool.ExpiringApproximateCache
import org.ergoplatform.settings.{ErgoSettings, NetworkCacheSettings}
import scorex.core.ModifierTypeId
import scorex.core.consensus.ContainsModifiers
import scorex.core.network.DeliveryTracker._
import scorex.core.network.ModifiersStatus._
import scorex.core.utils._
import scorex.util.{ModifierId, ScorexLogging}

import scala.collection.mutable
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

  * @param cacheSettings network cache settings
  * @param desiredSizeOfExpectingModifierQueue Approximate number of modifiers to be downloaded simultaneously,
  *                                            headers are much faster to process
  */
class DeliveryTracker(cacheSettings: NetworkCacheSettings,
                      desiredSizeOfExpectingModifierQueue: Int) extends ScorexLogging with ScorexEncoding {

  // when a remote peer is asked for a modifier we add the requested data to `requested`
  protected val requested: mutable.Map[ModifierTypeId, Map[ModifierId, RequestedInfo]] = mutable.Map()

  // when our node received a modifier we put it to `received`
  protected val received: mutable.Map[ModifierTypeId, Map[ModifierId, ConnectedPeer]] = mutable.Map()

  private val desiredSizeOfExpectingHeaderQueue: Int = desiredSizeOfExpectingModifierQueue * 5

  /** Bloom Filter based cache with invalid modifier ids */
  private var invalidModifierCache = emptyExpiringApproximateCache

  private def emptyExpiringApproximateCache = {
    val bloomFilterCapacity = cacheSettings.invalidModifiersBloomFilterCapacity
    val bloomFilterExpirationRate = cacheSettings.invalidModifiersBloomFilterExpirationRate
    val frontCacheSize = cacheSettings.invalidModifiersCacheSize
    val frontCacheExpiration = cacheSettings.invalidModifiersCacheExpiration
    ExpiringApproximateCache.empty(bloomFilterCapacity, bloomFilterExpirationRate, frontCacheSize, frontCacheExpiration)
  }

  def fullInfo: FullInfo = DeliveryTracker.FullInfo(invalidModifierCache.approximateElementCount, requested.toSeq, received.toSeq)

  def reset(): Unit = {
    log.info(s"Resetting state of DeliveryTracker...")
    requested.clear()
    received.clear()
    invalidModifierCache = emptyExpiringApproximateCache
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
    else if (invalidModifierCache.mightContain(modifierId)) Invalid
    else if (modifierKeepers.exists(_.contains(modifierId))) Held
    else Unknown

  // Write ERR message about incorrect transition into the log, so devs will find it eventually
  def checkStatusTransition(oldStatus: ModifiersStatus, expectedStatues: ModifiersStatus): Unit = {
    if (!isCorrectTransition(oldStatus, expectedStatues)) {
      log.error(s"Illegal status transition: $oldStatus -> $expectedStatues")
    }
  }

  /**
    * Set status of modifier with id `id` to `Requested`
    */
  def setRequested(typeId: ModifierTypeId,
                   id: ModifierId,
                   supplier: ConnectedPeer,
                   checksDone: Int = 0)
                  (schedule: CheckDelivery => Cancellable): Unit =
    tryWithLogging {
      checkStatusTransition(status(id, typeId, Seq.empty), Requested)
      val cancellable = schedule(CheckDelivery(supplier, typeId, id))
      val now = System.currentTimeMillis()
      val requestedInfo = RequestedInfo(supplier, cancellable, checksDone, now)
      requested.adjust(typeId)(_.fold(Map(id -> requestedInfo))(_.updated(id, requestedInfo)))
    }

  def getRequestedInfo(typeId: ModifierTypeId, id: ModifierId): Option[RequestedInfo] = {
    requested.get(typeId).flatMap(_.get(id))
  }

  /** Get peer we're communicating with in regards with modifier `id` **/
  def getSource(id: ModifierId, modifierTypeId: ModifierTypeId): Option[ConnectedPeer] = {
    status(id, modifierTypeId, Seq.empty) match {
      case Requested => requested.get(modifierTypeId).flatMap(_.get(id)).map(_.peer)
      case Received => received.get(modifierTypeId).flatMap(_.get(id))
      case _ => None
    }
  }

  /**
    * Modified with id `id` is permanently invalid - set its status to `Invalid`
    * and return [[ConnectedPeer]] which sent bad modifier.
    */
  def setInvalid(id: ModifierId, modifierTypeId: ModifierTypeId): Option[ConnectedPeer] = {
    val oldStatus: ModifiersStatus = status(id, modifierTypeId, Seq.empty)
    val transitionCheck = tryWithLogging {
      checkStatusTransition(oldStatus, Invalid)
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
                  Some(info.peer)
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
        invalidModifierCache = invalidModifierCache.put(id)
        senderOpt
      }
  }

  /**
    * Modifier with id `id` was successfully applied to history - set its status to `Held`.
    */
  def setHeld(id: ModifierId, modifierTypeId: ModifierTypeId): Unit =
    tryWithLogging {
      val oldStatus = status(id, modifierTypeId, Seq.empty)
      checkStatusTransition(oldStatus, Held)
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
      checkStatusTransition(oldStatus, Unknown)
      clearStatusForModifier(id, modifierTypeId, oldStatus) // clear old status
    }

  /**
    * Modifier with id `id`  was received from remote peer - set its status to `Received`.
    */
  def setReceived(id: ModifierId, modifierTypeId: ModifierTypeId, sender: ConnectedPeer): Unit =
    tryWithLogging {
      val oldStatus = status(id, modifierTypeId, Seq.empty)
      checkStatusTransition(oldStatus, Received)
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

  private def tryWithLogging[T](fn: => T): Try[T] =
    Try(fn).recoverWith {
      case e =>
        log.warn("Unexpected error", e)
        Failure(e)
    }

  override def toString: String = {
    val invalidModCount = s"invalid modifiers count : ${invalidModifierCache.approximateElementCount}"
    val requestedStr =
      requested.map { case (mType, infoByMid) =>
        val peersCheckTimes =
          infoByMid.toSeq.sortBy(_._2.checks).reverse.map { case (_, info) =>
            s"${info.peer.connectionId.remoteAddress} checked ${info.checks} times"
          }.mkString(", ")
        s"$mType : $peersCheckTimes"
      }.mkString("\n")
    val receivedStr =
      received.map { case (mType, peerByMid) =>
        val listOfPeers = peerByMid.values.toSet.mkString(", ")
        s"$mType : $listOfPeers"
      }.mkString("\n")
    s"$invalidModCount\nrequested modifiers:\n$requestedStr\nreceived modifiers:\n$receivedStr"
  }

}

object DeliveryTracker {

  case class RequestedInfo(peer: ConnectedPeer, cancellable: Cancellable, checks: Int, requestTime: Long)

  object RequestedInfo {
    import io.circe.syntax._

    implicit val jsonEncoder: Encoder[RequestedInfo] = { info: RequestedInfo =>
      val addressField = "address" -> info.peer.connectionId.remoteAddress.toString.asJson
      val checksField = "checks" -> info.checks.asJson
      val optionalFields =
        List(
          info.peer.peerInfo.map(_.peerSpec.protocolVersion.toString).map("version" -> _.asJson)
        ).flatten
      val fields = addressField :: checksField :: optionalFields
      Json.obj(fields:_*)
    }
  }

  case class FullInfo(
    invalidModifierApproxSize: Long,
    requested: Seq[(ModifierTypeId, Map[ModifierId, RequestedInfo])],
    received: Seq[(ModifierTypeId, Map[ModifierId, ConnectedPeer])]
  )

  object FullInfo {
    import io.circe.syntax._
    implicit val encodeState: Encoder[FullInfo] = new Encoder[FullInfo] {

      def nestedMapAsJson[T : Encoder](requested: Seq[(ModifierTypeId, Map[ModifierId, T])]): Json =
        Json.obj(
          requested.map { case (k, v) =>
            k.toString -> Json.obj(v.mapValues(_.asJson).toSeq:_*)
          }:_*
        )

      final def apply(state: FullInfo): Json = Json.obj(
        ("invalidModifierApproxSize", state.invalidModifierApproxSize.asJson),
        ("requested", nestedMapAsJson(state.requested)),
        ("received", nestedMapAsJson(state.received))
      )
    }
  }

  def empty(settings: ErgoSettings): DeliveryTracker = {
    new DeliveryTracker(
      settings.cacheSettings.network,
      settings.scorexSettings.network.desiredInvObjects
    )
  }

}
