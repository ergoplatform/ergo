package scorex.core.network.peer

import org.ergoplatform.nodeView.history.ErgoHistoryConstants._
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import java.net.{InetAddress, InetSocketAddress}
import org.ergoplatform.settings.ErgoSettings
import scorex.db.LDBFactory
import scorex.util.ScorexLogging

import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/**
  * In-memory peer database implementation supporting temporal blacklisting.
  */
final class PeerDatabase(settings: ErgoSettings) extends ScorexLogging {

  private val persistentStore = LDBFactory.createKvDb(s"${settings.directory}/peers")

  private var peers =
    loadPeers match {
      case Success(loadedPeers) =>
        loadedPeers
      case Failure(ex) =>
        log.error("Unable to load peers from database, loading from network only", ex)
        Map.empty[InetSocketAddress, PeerInfo]
    }

  /**
    * banned peer ip -> ban expiration timestamp
    */
  private var blacklist = Map.empty[InetAddress, Time]

  /**
    * penalized peer ip -> (accumulated penalty score, last penalty timestamp)
    */
  private var penaltyBook = Map.empty[InetAddress, (Int, Long)]

  /*
   * Serialize object using standard Java serializer
   */
  private def serialize(obj: Object): Array[Byte] = {
    val stream: ByteArrayOutputStream = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(stream)
    oos.writeObject(obj)
    oos.close()
    stream.toByteArray
  }

  /*
   * Deserialize object using standard Java serializer
   */
  private def deserialize(bytes: Array[Byte]) : Object = {
    val ois = new ObjectInputStream(new ByteArrayInputStream(bytes))
    ois.readObject()
  }

  /*
   * Load peers from persistent storage
   */
  private def loadPeers: Try[Map[InetSocketAddress, PeerInfo]] = Try {
    var peers = Map.empty[InetSocketAddress, PeerInfo]
    for ((addr,peer) <- persistentStore.getAll) {
      val address = deserialize(addr).asInstanceOf[InetSocketAddress]
      val peerInfo = PeerInfoSerializer.parseBytes(peer)
      peers += address -> peerInfo
    }
    peers
  }

  def get(peer: InetSocketAddress): Option[PeerInfo] = peers.get(peer)

  def addOrUpdateKnownPeer(peerInfo: PeerInfo): Unit = {
    if (!peerInfo.peerSpec.declaredAddress.exists(x => isBlacklisted(x.getAddress))) {
      peerInfo.peerSpec.address.foreach { address =>
        log.debug(s"Updating peer info for $address")
        peers += address -> peerInfo
        persistentStore.insert(serialize(address), PeerInfoSerializer.toBytes(peerInfo))
      }
    }
  }

  def addToBlacklist(socketAddress: InetSocketAddress, penaltyType: PenaltyType): Unit = {
    remove(socketAddress)
    Option(socketAddress.getAddress).foreach { address =>
      penaltyBook -= address
      if (!blacklist.keySet.contains(address)){
        blacklist += address -> (System.currentTimeMillis() + penaltyDuration(penaltyType))
      } else {
        log.warn(s"${address.toString} is already blacklisted")
      }
    }
  }

  def removeFromBlacklist(address: InetAddress): Unit = {
    log.info(s"$address removed from blacklist")
    blacklist -= address
  }

  def remove(address: InetSocketAddress): Unit = {
    peers -= address
    persistentStore.remove(Array(serialize(address)))
  }

  def knownPeers: Map[InetSocketAddress, PeerInfo] = peers

  def blacklistedPeers: Seq[InetAddress] = blacklist
    .map { case (address, bannedTill) =>
      checkBanned(address, bannedTill)
      address
    }
    .toSeq

  def isEmpty: Boolean = peers.isEmpty

  def isBlacklisted(address: InetAddress): Boolean =
    blacklist.get(address).exists(checkBanned(address, _))

  def isBlacklisted(address: InetSocketAddress): Boolean =
    Option(address.getAddress).exists(isBlacklisted)

  /**
    * Registers a new penalty in the penalty book.
    *
    * @return - `true` if penalty threshold is reached, `false` otherwise.
    */
  def penalize(socketAddress: InetSocketAddress, penaltyType: PenaltyType): Boolean =
    Option(socketAddress.getAddress).exists { address =>
      val currentTime = System.currentTimeMillis()
      val safeInterval = settings.scorexSettings.network.penaltySafeInterval.toMillis
      val (penaltyScoreAcc, lastPenaltyTs) = penaltyBook.getOrElse(address, (0, 0L))
      val applyPenalty = currentTime - lastPenaltyTs - safeInterval > 0 || penaltyType.isPermanent
      val newPenaltyScore = if (applyPenalty) {
        penaltyScoreAcc + penaltyScore(penaltyType)
      } else {
        penaltyScoreAcc
      }

      if (newPenaltyScore > settings.scorexSettings.network.penaltyScoreThreshold) {
        true
      } else {
        penaltyBook += address -> (newPenaltyScore -> System.currentTimeMillis())
        false
      }
    }

  /**
    * Currently accumulated penalty score for a given address.
    */
  def penaltyScore(address: InetAddress): Int =
    penaltyBook.getOrElse(address, (0, 0L))._1

  def penaltyScore(socketAddress: InetSocketAddress): Int =
    Option(socketAddress.getAddress).map(penaltyScore).getOrElse(0)

  private def checkBanned(address: InetAddress, bannedTill: Long): Boolean = {
    val stillBanned = System.currentTimeMillis() < bannedTill
    if (!stillBanned) removeFromBlacklist(address)
    stillBanned
  }

  private def penaltyScore(penaltyType: PenaltyType): Int =
    penaltyType match {
      case PenaltyType.NonDeliveryPenalty =>
        PenaltyType.NonDeliveryPenalty.penaltyScore
      case PenaltyType.MisbehaviorPenalty =>
        PenaltyType.MisbehaviorPenalty.penaltyScore
      case PenaltyType.SpamPenalty =>
        PenaltyType.SpamPenalty.penaltyScore
      case PenaltyType.PermanentPenalty =>
        PenaltyType.PermanentPenalty.penaltyScore
    }

  private def penaltyDuration(penalty: PenaltyType): Long =
    penalty match {
      case PenaltyType.NonDeliveryPenalty | PenaltyType.MisbehaviorPenalty | PenaltyType.SpamPenalty =>
        settings.scorexSettings.network.temporalBanDuration.toMillis
      case PenaltyType.PermanentPenalty =>
        (360 * 10).days.toMillis
    }

}
