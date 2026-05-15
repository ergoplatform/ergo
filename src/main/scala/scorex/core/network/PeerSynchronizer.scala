package scorex.core.network

import akka.actor.SupervisorStrategy.{Restart, Stop}
import akka.actor.{Actor, ActorInitializationException, ActorKilledException, ActorRef, ActorSystem, DeathPactException, OneForOneStrategy, Props}
import akka.pattern.ask
import akka.util.Timeout
import org.ergoplatform.network.PeerSpec
import scorex.core.network.NetworkController.ReceivableMessages.{PenalizePeer, SendToNetwork}
import org.ergoplatform.network.message.{GetPeersSpec, Message, MessageSpec, PeersSpec}
import org.ergoplatform.network.peer.{PeerAddressFilter, PeerInfo, PenaltyType}
import org.ergoplatform.network.peer.PeerManager.ReceivableMessages.{AddPeerIfEmpty, SeenPeers}
import org.ergoplatform.settings.{NetworkSettings, NetworkType}
import scorex.util.ScorexLogging
import shapeless.syntax.typeable._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

/**
  * Responsible for discovering and sharing new peers.
  */
class PeerSynchronizer(val networkControllerRef: ActorRef,
                       peerManager: ActorRef,
                       settings: NetworkSettings,
                       networkType: NetworkType)
                      (implicit ec: ExecutionContext) extends Actor with Synchronizer with ScorexLogging {


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

  private val peersSpec = new PeersSpec(settings.maxPeerSpecObjects)

  private val msgHandlers: PartialFunction[(MessageSpec[_], _, ConnectedPeer), Unit] = {
    case (_: PeersSpec, peers: Seq[PeerSpec]@unchecked, source) if peers.cast[Seq[PeerSpec]].isDefined =>
      addNewPeers(peers, source)

    case (spec, _, remote) if spec.messageCode == GetPeersSpec.messageCode =>
      gossipPeers(remote)
  }

  override def preStart: Unit = {
    super.preStart()

    val msg = Message[Unit](GetPeersSpec, Right(Unit), None)
    val stn = SendToNetwork(msg, SendToRandom)
    context.system.scheduler.scheduleWithFixedDelay(2.seconds, settings.getPeersInterval, networkControllerRef, stn)
  }

  override def receive: Receive = {

    // data received from a remote peer
    case Message(spec, Left(msgBytes), Some(source)) => parseAndHandle(msgHandlers, spec, msgBytes, source)

    // fall-through method for reporting unhandled messages
    case nonsense: Any => log.warn(s"PeerSynchronizer: got unexpected input $nonsense from ${sender()}")
  }

  override protected def penalizeMaliciousPeer(peer: ConnectedPeer): Unit = {
    networkControllerRef ! PenalizePeer(peer.connectionId.remoteAddress, PenaltyType.PermanentPenalty)
  }

  /**
    * Handles adding new peers to the peer database if they were previously unknown.
    *
    * Each entry's declared address is checked against [[PeerAddressFilter]];
    * bogus entries (link-local, multicast, loopback, RFC 1918 on mainnet, etc.)
    * are dropped. If any entry in the body is bogus, the gossiping `source` peer
    * is penalized — no legitimate Ergo node has any reason to advertise an
    * unroutable address to other peers.
    *
    * @param peers  sequence of peer specs describing remote peers' details
    * @param source the peer that sent us this `Peers` gossip message
    */
  private def addNewPeers(peers: Seq[PeerSpec], source: ConnectedPeer): Unit = {
    val (clean, bogus) = peers.partition { spec =>
      spec.declaredAddress match {
        case Some(addr) => !PeerAddressFilter.isBogus(addr, networkType)
        case None       => true // no declared address to filter; let PeerManager handle
      }
    }
    clean.foreach(peerSpec => peerManager ! AddPeerIfEmpty(peerSpec))
    if (bogus.nonEmpty) {
      val examples = bogus.flatMap(_.declaredAddress).take(3).mkString(", ")
      log.warn(s"$source gossiped ${bogus.size} bogus peer address(es) (e.g. $examples) on ${networkType.verboseName} — penalizing")
      networkControllerRef ! PenalizePeer(source.connectionId.remoteAddress, PenaltyType.MisbehaviorPenalty)
    }
  }

  /**
    * Handles gossiping about the locally known peer set to a given remote peer
    *
    * @param remote the remote peer to be informed of our local peers
    */
  private def gossipPeers(remote: ConnectedPeer): Unit = {
    implicit val timeout: Timeout = Timeout(settings.syncTimeout.getOrElse(5.seconds))

    // we send less peer that can be accepted, starting from 5.0.8
    val maxToSend = settings.maxPeerSpecObjects
    val peersToSend = if (maxToSend >= 16) {
      maxToSend / 8
    } else {
      maxToSend
    }

    (peerManager ? SeenPeers(peersToSend))
      .mapTo[Seq[PeerInfo]]
      .foreach { peers =>
        val msg = Message(peersSpec, Right(peers.map(_.peerSpec)), None)
        networkControllerRef ! SendToNetwork(msg, SendToPeer(remote))
      }
  }
}

object PeerSynchronizerRef {
  def props(networkControllerRef: ActorRef, peerManager: ActorRef, settings: NetworkSettings, networkType: NetworkType)
           (implicit ec: ExecutionContext): Props =
    Props(new PeerSynchronizer(networkControllerRef, peerManager, settings, networkType))

  def apply(name: String, networkControllerRef: ActorRef, peerManager: ActorRef, settings: NetworkSettings, networkType: NetworkType)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(networkControllerRef, peerManager, settings, networkType), name)
}
