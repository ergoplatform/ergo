package scorex.core.network

import akka.actor.SupervisorStrategy.{Restart, Stop}
import akka.actor.{Actor, ActorInitializationException, ActorKilledException, ActorRef, ActorSystem, DeathPactException, OneForOneStrategy, Props}
import akka.pattern.ask
import akka.util.Timeout
import org.ergoplatform.network.PeerSpec
import scorex.core.network.NetworkController.ReceivableMessages.{PenalizePeer, SendToNetwork}
import org.ergoplatform.network.message.{GetPeersSpec, Message, MessageSpec, PeersSpec}
import org.ergoplatform.network.peer.{PeerInfo, PenaltyType}
import org.ergoplatform.network.peer.PeerManager.ReceivableMessages.{AddPeerIfEmpty, SeenPeers}
import org.ergoplatform.settings.NetworkSettings
import scorex.util.ScorexLogging
import shapeless.syntax.typeable._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

/**
  * Responsible for discovering and sharing new peers.
  */
class PeerSynchronizer(val networkControllerRef: ActorRef,
                       peerManager: ActorRef,
                       settings: NetworkSettings)
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
    case (_: PeersSpec, peers: Seq[PeerSpec]@unchecked, _) if peers.cast[Seq[PeerSpec]].isDefined =>
      addNewPeers(peers)

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
    * Handles adding new peers to the peer database if they were previously unknown
    *
    * @param peers sequence of peer specs describing a remote peers details
    */
  private def addNewPeers(peers: Seq[PeerSpec]): Unit = {
    peers.foreach(peerSpec => peerManager ! AddPeerIfEmpty(peerSpec))
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
  def props(networkControllerRef: ActorRef, peerManager: ActorRef, settings: NetworkSettings)
           (implicit ec: ExecutionContext): Props =
    Props(new PeerSynchronizer(networkControllerRef, peerManager, settings))

  def apply(name: String, networkControllerRef: ActorRef, peerManager: ActorRef, settings: NetworkSettings)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(networkControllerRef, peerManager, settings), name)
}
