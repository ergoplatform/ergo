package scorex.core.network

import akka.actor.{Actor, ActorRef, Cancellable, Props, SupervisorStrategy}
import akka.io.Tcp
import akka.io.Tcp._
import akka.util.{ByteString, CompactByteString}
import org.ergoplatform.network.{Handshake, HandshakeSerializer, PeerSpec, Version}
import org.ergoplatform.network.Version.Eip37ForkVersion
import scorex.core.app.ScorexContext
import scorex.core.network.NetworkController.ReceivableMessages.{Handshaked, PenalizePeer}
import scorex.core.network.PeerConnectionHandler.ReceivableMessages
import org.ergoplatform.network.message.MessageSerializer
import org.ergoplatform.network.peer.{PeerInfo, PenaltyType}
import org.ergoplatform.settings.ScorexSettings
import scorex.util.ScorexLogging

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

class PeerConnectionHandler(scorexSettings: ScorexSettings,
                            networkControllerRef: ActorRef,
                            scorexContext: ScorexContext,
                            connectionDescription: ConnectionDescription
                           )(implicit ec: ExecutionContext)
  extends Actor with ScorexLogging {

  import PeerConnectionHandler.ReceivableMessages._

  private val networkSettings = scorexSettings.network
  private val connection = connectionDescription.connection
  private val connectionId = connectionDescription.connectionId
  private val direction = connectionDescription.connectionId.direction
  private val ownSocketAddress = connectionDescription.ownSocketAddress
  private val localFeatures = connectionDescription.localFeatures

  private val messageSerializer = new MessageSerializer(scorexContext.messageSpecs, networkSettings.magicBytes)

  // there is no recovery for broken connections
  override val supervisorStrategy: SupervisorStrategy = SupervisorStrategy.stoppingStrategy

  private var selfPeer: Option[ConnectedPeer] = None

  private var handshakeTimeoutCancellableOpt: Option[Cancellable] = None

  private var chunksBuffer: ByteString = CompactByteString.empty

  private var outMessagesBuffer: TreeMap[Long, ByteString] = TreeMap.empty

  private var outMessagesCounter: Long = 0

  override def preStart: Unit = {
    context watch connection
    connection ! Register(self, keepOpenOnPeerClosed = false, useResumeWriting = true)
    connection ! ResumeReading

    context.become(handshaking)
  }

  override def receive: Receive = reportStrangeInput

  override def postStop(): Unit = log.info(s"Peer handler to $connectionId destroyed")

  private def handshaking: Receive = {
    handshakeTimeoutCancellableOpt =
      Some(context.system.scheduler.scheduleOnce(networkSettings.handshakeTimeout)(self ! HandshakeTimeout))
    val hb = HandshakeSerializer.toBytes(createHandshakeMessage())
    connection ! Tcp.Write(ByteString(hb))
    log.info(s"Handshake sent to $connectionId")

    receiveAndHandleHandshake { receivedHandshake =>
      log.info(s"Got a Handshake from $connectionId")

      val peerInfo = PeerInfo(receivedHandshake.peerSpec, System.currentTimeMillis(), Some(direction))
      val peer = ConnectedPeer(connectionDescription.connectionId, self, Some(peerInfo))
      selfPeer = Some(peer)

      networkControllerRef ! Handshaked(peerInfo)
      handshakeTimeoutCancellableOpt.map(_.cancel())
      connection ! ResumeReading
      context become workingCycleWriting
    } orElse handshakeTimeout orElse closeCommands
  }

  //ban this peer for the wrong handshake message
  //peer will be added to the blacklist and the network controller will send CloseConnection
  private def banPeer(): Unit = {
    selfPeer.foreach(c => networkControllerRef ! PenalizePeer(c.connectionId.remoteAddress, PenaltyType.PermanentPenalty))
  }

  private def receiveAndHandleHandshake(handler: Handshake => Unit): Receive = {
    case Received(data) =>
      HandshakeSerializer.parseBytesTry(data.toArray) match {
        case Success(handshake) =>
          if (handshake.peerSpec.protocolVersion < Eip37ForkVersion) {
            // peers not suporting EIP-37 hard-fork are stuck on another chain
            log.info(s"Peer of version < 4.0.100 sent handshake $handshake")
            banPeer()
          } else {
            handler(handshake)
          }

        case Failure(t) =>
          log.info(s"Error during parsing a handshake: ${t.getMessage}", t)
          banPeer()
      }
  }

  private def handshakeTimeout: Receive = {
    case HandshakeTimeout =>
      log.info(s"Handshake timeout with $connectionId, going to drop the connection")
      self ! CloseConnection
  }

  private def workingCycleWriting: Receive =
    localInterfaceWriting orElse
      remoteInterface orElse
      closeCommands orElse
      reportStrangeInput

  private def workingCycleBuffering: Receive =
    localInterfaceBuffering orElse
      remoteInterface orElse
      closeCommands orElse
      reportStrangeInput

  private def closeCommands: Receive = {
    case CloseConnection =>
      log.info(s"Enforced to abort communication with: " + connectionId)
      pushAllWithNoAck()
      connection ! Abort // we're closing connection without waiting for a confirmation from the peer

    case cc: ConnectionClosed =>
      // connection closed from either side, actor is shutting down itself
      val reason: String = if (cc.isErrorClosed) {
        "error: " + cc.getErrorCause
      } else if (cc.isPeerClosed) {
        "closed by the peer"
      } else if (cc.isAborted) {
        "aborted locally"
      } else {
        ""
      }
      log.info(s"Connection closed to $connectionId, reason: " + reason)
      context stop self
  }

  def localInterfaceWriting: Receive = {
    case msg: org.ergoplatform.network.message.Message[_] =>
      log.info("Send message " + msg.spec + " to " + connectionId)
      outMessagesCounter += 1
      connection ! Write(messageSerializer.serialize(msg), ReceivableMessages.Ack(outMessagesCounter))

    case CommandFailed(Write(msg, ReceivableMessages.Ack(id))) =>
      log.warn(s"Failed to write ${msg.length} bytes to $connectionId, switching to buffering mode")
      connection ! ResumeWriting
      buffer(id, msg)
      context become workingCycleBuffering

    case ReceivableMessages.Ack(_) => // ignore ACKs in stable mode

    case WritingResumed => // ignore in stable mode
  }

  // operate in ACK mode until all buffered messages are transmitted
  def localInterfaceBuffering: Receive = {
    case msg: org.ergoplatform.network.message.Message[_] =>
      outMessagesCounter += 1
      buffer(outMessagesCounter, messageSerializer.serialize(msg))

    case CommandFailed(Write(msg, ReceivableMessages.Ack(id))) =>
      connection ! ResumeWriting
      buffer(id, msg)

    case CommandFailed(ResumeWriting) => // ignore in ACK mode

    case WritingResumed =>
      writeFirst()

    case ReceivableMessages.Ack(id) =>
      outMessagesBuffer -= id
      if (outMessagesBuffer.nonEmpty){
        writeFirst()
      } else {
        log.info("Buffered messages processed, exiting buffering mode")
        context become workingCycleWriting
      }
  }

  def remoteInterface: Receive = {
    case Received(data) =>

      chunksBuffer ++= data

      @tailrec
      def process(): Unit = {
        messageSerializer.deserialize(chunksBuffer, selfPeer) match {
          case Success(Some(message)) =>
            log.debug("Received message " + message.spec + " from " + connectionId)
            networkControllerRef ! message
            chunksBuffer = chunksBuffer.drop(message.messageLength)
            process()
          case Success(None) =>
          case Failure(e) =>
            e match {
              //peer is doing bad things, ban it
              case MaliciousBehaviorException(msg) =>
                log.warn(s"Banning peer for malicious behaviour($msg): ${connectionId.toString}")
                //peer will be added to the blacklist and the network controller will send CloseConnection
                networkControllerRef ! PenalizePeer(connectionId.remoteAddress, PenaltyType.PermanentPenalty)
              //non-malicious corruptions
              case _ =>
                log.info(s"Corrupted data from ${connectionId.toString}: ${e.getMessage}")
            }
        }
      }

      process()
      connection ! ResumeReading
  }

  private def reportStrangeInput: Receive = {
    case nonsense =>
      log.warn(s"Strange input for PeerConnectionHandler: $nonsense")
  }

  private def buffer(id: Long, msg: ByteString): Unit = {
    outMessagesBuffer += id -> msg
  }

  private def writeFirst(): Unit = {
    outMessagesBuffer.headOption.foreach { case (id, msg) =>
      connection ! Write(msg, ReceivableMessages.Ack(id))
    }
  }

  // Write into the wire all the buffered messages we have for the peer with no ACK
  private def pushAllWithNoAck(): Unit = {
    outMessagesBuffer.foreach { case (_, msg) =>
      connection ! Write(msg, NoAck)
    }
  }

  private def createHandshakeMessage(): Handshake = {
    Handshake(
      PeerSpec(
        networkSettings.agentName,
        Version(networkSettings.appVersion),
        networkSettings.nodeName,
        ownSocketAddress,
        localFeatures
      ),
      System.currentTimeMillis()
    )
  }

}

object PeerConnectionHandler {

  object ReceivableMessages {
    
    case object HandshakeTimeout

    case object CloseConnection

    final case class Ack(id: Long) extends Tcp.Event

  }

}

object PeerConnectionHandlerRef {

  def props(settings: ScorexSettings,
            networkControllerRef: ActorRef,
            scorexContext: ScorexContext,
            connectionDescription: ConnectionDescription
           )(implicit ec: ExecutionContext): Props =
    Props(new PeerConnectionHandler(settings, networkControllerRef, scorexContext, connectionDescription))

}
