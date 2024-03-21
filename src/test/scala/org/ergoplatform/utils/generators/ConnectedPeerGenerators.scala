package org.ergoplatform.utils.generators

import akka.actor.ActorRef
import akka.util.ByteString
import org.scalacheck.Gen
import scorex.core.network._
import org.ergoplatform.network.peer.PeerInfo

object ConnectedPeerGenerators {
  import org.ergoplatform.utils.generators.CoreObjectGenerators._

  lazy val nonEmptyByteStringGen: Gen[ByteString] = nonEmptyBytesGen.map(ByteString(_))

  lazy val connectionIdGen: Gen[ConnectionId] = for {
    ip1 <- inetSocketAddressGen
    ip2 <- inetSocketAddressGen
    direction <- Gen.oneOf[ConnectionDirection](Seq[ConnectionDirection](Incoming, Outgoing))
  } yield ConnectionId(ip1, ip2, direction)

  def peerInfoGen: Gen[PeerInfo] = for {
    peerSpec <- peerSpecGen
  } yield PeerInfo(peerSpec, 0L, Some(Incoming), 0L)

  def connectedPeerGen(peerRef: ActorRef): Gen[ConnectedPeer] = for {
    connectionId <- connectionIdGen
    peerInfo <- peerInfoGen
  } yield ConnectedPeer(connectionId, peerRef, Some(peerInfo))
}
