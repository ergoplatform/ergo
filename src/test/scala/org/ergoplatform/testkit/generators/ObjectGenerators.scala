package org.ergoplatform.testkit.generators

import java.net.{InetAddress, InetSocketAddress, URL}
import akka.actor.ActorRef
import akka.util.ByteString
import org.ergoplatform.NodeViewModifier
import org.ergoplatform.modifiers.NetworkObjectTypeId
import org.ergoplatform.network.{ModePeerFeature, PeerFeature, PeerSpec, Version}
import org.ergoplatform.nodeView.state.StateType
import org.scalacheck.Gen.{const, some}
import org.scalacheck.{Arbitrary, Gen}
import org.ergoplatform.network.message.{InvData, ModifiersData}
import org.ergoplatform.network.peer.{PeerInfo, RestApiUrlPeerFeature}
import scorex.core.network.{ConnectedPeer, ConnectionDirection, ConnectionId, Incoming, Outgoing}
import scorex.util.{ModifierId, bytesToId}

trait ObjectGenerators {

  val MaxVersion = 999
  val MaxIp = 255
  val MaxPort = 65535

  val modePeerFeatureGen = for {
    utxo <- Gen.oneOf(true, false)
    stateType <- if(utxo) StateType.Utxo else StateType.Digest
    verifyingTransactions <- Gen.oneOf(true, false)
    popowSuffix <- Gen.option(smallInt)
    blocksToKeep <- smallInt
  } yield ModePeerFeature(stateType, verifyingTransactions, popowSuffix, blocksToKeep)

  lazy val smallInt: Gen[Int] = Gen.choose(0, 20)

  lazy val nonEmptyBytesGen: Gen[Array[Byte]] = Gen.nonEmptyListOf(Arbitrary.arbitrary[Byte])
    .map(_.toArray).suchThat(_.length > 0)

  lazy val nonEmptyByteStringGen: Gen[ByteString] = nonEmptyBytesGen.map(ByteString(_))

  def genBoundedBytes(minSize: Int, maxSize: Int): Gen[Array[Byte]] = {
    Gen.choose(minSize, maxSize) flatMap { sz => Gen.listOfN(sz, Arbitrary.arbitrary[Byte]).map(_.toArray) }
  }

  def genBytes(size: Int): Gen[Array[Byte]] = genBoundedBytes(size, size)

  lazy val positiveLongGen: Gen[Long] = Gen.choose(1, Long.MaxValue)

  lazy val positiveByteGen: Gen[Byte] = Gen.choose(1, Byte.MaxValue)


  lazy val modifierIdGen: Gen[ModifierId] = Gen.listOfN(NodeViewModifier.ModifierIdSize, Arbitrary.arbitrary[Byte])
    .map(id => bytesToId(id.toArray))

  lazy val modifierTypeIdGen: Gen[NetworkObjectTypeId.Value] = Arbitrary.arbitrary[Byte].map(t => NetworkObjectTypeId.fromByte(t))

  lazy val invDataGen: Gen[InvData] = for {
    modifierTypeId: NetworkObjectTypeId.Value <- modifierTypeIdGen
    modifierIds: Seq[ModifierId] <- Gen.nonEmptyListOf(modifierIdGen) if modifierIds.nonEmpty
  } yield InvData(modifierTypeId, modifierIds)

  lazy val modifierWithIdGen: Gen[(ModifierId, Array[Byte])] = for {
    id <- modifierIdGen
    mod <- nonEmptyBytesGen
  } yield id -> mod

  lazy val modifiersGen: Gen[ModifiersData] = for {
    modifierTypeId: NetworkObjectTypeId.Value <- modifierTypeIdGen
    modifiers: Map[ModifierId, Array[Byte]] <- Gen.nonEmptyMap(modifierWithIdGen).suchThat(_.nonEmpty)
  } yield ModifiersData(modifierTypeId, modifiers)

  lazy val appVersionGen: Gen[Version] = for {
    fd <- Gen.choose(0: Byte, Byte.MaxValue)
    sd <- Gen.choose(0: Byte, Byte.MaxValue)
    td <- Gen.choose(0: Byte, Byte.MaxValue)
  } yield Version(fd, sd, td)

  lazy val inetSocketAddressGen: Gen[InetSocketAddress] = for {
    ip1 <- Gen.choose(0, MaxIp)
    ip2 <- Gen.choose(0, MaxIp)
    ip3 <- Gen.choose(0, MaxIp)
    ip4 <- Gen.choose(0, MaxIp)
    port <- Gen.choose(0, MaxPort)
  } yield new InetSocketAddress(InetAddress.getByName(s"$ip1.$ip2.$ip3.$ip4"), port)

  lazy val urlGen: Gen[URL] = for {
    protocol <- Gen.frequency(5 -> const("http://"), 5 -> const("https://"))
    ip1 <- Gen.choose(0, MaxIp)
    ip2 <- Gen.choose(0, MaxIp)
    ip3 <- Gen.choose(0, MaxIp)
    ip4 <- Gen.choose(0, MaxIp)
    host <- Gen.frequency(5 -> const(s"$ip1.$ip2.$ip3.$ip4"), 5 -> const("example.com"))
    port <- Gen.choose(0, MaxPort)
    suffix <- Gen.frequency(5 -> const("/"), 5 -> const(""))
  } yield new URL(s"$protocol$host:$port$suffix")

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

  def peerSpecGen: Gen[PeerSpec] = for {
    declaredAddress <- Gen.frequency(5 -> const(None), 5 -> some(inetSocketAddressGen))
    features: Seq[PeerFeature] <- Gen.someOf(modePeerFeatureGen, urlGen.flatMap(url => RestApiUrlPeerFeature(url)))
    version <- appVersionGen
  } yield PeerSpec("ergoref", version, "ergo-node", declaredAddress, features)
}
