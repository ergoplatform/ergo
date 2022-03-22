package scorex.testkit.generators

import java.net.{InetAddress, InetSocketAddress}
import akka.actor.ActorRef
import akka.util.ByteString
import org.scalacheck.Gen.{const, some}
import org.scalacheck.{Arbitrary, Gen}
import scorex.core.app.Version
import scorex.core.network.message.{InvData, ModifiersData}
import scorex.core.network._
import scorex.core.network.peer.PeerInfo
import scorex.core.serialization.ScorexSerializer
import scorex.core.{ModifierTypeId, NodeViewModifier}
import scorex.util.serialization._
import scorex.util.{ModifierId, bytesToId}

trait ObjectGenerators {

  object FullNodePeerFeature extends PeerFeature {
    override type M = PeerFeature
    override val featureId: PeerFeature.Id = 1: Byte

    override def serializer: ScorexSerializer[PeerFeature] = new ScorexSerializer[PeerFeature] {

      override def serialize(obj: PeerFeature, w: Writer): Unit = {
        w.put(1)
        w.put(2)
        w.put(3)
      }

      override def parse(r: Reader): PeerFeature = {
        require(r.getByte() == 1 && r.getByte() == 2 && r.getByte() == 3)
        FullNodePeerFeature
      }
    }
  }

  val MaxVersion = 999
  val MaxIp = 255
  val MaxPort = 65535

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

  lazy val modifierTypeIdGen: Gen[ModifierTypeId] = Arbitrary.arbitrary[Byte].map(t => ModifierTypeId @@ t)

  lazy val invDataGen: Gen[InvData] = for {
    modifierTypeId: ModifierTypeId <- modifierTypeIdGen
    modifierIds: Seq[ModifierId] <- Gen.nonEmptyListOf(modifierIdGen) if modifierIds.nonEmpty
  } yield InvData(modifierTypeId, modifierIds)

  lazy val modifierWithIdGen: Gen[(ModifierId, Array[Byte])] = for {
    id <- modifierIdGen
    mod <- nonEmptyBytesGen
  } yield id -> mod

  lazy val modifiersGen: Gen[ModifiersData] = for {
    modifierTypeId: ModifierTypeId <- modifierTypeIdGen
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

  lazy val connectionIdGen: Gen[ConnectionId] = for {
    ip1 <- inetSocketAddressGen
    ip2 <- inetSocketAddressGen
    direction <- Gen.oneOf[ConnectionDirection](Seq[ConnectionDirection](Incoming, Outgoing))
  } yield ConnectionId(ip1, ip2, direction)

  def peerInfoGen: Gen[PeerInfo] = for {
    peerSpec <- peerSpecGen
  } yield PeerInfo(peerSpec, 0L, Some(Incoming))

  def connectedPeerGen(peerRef: ActorRef): Gen[ConnectedPeer] = for {
    connectionId <- connectionIdGen
    peerInfo <- peerInfoGen
  } yield ConnectedPeer(connectionId, peerRef, 0, Some(peerInfo))

  def peerSpecGen: Gen[PeerSpec] = for {
    declaredAddress <- Gen.frequency(5 -> const(None), 5 -> some(inetSocketAddressGen))
    restApiAddress <- Gen.frequency(5 -> const(None), 5 -> some(inetSocketAddressGen))
    features <- Gen.frequency(5 -> const(None), 5 -> some(Gen.oneOf(Seq(FullNodePeerFeature))))
    version <- appVersionGen
  } yield PeerSpec("ergoref", version, "ergo-node", declaredAddress, features.toSeq, restApiAddress)
}
