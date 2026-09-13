package org.ergoplatform.network

import java.net.InetSocketAddress
import akka.actor.ActorRef
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.network.message.{
  GetNipopowProofSpec,
  GetSnapshotsInfoSpec,
  InvData,
  InvSpec,
  Message,
  MessageSpec,
  NipopowProofData,
  NipopowProofSpec
}
import org.ergoplatform.utils.ErgoCorePropertyTest
import scorex.core.network.{ConnectedPeer, ConnectionId, Incoming, Synchronizer}
import scorex.util.ModifierId
import scorex.util.encode.Base16

class MessageSpecSpecification extends ErgoCorePropertyTest {

  private class TestSynchronizer extends Synchronizer {
    var handled: Boolean = false
    var penalized: Boolean = false

    def handle(spec: MessageSpec[Any], bytes: Array[Byte], source: ConnectedPeer) =
      parseAndHandle({ case _ => handled = true }, spec, bytes, source)

    override protected def penalizeMaliciousPeer(peer: ConnectedPeer): Unit = penalized = true
  }

  private val peer = ConnectedPeer(
    ConnectionId(
      new InetSocketAddress("127.0.0.1", 9030),
      new InetSocketAddress("127.0.0.1", 9031),
      Incoming
    ),
    ActorRef.noSender,
    None
  )

  property("framed message parsers reject trailing payload bytes") {
    val headerId = Array.fill(16)(1: Byte) ++ Array.fill(16)(2: Byte)
    val headerIdEncoded = ModifierId @@ Base16.encode(headerId)
    val payload = InvSpec.toBytes(InvData(Header.modifierTypeId, Seq(headerIdEncoded)))

    Message(InvSpec, Left(payload ++ Array(0: Byte)), None).data.isFailure shouldBe true
  }

  property("the inbound synchronizer rejects and penalizes trailing payload bytes") {
    val headerId = Array.fill(16)(1: Byte) ++ Array.fill(16)(2: Byte)
    val headerIdEncoded = ModifierId @@ Base16.encode(headerId)
    val payload = InvSpec.toBytes(InvData(Header.modifierTypeId, Seq(headerIdEncoded)))

    val synchronizer = new TestSynchronizer
    synchronizer
      .handle(InvSpec.asInstanceOf[MessageSpec[Any]], payload ++ Array(0: Byte), peer)
      .isFailure shouldBe true
    synchronizer.handled shouldBe false
    synchronizer.penalized shouldBe true
  }

  property("the inbound synchronizer preserves valid framed payload extensions") {
    val headerId = Array.fill(16)(1: Byte) ++ Array.fill(16)(2: Byte)
    val headerIdEncoded = ModifierId @@ Base16.encode(headerId)
    val invPayload = InvSpec.toBytes(InvData(Header.modifierTypeId, Seq(headerIdEncoded)))
    val invSynchronizer = new TestSynchronizer

    invSynchronizer
      .handle(InvSpec.asInstanceOf[MessageSpec[Any]], invPayload, peer)
      .isSuccess shouldBe true
    invSynchronizer.handled shouldBe true
    invSynchronizer.penalized shouldBe false

    val snapshotsSynchronizer = new TestSynchronizer
    snapshotsSynchronizer
      .handle(GetSnapshotsInfoSpec.asInstanceOf[MessageSpec[Any]], Array.fill[Byte](99)(1), peer)
      .isSuccess shouldBe true
    snapshotsSynchronizer.handled shouldBe true
    snapshotsSynchronizer.penalized shouldBe false

    val nipopowPayload = GetNipopowProofSpec
      .toBytes(NipopowProofData(m = 1, k = 1, headerId = None))
      .dropRight(1) ++ Array[Byte](3, 1, 2, 3)
    val nipopowSynchronizer = new TestSynchronizer
    nipopowSynchronizer
      .handle(GetNipopowProofSpec.asInstanceOf[MessageSpec[Any]], nipopowPayload, peer)
      .isSuccess shouldBe true
    nipopowSynchronizer.handled shouldBe true
    nipopowSynchronizer.penalized shouldBe false
  }

  property("extensible empty request payloads preserve bounded extension bytes") {
    Message(GetSnapshotsInfoSpec, Left(Array.fill[Byte](99)(1)), None).data.isSuccess shouldBe true
    Message(GetSnapshotsInfoSpec, Left(Array.fill[Byte](100)(1)), None).data.isFailure shouldBe true
  }

  property("nipopow request payloads accept declared extensions but reject malformed tails") {
    val spec = GetNipopowProofSpec
    val payload = spec.toBytes(NipopowProofData(m = 1, k = 1, headerId = None))
    val withDeclaredExtension = payload.dropRight(1) ++ Array[Byte](3, 1, 2, 3)

    Message(spec, Left(withDeclaredExtension), None).data.isSuccess shouldBe true
    Message(spec, Left(withDeclaredExtension ++ Array(4: Byte)), None).data.isFailure shouldBe true
    Message(spec, Left(payload.dropRight(1) ++ Array[Byte](2, 1)), None).data.isFailure shouldBe true
  }

  property("nipopow proof payloads accept declared extensions but reject malformed tails") {
    val spec = NipopowProofSpec
    val payload = spec.toBytes(Array(1: Byte))
    val withDeclaredExtension = payload.dropRight(1) ++ Array[Byte](3, 1, 2, 3)

    Message(spec, Left(withDeclaredExtension), None).data.isSuccess shouldBe true
    Message(spec, Left(withDeclaredExtension ++ Array(4: Byte)), None).data.isFailure shouldBe true
    Message(spec, Left(payload.dropRight(1) ++ Array[Byte](2, 1)), None).data.isFailure shouldBe true
  }
}
