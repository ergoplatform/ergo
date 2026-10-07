package org.ergoplatform.network

import akka.actor.{ActorRef, Cancellable}
import io.circe._
import io.circe.syntax._
import org.ergoplatform.modifiers.NetworkObjectTypeId
import org.ergoplatform.utils.ErgoCorePropertyTest
import scorex.util.{ModifierId, bytesToId}
import scorex.core.network.DeliveryTracker
import scorex.core.network.ModifiersStatus._
import org.ergoplatform.consensus.ContainsModifiers


class DeliveryTrackerSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ConnectedPeerGenerators._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators.defaultHeaderGen

  property("tracker should accept requested modifiers, turn them into received and clear them") {
    forAll(connectedPeerGen(ActorRef.noSender)) { peer =>
      val tracker = DeliveryTracker.empty(settings)
      val mid: ModifierId = ModifierId @@ "foo"
      val mTypeId: NetworkObjectTypeId.Value = NetworkObjectTypeId.fromByte(104)
      tracker.setRequested(mTypeId, mid, peer) { _ => Cancellable.alreadyCancelled}
      val infoFields =
        Seq(
          "address" -> peer.connectionId.remoteAddress.toString.asJson,
          "checks" -> 0.asJson
        ) ++ peer.peerInfo.map(_.peerSpec.protocolVersion.toString.asJson).map("version" -> _)
      tracker.fullInfo.asJson shouldBe Json.obj(
        "invalidModifierApproxSize" -> 0.asJson,
        "requested" -> Json.obj(
          "104" -> Json.obj(
            "foo" -> Json.obj(infoFields:_*)
          )
        ),
        "received" -> Json.obj()
      )

      tracker.setReceived(mid, mTypeId, peer)
      val infoFields2 =
        Seq(
          "address" -> peer.connectionId.remoteAddress.toString.asJson
        ) ++ peer.peerInfo.map(_.peerSpec.protocolVersion.toString.asJson).map("version" -> _)

      tracker.fullInfo.asJson shouldBe Json.obj(
        "invalidModifierApproxSize" -> 0.asJson,
        "requested" -> Json.obj(
          "104" -> Json.obj(),
        ),
        "received" -> Json.obj(
          "104" -> Json.obj(
            "foo" -> Json.obj(infoFields2:_*)
          ),
        )
      )
      tracker.clearStatusForModifier(mid, mTypeId, Received)
      tracker.fullInfo.asJson shouldBe Json.obj(
        "invalidModifierApproxSize" -> 0.asJson,
        "requested" -> Json.obj(
          "104" -> Json.obj(),
        ),
        "received" -> Json.obj(
          "104" -> Json.obj()
        )
      )

      tracker.reset()
      val fullInfoAfterReset = tracker.fullInfo
      fullInfoAfterReset.invalidModifierApproxSize shouldBe 0
      fullInfoAfterReset.requested.size shouldBe 0
      fullInfoAfterReset.received.size shouldBe 0
    }
  }

  property("tracker should return Held status for modifiers in history") {
    import org.ergoplatform.modifiers.history.header.Header
    val tracker = DeliveryTracker.empty(settings)
    val mid: ModifierId = ModifierId @@ "held_modifier"
    val mTypeId: NetworkObjectTypeId.Value = NetworkObjectTypeId.fromByte(104)

    // Create a mock ContainsModifiers that reports the modifier as held
    val mockHeader = defaultHeaderGen.sample.get
    val mockHistory = new ContainsModifiers[Header] {
      override def modifierById(modifierId: ModifierId): Option[Header] =
        if (modifierId == mid) Some(mockHeader) else None
    }

    // Without history, modifier should be Unknown
    tracker.status(mid, mTypeId, Seq.empty) shouldBe Unknown

    // With history that contains the modifier, should be Held
    tracker.status(mid, mTypeId, Seq(mockHistory)) shouldBe Held

    // If modifier is in received cache, it should take precedence over Held
    forAll(connectedPeerGen(ActorRef.noSender)) { peer =>
      tracker.setReceived(mid, mTypeId, peer)
      tracker.status(mid, mTypeId, Seq(mockHistory)) shouldBe Received
    }
  }

  property("tracker should return correct status precedence") {
    forAll(connectedPeerGen(ActorRef.noSender)) { peer =>
      val tracker = DeliveryTracker.empty(settings)
      val mid: ModifierId = bytesToId(scorex.utils.Random.randomBytes(32))
      val mTypeId: NetworkObjectTypeId.Value = NetworkObjectTypeId.fromByte(104)

      // Initially Unknown
      tracker.status(mid, mTypeId, Seq.empty) shouldBe Unknown

      // Set as Requested
      tracker.setRequested(mTypeId, mid, peer) { _ => Cancellable.alreadyCancelled }
      tracker.status(mid, mTypeId, Seq.empty) shouldBe Requested

      // Set as Received - should override Requested
      tracker.setReceived(mid, mTypeId, peer)
      tracker.status(mid, mTypeId, Seq.empty) shouldBe Received

      // Set as Invalid - should override Received
      tracker.setInvalid(mid, mTypeId)
      tracker.status(mid, mTypeId, Seq.empty) shouldBe Invalid
    }
  }

}
