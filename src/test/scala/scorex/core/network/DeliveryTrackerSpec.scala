package scorex.core.network

import akka.actor.{ActorRef, Cancellable}
import io.circe._
import io.circe.syntax._
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.core.ModifierTypeId
import scorex.core.network.ModifiersStatus.Received
import scorex.testkit.generators.ObjectGenerators
import scorex.util.ModifierId

class DeliveryTrackerSpec extends ErgoPropertyTest with ObjectGenerators {

  property("tracker should accept requested modifiers, turn them into received and clear them") {
    forAll(connectedPeerGen(ActorRef.noSender)) { peer =>
      val tracker = DeliveryTracker.empty(settings)
      val mid: ModifierId = ModifierId @@ "foo"
      val mTypeId: ModifierTypeId = ModifierTypeId @@ (104: Byte)
      tracker.setRequested(Seq(mid), mTypeId, Some(peer)) { _ => Cancellable.alreadyCancelled}
      tracker.fullInfo.asJson shouldBe Json.obj(
        "invalidModifierApproxSize" -> 0.asJson,
        "requested" -> Json.obj(
          "104" -> Json.obj(
            "foo" -> Json.obj(
              "address" -> peer.connectionId.remoteAddress.toString.asJson,
              "checks" -> 0.asJson
            )
          )
        ),
        "received" -> Json.obj()
      )

      tracker.setReceived(mid, mTypeId, peer)
      tracker.fullInfo.asJson shouldBe Json.obj(
        "invalidModifierApproxSize" -> 0.asJson,
        "requested" -> Json.obj(
          "104" -> Json.obj(),
        ),
        "received" -> Json.obj(
          "104" -> Json.obj(
            "foo" -> Json.obj(
              "address" -> peer.connectionId.remoteAddress.toString.asJson
            )
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

}
