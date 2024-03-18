package scorex.core.network

import akka.actor.{ActorRef, Cancellable}
import io.circe._
import io.circe.syntax._
import org.ergoplatform.modifiers.NetworkObjectTypeId
import org.ergoplatform.utils.ErgoCorePropertyTest
import scorex.core.network.ModifiersStatus.Received
import scorex.util.ModifierId

class DeliveryTrackerSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.generators.ConnectedPeerGenerators._
  import org.ergoplatform.utils.ErgoNodeTestConstants._

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

}
