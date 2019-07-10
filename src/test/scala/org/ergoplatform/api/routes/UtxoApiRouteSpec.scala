package org.ergoplatform.api.routes

import akka.actor.{Actor, ActorRef, Props}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import org.ergoplatform.api.UtxoApiRoute
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetDataFromHistory, GetReaders, Readers}
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.utils.Stubs
import org.scalatest.{FlatSpec, Matchers}
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base16

class UtxoApiRouteSpec extends FlatSpec
  with Matchers
  with ScalatestRouteTest
  with Stubs
  with FailFastCirceSupport {

  val prefix = "/utxo"

  val utxoSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(stateType = StateType.Utxo))

  lazy val utxoState = boxesHolderGen.map(WrappedUtxoState(_, createTempDir, None, utxoSettings)).sample.value

  lazy val utxoReaders = Readers(history, utxoState, memPool, wallet)

  class UtxoReadersStub extends Actor {
    def receive: PartialFunction[Any, Unit] = {
      case GetReaders => sender() ! utxoReaders
      case GetDataFromHistory(f) => sender() ! f(history)
    }
  }

  object UtxoReadersStub {
    def props(): Props = Props(new UtxoReadersStub)
  }

  lazy val utxoReadersRef: ActorRef = system.actorOf(UtxoReadersStub.props())

  val route: Route = UtxoApiRoute(utxoReadersRef, utxoSettings.scorexSettings.restApi).route


  it should "get utxo box with /byId" in {
    val boxId = Base16.encode(utxoState.takeBoxes(1).head.id)
    Get(prefix + s"/byId/$boxId") ~> route ~> check {
      status shouldBe StatusCodes.OK
    }
  }

  it should "not found utxo box with /byId" in {
    val boxId = Base16.encode(Blake2b256(utxoState.takeBoxes(1).head.id))
    Get(prefix + s"/byId/$boxId") ~> route ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

}
