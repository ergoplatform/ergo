package org.ergoplatform.http.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import org.ergoplatform.modifiers.mempool.ErgoBoxSerializer
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.state.{ErgoStateReader, UtxoStateReader}
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings
import scorex.crypto.authds.ADKey
import scorex.util.encode.Base16

import scala.concurrent.Future

case class UtxoApiRoute(readersHolder: ActorRef, override val settings: RESTApiSettings)
                       (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ApiCodecs {

  private def getState: Future[ErgoStateReader] = (readersHolder ? GetReaders).mapTo[Readers].map(_.s)

  private def getStateAndPool: Future[(ErgoStateReader, ErgoMemPoolReader)] =
    (readersHolder ? GetReaders).mapTo[Readers].map(rs => (rs.s, rs.m))

  override val route: Route = pathPrefix("utxo") {
    byId ~ serializedById ~ genesis ~ withPoolById ~ withPoolSerializedById
  }

  def withPoolById: Route = (get & path("withPool" / "byId" / Segment)) { id =>
    ApiResponse(getStateAndPool.map {
      case (usr: UtxoStateReader, mp) =>
        usr.withMempool(mp).boxById(ADKey @@ Base16.decode(id).get)
      case _ => None
    })
  }

  def withPoolSerializedById: Route = (get & path("withPool" / "byIdBinary" / Segment)) { id =>
    ApiResponse(
      getStateAndPool.map {
        case (usr: UtxoStateReader, mp) =>
          usr.withMempool(mp).boxById(ADKey @@ Base16.decode(id).get).map { box =>
            val bytes = ErgoBoxSerializer.toBytes(box)
            val boxBytes = Base16.encode(bytes)
            Map("boxId" -> id, "bytes" -> boxBytes)
          }
        case _ => None
      }
    )
  }

  def byId: Route = (get & path("byId" / Segment)) { id =>
    ApiResponse(getState.map {
      case usr: UtxoStateReader =>
        usr.boxById(ADKey @@ Base16.decode(id).get)
      case _ => None
    })
  }

  def serializedById: Route = (get & path("byIdBinary" / Segment)) { id =>
    ApiResponse(
      getState.map {
        case usr: UtxoStateReader =>
          usr.boxById(ADKey @@ Base16.decode(id).get).map { box =>
            val bytes = ErgoBoxSerializer.toBytes(box)
            val boxBytes = Base16.encode(bytes)
            Map("boxId" -> id, "bytes" -> boxBytes)
          }
        case _ => None
      }
    )
  }

  def genesis: Route = (get & path("genesis")) {
    ApiResponse(getState.map(_.genesisBoxes))
  }
}
