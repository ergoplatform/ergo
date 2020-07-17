package org.ergoplatform.http.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.state.{ErgoStateReader, UtxoStateReader}
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings
import scorex.crypto.authds.ADKey
import scorex.util.encode.Base16

import scala.concurrent.Future

case class UtxoApiRoute(readersHolder: ActorRef, override val settings: RESTApiSettings)
                       (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ApiCodecs {

  private def getState: Future[ErgoStateReader] = (readersHolder ? GetReaders).mapTo[Readers].map(_.s)

  override val route: Route = pathPrefix("utxo") {
    byId ~ serializedbyId ~ genesis
  }

  def byId: Route = (get & path("byId" / Segment)) { id =>
    ApiResponse(getState.map {
      case usr: UtxoStateReader =>
        usr.boxById(ADKey @@ Base16.decode(id).get)
      case _ => None
    })
  }

  def serializedbyId: Route = (get & path("byIdBinary" / Segment)) { id =>
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
