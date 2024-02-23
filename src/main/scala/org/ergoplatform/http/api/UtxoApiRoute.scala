package org.ergoplatform.http.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import org.ergoplatform.ErgoBox
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.state.{ErgoStateReader, UtxoSetSnapshotPersistence, UtxoStateReader}
import org.ergoplatform.settings.RESTApiSettings
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer
import scorex.core.api.http.ApiResponse
import scorex.crypto.authds.ADKey
import scorex.util.encode.Base16

import scala.concurrent.Future

case class UtxoApiRoute(readersHolder: ActorRef, override val settings: RESTApiSettings)(
  implicit val context: ActorRefFactory
) extends ErgoBaseApiRoute
  with ApiCodecs with ApiExtraCodecs {

  private def getState: Future[ErgoStateReader] =
    (readersHolder ? GetReaders).mapTo[Readers].map(_.s)

  private def getStateAndPool: Future[(ErgoStateReader, ErgoMemPoolReader)] =
    (readersHolder ? GetReaders).mapTo[Readers].map(rs => (rs.s, rs.m))

  override val route: Route = pathPrefix("utxo") {
    byId ~ serializedById ~ genesis ~ withPoolById ~ withPoolByIds ~ withPoolSerializedById ~ getBoxesBinaryProof ~ getSnapshotsInfo
  }

  def withPoolById: Route = (get & path("withPool" / "byId" / Segment)) { id =>
    ApiResponse(getStateAndPool.map {
      case (usr: UtxoStateReader, mp) =>
        usr.withMempool(mp).boxById(ADKey @@ Base16.decode(id).get)
      case _ => None
    })
  }

  def withPoolByIds: Route =
    (post & path("withPool" / "byIds") & entity(as[Seq[String]])) { ids =>
      ApiResponse(getStateAndPool.map {
        case (usr: UtxoStateReader, mp) =>
          ids.flatMap(id => usr.withMempool(mp).boxById(ADKey @@ Base16.decode(id).get))
        case _ => Seq.empty
      })
    }

  def withPoolSerializedById: Route = (get & path("withPool" / "byIdBinary" / Segment)) {
    id =>
      ApiResponse(
        getStateAndPool.map {
          case (usr: UtxoStateReader, mp) =>
            usr.withMempool(mp).boxById(ADKey @@ Base16.decode(id).get).map { box =>
              val bytes    = ErgoBoxSerializer.toBytes(box)
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
            val bytes    = ErgoBoxSerializer.toBytes(box)
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

  def getBoxesBinaryProof: Route =
    (post & path("getBoxesBinaryProof") & entity(as[Seq[ErgoBox.BoxId]])) { boxes =>
      ApiResponse(getState.map {
        case usr: UtxoStateReader =>
          Some(Base16.encode(usr.generateBatchProofForBoxes(boxes)))
        case _ => None
      })
    }

  /**
    * Handler for /utxo/getSnapshotsInfo API call which is providing list of
    * UTXO set snapshots stored locally
    */
  def getSnapshotsInfo: Route = (get & path("getSnapshotsInfo")) {
    ApiResponse(getState.map {
      case usr: UtxoSetSnapshotPersistence =>
        Some(usr.getSnapshotInfo())
      case _ => None
    })
  }

}
