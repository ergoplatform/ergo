package org.ergoplatform.http.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.{Directive1, Route, ValidationRejection}
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
import scala.util.{Failure, Success}

case class UtxoApiRoute(readersHolder: ActorRef, override val settings: RESTApiSettings)(
  implicit val context: ActorRefFactory
) extends ErgoBaseApiRoute
  with ApiCodecs with ApiExtraCodecs {

  private def getState: Future[ErgoStateReader] =
    (readersHolder ? GetReaders).mapTo[Readers].map(_.s)

  private def getStateAndPool: Future[(ErgoStateReader, ErgoMemPoolReader)] =
    (readersHolder ? GetReaders).mapTo[Readers].map(rs => (rs.s, rs.m))

  private def boxId(value: String): Directive1[ErgoBox.BoxId] =
    Base16.decode(value) match {
      case Success(bytes) => provide(ADKey @@ bytes)
      case Failure(_)     => reject(ValidationRejection(s"boxId $value is invalid, it should be hex string"))
    }

  private def boxIds(values: Seq[String]): Directive1[Seq[ErgoBox.BoxId]] = {
    val parsed = values.map(value => value -> Base16.decode(value))
    parsed.collectFirst { case (value, Failure(_)) => value } match {
      case Some(value) => reject(ValidationRejection(s"boxId $value is invalid, it should be hex string"))
      case None        => provide(parsed.collect { case (_, Success(bytes)) => ADKey @@ bytes })
    }
  }

  override val route: Route = pathPrefix("utxo") {
    byId ~ serializedById ~ genesis ~ withPoolById ~ withPoolByIds ~ withPoolSerializedById ~ getBoxesBinaryProof ~ getSnapshotsInfo
  }

  def withPoolById: Route = (get & path("withPool" / "byId" / Segment)) { id =>
    boxId(id) { boxId =>
      ApiResponse(getStateAndPool.map {
        case (usr: UtxoStateReader, mp) =>
          usr.withMempool(mp).boxById(boxId)
        case _ => None
      })
    }
  }

  def withPoolByIds: Route =
    (post & path("withPool" / "byIds") & entity(as[Seq[String]])) { ids =>
      boxIds(ids) { boxIds =>
        ApiResponse(getStateAndPool.map {
          case (usr: UtxoStateReader, mp) =>
            boxIds.flatMap(id => usr.withMempool(mp).boxById(id))
          case _ => Seq.empty
        })
      }
    }

  def withPoolSerializedById: Route = (get & path("withPool" / "byIdBinary" / Segment)) {
    id =>
      boxId(id) { boxId =>
        ApiResponse(
          getStateAndPool.map {
            case (usr: UtxoStateReader, mp) =>
              usr.withMempool(mp).boxById(boxId).map { box =>
                val bytes    = ErgoBoxSerializer.toBytes(box)
                val boxBytes = Base16.encode(bytes)
                Map("boxId" -> id, "bytes" -> boxBytes)
              }
            case _ => None
          }
        )
      }
  }

  def byId: Route = (get & path("byId" / Segment)) { id =>
    boxId(id) { boxId =>
      ApiResponse(getState.map {
        case usr: UtxoStateReader =>
          usr.boxById(boxId)
        case _ => None
      })
    }
  }

  def serializedById: Route = (get & path("byIdBinary" / Segment)) { id =>
    boxId(id) { boxId =>
      ApiResponse(
        getState.map {
          case usr: UtxoStateReader =>
            usr.boxById(boxId).map { box =>
              val bytes    = ErgoBoxSerializer.toBytes(box)
              val boxBytes = Base16.encode(bytes)
              Map("boxId" -> id, "bytes" -> boxBytes)
            }
          case _ => None
        }
      )
    }
  }

  def genesis: Route = (get & path("genesis")) {
    ApiResponse(getState.map(_.genesisBoxes))
  }

  def getBoxesBinaryProof: Route =
    (post & path("getBoxesBinaryProof") & withAuth & entity(as[Seq[ErgoBox.BoxId]])) { boxes =>
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
