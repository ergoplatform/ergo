package org.ergoplatform.http.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.{Directive1, Route, ValidationRejection}
import akka.pattern.ask
import org.ergoplatform.ErgoBox
import org.ergoplatform.http.api.ApiError.BadRequest
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.state.{ErgoStateReader, UtxoSetSnapshotPersistence, UtxoStateReader}
import org.ergoplatform.settings.Constants.HashLength
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

  private val MaxBatchItems = 16384

  private def validateMaxItems(count: Int, itemName: String)(inner: => Route): Route =
    if (count > MaxBatchItems) {
      BadRequest(s"No more than $MaxBatchItems $itemName can be requested")
    } else {
      inner
    }

  private def parseBoxId(value: String): Either[String, ErgoBox.BoxId] =
    Base16.decode(value) match {
      case Success(bytes) if bytes.length == HashLength =>
        Right(ADKey @@ bytes)
      case Success(_) =>
        Left(s"boxId is invalid, it should be $HashLength-byte hex string")
      case Failure(_) =>
        Left("boxId is invalid, it should be hex string")
    }

  private def boxId(value: String): Directive1[ErgoBox.BoxId] =
    parseBoxId(value) match {
      case Right(boxId) => provide(boxId)
      case Left(error)  => reject(ValidationRejection(error))
    }

  private def boxIds(values: Seq[String]): Directive1[Seq[ErgoBox.BoxId]] = {
    val parsed = values.map(value => value -> parseBoxId(value))
    parsed.collectFirst { case (_, Left(error)) => error } match {
      case Some(error) => reject(ValidationRejection(error))
      case None        => provide(parsed.collect { case (_, Right(boxId)) => boxId })
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
      validateMaxItems(ids.size, "box ids") {
        boxIds(ids) { boxIds =>
          ApiResponse(getStateAndPool.map {
            case (usr: UtxoStateReader, mp) =>
              boxIds.flatMap(id => usr.withMempool(mp).boxById(id))
            case _ => Seq.empty
          })
        }
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
    (post & path("getBoxesBinaryProof") & entity(as[Seq[ErgoBox.BoxId]])) { boxes =>
      validateMaxItems(boxes.size, "box ids") {
        ApiResponse(getState.map {
          case usr: UtxoStateReader =>
            Some(Base16.encode(usr.generateBatchProofForBoxes(boxes)))
          case _ => None
        })
      }
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
