package org.ergoplatform.http.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.{Directive, Route}
import akka.pattern.ask
import org.ergoplatform.{ErgoAddress, ErgoAddressEncoder}
import org.ergoplatform.nodeView.ErgoReadersHolder.GetDataFromHistory
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.history.extra._
import org.ergoplatform.settings.ErgoSettings
import scorex.core.api.http.ApiError.BadRequest
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings
import scorex.util.encode.Base16
import scorex.util.{ModifierId, bytesToId}
import sigmastate.Values.ErgoTree
import sigmastate.serialization.ErgoTreeSerializer

import scala.concurrent.Future
import scala.util.{Failure, Success}

case class ExtraIndexApiRoute(readersHolder: ActorRef, ergoSettings: ErgoSettings, extraIndexerOpt: Option[ActorRef])
                        (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ApiCodecs {

  val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  val paging: Directive[(Long, Long)] = parameters("fromIndex".as[Long] ? 0L, "toIndex".as[Long] ? 10L)

  val lastN: Directive[Tuple1[Long]] = parameter("limit".as[Long] ? 20L)

  val addressEncoder: ErgoAddressEncoder = ergoSettings.chainSettings.addressEncoder

  private val MaxTxs = 16384
  private val MaxBoxes = MaxTxs * 3

  override val route: Route = pathPrefix("extra") {
    getTxByIdR ~
      getTxByIndexR ~
      getTxsByAddressR ~
      getTxRangeR ~
      getBoxByIdR ~
      getBoxByIndexR ~
      getBoxesByAddressR ~
      getBoxesByAddressUnspentR ~
      getBoxRangeR ~
      getBoxesByErgoTreeR ~
      getBoxesByErgoTreeUnspentR
  }

  private def getHistory: Future[ErgoHistoryReader] =
    (readersHolder ? GetDataFromHistory[ErgoHistoryReader](r => r)).mapTo[ErgoHistoryReader]

  private def getAddress(addr: ErgoAddress)(implicit history: ErgoHistoryReader): Option[IndexedErgoAddress] = {
    history.typedModifierById[IndexedErgoAddress](IndexedErgoAddressSerializer.addressToModifierId(addr))
  }

  private def getTxById(id: ModifierId)(implicit history: ErgoHistoryReader): Option[IndexedErgoTransaction] =
    history.typedModifierById[IndexedErgoTransaction](id) match {
      case Some(tx) => Some(tx.retrieveBody(history))
      case None     => None
    }


  private def getTxByIdF(id: ModifierId) : Future[Option[IndexedErgoTransaction]] =
    getHistory.map { history =>
      getTxById(id)(history)
    }

  private def getTxByIdR: Route = (get & pathPrefix("transaction" / "byId") & modifierId) { id =>
    ApiResponse(getTxByIdF(bytesToId(Base16.decode(id).get)))
  }

  private def getTxByIndex(index: Long): Future[Option[IndexedErgoTransaction]] =
    getHistory.map { history =>
      getTxById(history.typedModifierById[NumericTxIndex](bytesToId(NumericTxIndex.indexToBytes(index))).get.m)(history)
    }

  private def getTxByIndexR: Route = (pathPrefix("transaction" / "byIndex" / LongNumber) & get) { index =>
    ApiResponse(getTxByIndex(index))
  }

  private def getTxsByAddress(addr: ErgoAddress, lastN: Long): Future[Option[Seq[IndexedErgoTransaction]]] =
    getHistory.map { history =>
      getAddress(addr)(history) match {
        case Some(addr) => Some(addr.retrieveTxs(history, lastN).transactions(-1))
        case None       => None
      }
    }

  private def getTxsByAddressR: Route = (get & pathPrefix("transaction" / "byAddress") & path(Segment) & lastN) { (address, limit) =>
    addressEncoder.fromString(address) match {
      case Success(addr) => ApiResponse(getTxsByAddress(addr, limit))
      case Failure(_)    => BadRequest("Incorrect address format")
    }
  }

  private def getTxRange(fromHeight: Long, toHeight: Long): Future[Seq[ModifierId]] =
    getHistory.map { history =>
      (for(n <- fromHeight to toHeight)
        yield history.typedModifierById[NumericTxIndex](bytesToId(NumericTxIndex.indexToBytes(n))).get.m
      ).toSeq
    }

  private def getTxRangeR: Route = (pathPrefix("transaction" / "range") & paging) { (fromIndex, toIndex) =>
    if (toIndex < fromIndex) {
      BadRequest("toIndex < fromIndex")
    } else if (fromIndex - toIndex > MaxTxs) {
      BadRequest(s"No more than $MaxTxs transactions can be requested")
    } else {
      ApiResponse(getTxRange(fromIndex, toIndex))
    }
  }

  private def getBoxById(id: ModifierId)(implicit history: ErgoHistoryReader): Option[IndexedErgoBox] =
    history.typedModifierById[IndexedErgoBox](id)

  private def getBoxByIdF(id: ModifierId): Future[Option[IndexedErgoBox]] =
    getHistory.map { history =>
      getBoxById(id)(history)
    }

  private def getBoxByIdR: Route = (get & pathPrefix("box" / "byId") & modifierId) { id =>
    ApiResponse(getBoxByIdF(bytesToId(Base16.decode(id).get)))
  }

  private def getBoxByIndex(index: Long): Future[Option[IndexedErgoBox]] =
    getHistory.map { history =>
      getBoxById(history.typedModifierById[NumericBoxIndex](bytesToId(NumericBoxIndex.indexToBytes(index))).get.m)(history)
    }

  private def getBoxByIndexR: Route = (pathPrefix("box" / "byIndex" / LongNumber) & get) { index =>
    ApiResponse(getBoxByIndex(index))
  }

  private def getBoxesByAddress(addr: ErgoAddress, lastN: Long): Future[Seq[IndexedErgoBox]] =
    getHistory.map { history =>
      getAddress(addr)(history) match {
        case Some(addr) => addr.retrieveBoxes(history, lastN).boxes()
        case None       => Seq.empty[IndexedErgoBox]
      }
    }

  private def getBoxesByAddressR: Route = (get & pathPrefix("box" / "byAddress") & path(Segment) & lastN) { (address, limit) =>
    addressEncoder.fromString(address) match {
      case Success(addr) => ApiResponse(getBoxesByAddress(addr, limit))
      case Failure(_)    => BadRequest("Incorrect address format")
    }
  }

  private def getBoxesByAddressUnspent(addr: ErgoAddress, lastN: Long): Future[Seq[IndexedErgoBox]] =
    getHistory.map { history =>
      getAddress(addr)(history) match {
        case Some(addr) => addr.retrieveBoxes(history, lastN).utxos()
        case None       => Seq.empty[IndexedErgoBox]
      }
    }

  private def getBoxesByAddressUnspentR: Route = (get & pathPrefix("box" / "unspent" / "byAddress") & path(Segment) & lastN) { (address, limit) =>
    addressEncoder.fromString(address) match {
      case Success(addr) => ApiResponse(getBoxesByAddressUnspent(addr, limit))
      case Failure(_)    => BadRequest("Incorrect address format")
    }
  }

  private def getBoxRange(fromHeight: Long, toHeight: Long): Future[Seq[ModifierId]] =
    getHistory.map { history =>
      (for(n <- fromHeight to toHeight)
          yield history.typedModifierById[NumericBoxIndex](bytesToId(NumericBoxIndex.indexToBytes(n))).get.m
      ).toSeq
    }

  private def getBoxRangeR: Route = (pathPrefix("box" / "range") & paging) { (fromIndex, toIndex) =>
    if (toIndex < fromIndex) {
      BadRequest("toIndex < fromIndex")
    } else if (fromIndex - toIndex > MaxBoxes) {
      BadRequest(s"No more than $MaxBoxes boxes can be requested")
    } else {
      ApiResponse(getBoxRange(fromIndex, toIndex))
    }
  }

  private def getBoxesByErgoTree(tree: ErgoTree, lastN: Long): Future[Seq[IndexedErgoBox]] =
    getHistory.map { history =>
      history.typedModifierById[IndexedErgoTree](bytesToId(IndexedErgoTreeSerializer.ergoTreeHash(tree))) match {
        case Some(iEt) => iEt.retrieveBody(history, lastN)
        case None      => Seq.empty[IndexedErgoBox]
      }
    }

  private def getBoxesByErgoTreeR: Route = (get & pathPrefix("box" / "byErgoTree") & path(Segment) & lastN) { (tree, limit) =>
    try {
      ApiResponse(getBoxesByErgoTree(ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(Base16.decode(tree).get), limit))
    }catch {
      case e: Exception => BadRequest(s"${e.getMessage}")
    }
  }

  private def getBoxesByErgoTreeUnspent(tree: ErgoTree, lastN: Long): Future[Seq[IndexedErgoBox]] =
    getHistory.map { history =>
      history.typedModifierById[IndexedErgoTree](bytesToId(IndexedErgoTreeSerializer.ergoTreeHash(tree))) match {
        case Some(iEt) => iEt.retrieveBody(history, lastN).filter(!_.trackedBox.isSpent)
        case None      => Seq.empty[IndexedErgoBox]
      }
    }

  private def getBoxesByErgoTreeUnspentR: Route = (get & pathPrefix("box" / "unspent" / "byErgoTree") & path(Segment) & lastN) { (tree, limit) =>
    try {
      ApiResponse(getBoxesByErgoTreeUnspent(ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(Base16.decode(tree).get), limit))
    }catch {
      case e: Exception => BadRequest(s"${e.getMessage}")
    }
  }

}

