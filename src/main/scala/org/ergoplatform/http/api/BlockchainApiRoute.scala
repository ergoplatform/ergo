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

case class BlockchainApiRoute(readersHolder: ActorRef, ergoSettings: ErgoSettings, extraIndexerOpt: Option[ActorRef])
                        (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ApiCodecs {

  val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  val paging: Directive[(Long, Long)] = parameters("offset".as[Long] ? 0L, "limit".as[Long] ? 10L)

  private val MaxItems = 16384

  val addressEncoder: ErgoAddressEncoder = ergoSettings.chainSettings.addressEncoder


  override val route: Route = pathPrefix("blockchain") {
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
      getBoxesByErgoTreeUnspentR ~
      getTokenInfoByIdR
  }

  private def getHistory: Future[ErgoHistoryReader] =
    (readersHolder ? GetDataFromHistory[ErgoHistoryReader](r => r)).mapTo[ErgoHistoryReader]

  private def getAddress(tree: ErgoTree)(implicit history: ErgoHistoryReader): Option[IndexedErgoAddress] = {
    history.typedModifierById[IndexedErgoAddress](bytesToId(IndexedErgoAddressSerializer.hashErgoTree(tree)))
  }

  private def getAddress(addr: ErgoAddress)(implicit history: ErgoHistoryReader): Option[IndexedErgoAddress] = getAddress(addr.script)

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
    ApiResponse(getTxByIdF(id))
  }

  private def getTxByIndex(index: Long): Future[Option[IndexedErgoTransaction]] =
    getHistory.map { history =>
      getTxById(history.typedModifierById[NumericTxIndex](bytesToId(NumericTxIndex.indexToBytes(index))).get.m)(history)
    }

  private def getTxByIndexR: Route = (pathPrefix("transaction" / "byIndex" / LongNumber) & get) { index =>
    ApiResponse(getTxByIndex(index))
  }

  private def getTxsByAddress(addr: ErgoAddress, offset: Long, limit: Long): Future[Option[Seq[IndexedErgoTransaction]]] =
    getHistory.map { history =>
      getAddress(addr)(history) match {
        case Some(addr) => Some(addr.retrieveTxs(history, offset, limit))
        case None       => None
      }
    }

  private def getTxsByAddressR: Route = (get & pathPrefix("transaction" / "byAddress") & path(Segment) & paging) { (address, offset, limit) =>
    if(limit > MaxItems) {
      BadRequest(s"No more than $MaxItems transactions can be requested")
    }else {
      addressEncoder.fromString(address) match {
        case Success(addr) => ApiResponse(getTxsByAddress(addr, offset, limit))
        case Failure(_) => BadRequest("Incorrect address format")
      }
    }
  }

  private def getTxRange(offset: Long, limit: Long): Future[Seq[ModifierId]] =
    getHistory.map { history =>
      val base: Int = (history.fullBlockHeight - offset).toInt
      for(n <- (base - limit) to base) yield history.typedModifierById[NumericTxIndex](bytesToId(NumericTxIndex.indexToBytes(n))).get.m
    }

  private def getTxRangeR: Route = (pathPrefix("transaction" / "range") & paging) { (offset, limit) =>
    if(limit > MaxItems) {
      BadRequest(s"No more than $MaxItems transactions can be requested")
    }else {
      ApiResponse(getTxRange(offset, limit))
    }
  }

  private def getBoxById(id: ModifierId)(implicit history: ErgoHistoryReader): Option[IndexedErgoBox] =
    history.typedModifierById[IndexedErgoBox](id)

  private def getBoxByIdF(id: ModifierId): Future[Option[IndexedErgoBox]] =
    getHistory.map { history =>
      getBoxById(id)(history)
    }

  private def getBoxByIdR: Route = (get & pathPrefix("box" / "byId") & modifierId) { id =>
    ApiResponse(getBoxByIdF(id))
  }

  private def getBoxByIndex(index: Long): Future[Option[IndexedErgoBox]] =
    getHistory.map { history =>
      getBoxById(history.typedModifierById[NumericBoxIndex](bytesToId(NumericBoxIndex.indexToBytes(index))).get.m)(history)
    }

  private def getBoxByIndexR: Route = (pathPrefix("box" / "byIndex" / LongNumber) & get) { index =>
    ApiResponse(getBoxByIndex(index))
  }

  private def getBoxesByAddress(addr: ErgoAddress, offset: Long, limit: Long): Future[Seq[IndexedErgoBox]] =
    getHistory.map { history =>
      getAddress(addr)(history) match {
        case Some(addr) => addr.retrieveBoxes(history, offset, limit)
        case None       => Seq.empty[IndexedErgoBox]
      }
    }

  private def getBoxesByAddressR: Route = (get & pathPrefix("box" / "byAddress") & path(Segment) & paging) { (address, offset, limit) =>
    if(limit > MaxItems) {
      BadRequest(s"No more than $MaxItems boxes can be requested")
    }else {
      addressEncoder.fromString(address) match {
        case Success(addr) => ApiResponse(getBoxesByAddress(addr, offset, limit))
        case Failure(_)    => BadRequest("Incorrect address format")
      }
    }
  }

  private def getBoxesByAddressUnspent(addr: ErgoAddress, offset: Long, limit: Long): Future[Seq[IndexedErgoBox]] =
    getHistory.map { history =>
      getAddress(addr)(history) match {
        case Some(addr) => addr.retrieveUtxos(history, offset, limit)
        case None       => Seq.empty[IndexedErgoBox]
      }
    }

  private def getBoxesByAddressUnspentR: Route = (get & pathPrefix("box" / "unspent" / "byAddress") & path(Segment) & paging) { (address, offset, limit) =>
    if(limit > MaxItems) {
      BadRequest(s"No more than $MaxItems boxes can be requested")
    }else {
      addressEncoder.fromString(address) match {
        case Success(addr) => ApiResponse(getBoxesByAddressUnspent(addr, offset, limit))
        case Failure(_) => BadRequest("Incorrect address format")
      }
    }
  }

  private def getBoxRange(offset: Long, limit: Long): Future[Seq[ModifierId]] =
    getHistory.map { history =>
      val base: Int = (history.fullBlockHeight - offset).toInt
      for(n <- (base - limit) to base) yield history.typedModifierById[NumericBoxIndex](bytesToId(NumericBoxIndex.indexToBytes(n))).get.m
    }

  private def getBoxRangeR: Route = (pathPrefix("box" / "range") & paging) { (offset, limit) =>
    if(limit > MaxItems) {
      BadRequest(s"No more than $MaxItems boxes can be requested")
    }else {
      ApiResponse(getBoxRange(offset, limit))
    }
  }

  private def getBoxesByErgoTree(tree: ErgoTree, offset: Long, limit: Long): Future[Seq[IndexedErgoBox]] =
    getHistory.map { history =>
      getAddress(tree)(history) match {
        case Some(iEa) => iEa.retrieveBoxes(history, offset, limit)
        case None      => Seq.empty[IndexedErgoBox]
      }
    }

  private def getBoxesByErgoTreeR: Route = (get & pathPrefix("box" / "byErgoTree") & path(Segment) & paging) { (tree, offset, limit) =>
    try {
      if(limit > MaxItems) {
        BadRequest(s"No more than $MaxItems boxes can be requested")
      }else {
        ApiResponse(getBoxesByErgoTree(ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(Base16.decode(tree).get), offset, limit))
      }
    }catch {
      case e: Exception => BadRequest(s"${e.getMessage}")
    }
  }

  private def getBoxesByErgoTreeUnspent(tree: ErgoTree, offset: Long, limit: Long): Future[Seq[IndexedErgoBox]] =
    getHistory.map { history =>
      getAddress(tree)(history) match {
        case Some(iEa) => iEa.retrieveUtxos(history, offset, limit)
        case None      => Seq.empty[IndexedErgoBox]
      }
    }

  private def getBoxesByErgoTreeUnspentR: Route = (get & pathPrefix("box" / "unspent" / "byErgoTree") & path(Segment) & paging) { (tree, offset, limit) =>
    try {
      if(limit > MaxItems) {
        BadRequest(s"No more than $MaxItems boxes can be requested")
      }else {
        ApiResponse(getBoxesByErgoTreeUnspent(ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(Base16.decode(tree).get), offset, limit))
      }
    }catch {
      case e: Exception => BadRequest(s"${e.getMessage}")
    }
  }

  private def getTokenInfoById(id: ModifierId): Future[Option[IndexedToken]] = {
    getHistory.map { history =>
      history.typedModifierById[IndexedToken](id)
    }
  }

  private def getTokenInfoByIdR: Route = (get & pathPrefix("token" / "byId") & modifierId) { id =>
    ApiResponse(getTokenInfoById(id))
  }

}
