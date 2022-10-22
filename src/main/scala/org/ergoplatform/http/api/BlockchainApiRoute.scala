package org.ergoplatform.http.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.{Directive, Directive1, Route, ValidationRejection}
import akka.pattern.ask
import org.ergoplatform.{ErgoAddress, ErgoAddressEncoder}
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetDataFromHistory, GetReaders, Readers}
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.history.extra.ExtraIndexerRef.{GlobalBoxIndexKey, GlobalTxIndexKey}
import org.ergoplatform.nodeView.history.extra._
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.settings.ErgoSettings
import scorex.core.api.http.ApiError.BadRequest
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings
import scorex.util.{ModifierId, bytesToId}
import sigmastate.Values.ErgoTree
import spire.implicits.cfor

import java.nio.ByteBuffer
import scala.concurrent.Future
import scala.util.Success

case class BlockchainApiRoute(readersHolder: ActorRef, ergoSettings: ErgoSettings)
                        (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ApiCodecs {

  val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  val paging: Directive[(Int, Int)] = parameters("offset".as[Int] ? 0, "limit".as[Int] ? 5)

  private val MaxItems = 16384

  implicit val ae: ErgoAddressEncoder = ergoSettings.chainSettings.addressEncoder

  val ergoAddress: Directive1[ErgoAddress] = entity(as[String]).flatMap(handleErgoAddress)

  private def handleErgoAddress(value: String): Directive1[ErgoAddress] = {
    ae.fromString(value) match {
      case Success(addr) => provide(addr)
      case _ => reject(ValidationRejection("Wrong address format"))
    }
  }

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
      getTokenInfoByIdR ~
      getAddressBalanceTotalR
  }

  private def getHistory: Future[ErgoHistoryReader] =
    (readersHolder ? GetDataFromHistory[ErgoHistoryReader](r => r)).mapTo[ErgoHistoryReader]

  private def getHistoryWithMempool: Future[(ErgoHistoryReader,ErgoMemPoolReader)] =
    (readersHolder ? GetReaders).mapTo[Readers].map(r => (r.h, r.m))

  private def getAddress(tree: ErgoTree)(history: ErgoHistoryReader): Option[IndexedErgoAddress] = {
    history.typedExtraIndexById[IndexedErgoAddress](bytesToId(IndexedErgoAddressSerializer.hashErgoTree(tree)))
  }

  private def getAddress(addr: ErgoAddress)(history: ErgoHistoryReader): Option[IndexedErgoAddress] = getAddress(addr.script)(history)

  private def getTxById(id: ModifierId)(history: ErgoHistoryReader): Option[IndexedErgoTransaction] =
    history.typedExtraIndexById[IndexedErgoTransaction](id) match {
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

  private def getTxByIndex(index: Long)(history: ErgoHistoryReader): Option[IndexedErgoTransaction] =
    getTxById(history.typedExtraIndexById[NumericTxIndex](bytesToId(NumericTxIndex.indexToBytes(index))).get.m)(history)

  private def getLastTx(history: ErgoHistoryReader): IndexedErgoTransaction =
    getTxByIndex(ByteBuffer.wrap(history.modifierBytesById(bytesToId(GlobalTxIndexKey)).getOrElse(Array.fill[Byte](8){0})).getLong - 1)(history).get

  private def getTxByIndexF(index: Long): Future[Option[IndexedErgoTransaction]] =
    getHistory.map { history =>
      getTxByIndex(index)(history)
    }

  private def getTxByIndexR: Route = (pathPrefix("transaction" / "byIndex" / LongNumber) & get) { index =>
    ApiResponse(getTxByIndexF(index))
  }

  private def getTxsByAddress(addr: ErgoAddress, offset: Int, limit: Int): Future[(Seq[IndexedErgoTransaction],Long)] =
    getHistory.map { history =>
      getAddress(addr)(history) match {
        case Some(addr) => (addr.retrieveTxs(history, offset, limit), addr.txCount())
        case None       => (Seq.empty[IndexedErgoTransaction], 0L)
      }
    }

  private def getTxsByAddressR: Route = (post & pathPrefix("transaction" / "byAddress") & ergoAddress & paging) { (address, offset, limit) =>
    if(limit > MaxItems) {
      BadRequest(s"No more than $MaxItems transactions can be requested")
    }else {
      ApiResponse(getTxsByAddress(address, offset, limit))
    }
  }

  private def getTxRange(offset: Int, limit: Int): Future[Seq[ModifierId]] =
    getHistory.map { history =>
      val base: Int = getLastTx(history).globalIndex.toInt - offset
      val txIds: Array[ModifierId] = new Array[ModifierId](limit)
      cfor(0)(_ < limit, _ + 1) { i =>
        txIds(i) = history.typedExtraIndexById[NumericTxIndex](bytesToId(NumericTxIndex.indexToBytes(base - limit + i))).get.m
      }
      txIds.reverse
    }

  private def getTxRangeR: Route = (pathPrefix("transaction" / "range") & paging) { (offset, limit) =>
    if(limit > MaxItems) {
      BadRequest(s"No more than $MaxItems transactions can be requested")
    }else {
      ApiResponse(getTxRange(offset, limit))
    }
  }

  private def getBoxById(id: ModifierId)(history: ErgoHistoryReader): Option[IndexedErgoBox] =
    history.typedExtraIndexById[IndexedErgoBox](id)

  private def getBoxByIdF(id: ModifierId): Future[Option[IndexedErgoBox]] =
    getHistory.map { history =>
      getBoxById(id)(history)
    }

  private def getBoxByIdR: Route = (get & pathPrefix("box" / "byId") & modifierId) { id =>
    ApiResponse(getBoxByIdF(id))
  }

  private def getBoxByIndex(index: Long)(history: ErgoHistoryReader): Option[IndexedErgoBox] =
    getBoxById(history.typedExtraIndexById[NumericBoxIndex](bytesToId(NumericBoxIndex.indexToBytes(index))).get.m)(history)

  private def getBoxByIndexF(index: Long): Future[Option[IndexedErgoBox]] =
    getHistory.map { history =>
      getBoxByIndex(index)(history)
    }

  private def getBoxByIndexR: Route = (pathPrefix("box" / "byIndex" / LongNumber) & get) { index =>
    ApiResponse(getBoxByIndexF(index))
  }

  private def getBoxesByAddress(addr: ErgoAddress, offset: Int, limit: Int): Future[(Seq[IndexedErgoBox],Long)] =
    getHistory.map { history =>
      getAddress(addr)(history) match {
        case Some(addr) => (addr.retrieveBoxes(history, offset, limit).reverse, addr.boxCount())
        case None       => (Seq.empty[IndexedErgoBox], 0L)
      }
    }

  private def getBoxesByAddressR: Route = (post & pathPrefix("box" / "byAddress") & ergoAddress & paging) { (address, offset, limit) =>
    if(limit > MaxItems) {
      BadRequest(s"No more than $MaxItems boxes can be requested")
    }else {
      ApiResponse(getBoxesByAddress(address, offset, limit))
    }
  }

  private def getBoxesByAddressUnspent(addr: ErgoAddress, offset: Int, limit: Int): Future[Seq[IndexedErgoBox]] =
    getHistory.map { history =>
      getAddress(addr)(history) match {
        case Some(addr) => addr.retrieveUtxos(history, offset, limit).reverse
        case None       => Seq.empty[IndexedErgoBox]
      }
    }

  private def getBoxesByAddressUnspentR: Route = (post & pathPrefix("box" / "unspent" / "byAddress") & ergoAddress & paging) { (address, offset, limit) =>
    if(limit > MaxItems) {
      BadRequest(s"No more than $MaxItems boxes can be requested")
    }else {
      ApiResponse(getBoxesByAddressUnspent(address, offset, limit))
    }
  }

  private def getLastBox(history: ErgoHistoryReader): IndexedErgoBox =
    getBoxByIndex(ByteBuffer.wrap(history.modifierBytesById(bytesToId(GlobalBoxIndexKey)).getOrElse(Array.fill[Byte](8){0})).getLong - 1)(history).get

  private def getBoxRange(offset: Int, limit: Int): Future[Seq[ModifierId]] =
    getHistory.map { history =>
      val base: Int = getLastBox(history).globalIndex.toInt - offset
      val boxIds: Array[ModifierId] = new Array[ModifierId](limit)
      cfor(0)(_ < limit, _ + 1) { i =>
        boxIds(i) = history.typedExtraIndexById[NumericBoxIndex](bytesToId(NumericBoxIndex.indexToBytes(base - limit + i))).get.m
      }
      boxIds.reverse
    }

  private def getBoxRangeR: Route = (pathPrefix("box" / "range") & paging) { (offset, limit) =>
    if(limit > MaxItems) {
      BadRequest(s"No more than $MaxItems boxes can be requested")
    }else {
      ApiResponse(getBoxRange(offset, limit))
    }
  }

  private def getBoxesByErgoTree(tree: ErgoTree, offset: Int, limit: Int): Future[(Seq[IndexedErgoBox],Long)] =
    getHistory.map { history =>
      getAddress(tree)(history) match {
        case Some(iEa) => (iEa.retrieveBoxes(history, offset, limit).reverse, iEa.boxCount())
        case None      => (Seq.empty[IndexedErgoBox], 0L)
      }
    }

  private def getBoxesByErgoTreeR: Route = (post & pathPrefix("box" / "byErgoTree") & ergoTree & paging) { (tree, offset, limit) =>
    if(limit > MaxItems) {
      BadRequest(s"No more than $MaxItems boxes can be requested")
    }else {
      ApiResponse(getBoxesByErgoTree(tree, offset, limit))
    }
  }

  private def getBoxesByErgoTreeUnspent(tree: ErgoTree, offset: Int, limit: Int): Future[Seq[IndexedErgoBox]] =
    getHistory.map { history =>
      getAddress(tree)(history) match {
        case Some(iEa) => iEa.retrieveUtxos(history, offset, limit).reverse
        case None      => Seq.empty[IndexedErgoBox]
      }
    }

  private def getBoxesByErgoTreeUnspentR: Route = (post & pathPrefix("box" / "unspent" / "byErgoTree") & ergoTree & paging) { (tree, offset, limit) =>
    if(limit > MaxItems) {
      BadRequest(s"No more than $MaxItems boxes can be requested")
    }else {
      ApiResponse(getBoxesByErgoTreeUnspent(tree, offset, limit))
    }
  }

  private def getTokenInfoById(id: ModifierId): Future[Option[IndexedToken]] = {
    getHistory.map { history =>
      history.typedExtraIndexById[IndexedToken](IndexedTokenSerializer.uniqueId(id))
    }
  }

  private def getTokenInfoByIdR: Route = (get & pathPrefix("token" / "byId") & modifierId) { id =>
    ApiResponse(getTokenInfoById(id))
  }

  private def getUnconfirmedForAddress(address: ErgoAddress)(mempool: ErgoMemPoolReader): BalanceInfo = {
    val bal: BalanceInfo = new BalanceInfo
    mempool.getAll.map(_.transaction).foreach(tx => {
      tx.outputs.foreach(box => {
        if(IndexedErgoBoxSerializer.getAddress(box.ergoTree).equals(address)) bal.add(box)
      })
    })
    bal
  }

  private def getAddressBalanceTotal(address: ErgoAddress): Future[(BalanceInfo,BalanceInfo)] = {
    getHistoryWithMempool.map { case (history, mempool) =>
      getAddress(address)(history) match {
        case Some(addr) =>
          (addr.balanceInfo.get.retreiveAdditionalTokenInfo(history), getUnconfirmedForAddress(address)(mempool).retreiveAdditionalTokenInfo(history))
        case None =>
          (new BalanceInfo, getUnconfirmedForAddress(address)(mempool).retreiveAdditionalTokenInfo(history))
      }
    }
  }

  private def getAddressBalanceTotalR: Route = (post & pathPrefix("balance") & ergoAddress) { address =>
    ApiResponse(getAddressBalanceTotal(address))
  }

}
