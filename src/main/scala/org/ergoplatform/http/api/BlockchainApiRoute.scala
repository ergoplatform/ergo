package org.ergoplatform.http.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.{Directive, Directive1, Route, ValidationRejection}
import akka.http.scaladsl.unmarshalling.Unmarshaller
import akka.pattern.ask
import io.circe.Json
import io.circe.syntax.EncoderOps
import org.ergoplatform.http.api.SortDirection.{ASC, DESC, Direction, INVALID}
import org.ergoplatform.{ErgoAddress, ErgoAddressEncoder}
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetDataFromHistory, GetReaders, Readers}
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.ReceivableMessages.GetSegmentThreshold
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.{GlobalBoxIndexKey, GlobalTxIndexKey, getIndex}
import org.ergoplatform.nodeView.history.extra.IndexedErgoAddressSerializer.hashErgoTree
import org.ergoplatform.nodeView.history.extra.IndexedTokenSerializer.uniqueId
import org.ergoplatform.nodeView.history.extra._
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.settings.{ErgoSettings, RESTApiSettings}
import org.ergoplatform.http.api.ApiError.{BadRequest, InternalError}
import scorex.core.api.http.ApiResponse
import scorex.util.{ModifierId, bytesToId}
import sigma.ast.ErgoTree
import spire.implicits.cfor

import scala.concurrent.duration.{Duration, SECONDS}
import scala.concurrent.{Await, Future}
import scala.util.Success

case class BlockchainApiRoute(readersHolder: ActorRef, ergoSettings: ErgoSettings, indexerOpt: Option[ActorRef])
                        (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ApiCodecs with ApiExtraCodecs {

  val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  private implicit val segmentTreshold: Int = indexerOpt.map { indexer =>
    Await.result[Int]((indexer ? GetSegmentThreshold).asInstanceOf[Future[Int]], Duration(3, SECONDS))
  }.getOrElse(0)

  private val paging: Directive[(Int, Int)] = parameters("offset".as[Int] ? 0, "limit".as[Int] ? 5)

  private implicit val sortMarshaller: Unmarshaller[String, Direction] = Unmarshaller.strict[String, Direction] { str =>
    str.toLowerCase match {
      case "asc" => ASC
      case "desc" => DESC
      case _ => INVALID
    }
  }
  private val sortDir: Directive[Tuple1[Direction]] = parameters("sortDirection".as(sortMarshaller) ? DESC)

  private val unconfirmed: Directive[Tuple1[Boolean]] = parameters("includeUnconfirmed".as[Boolean] ? false)

  /**
    * Total number of boxes/transactions that can be requested at once to avoid too heavy requests ([[BlocksApiRoute.MaxHeaders]])
    */
  private val MaxItems = 16384

  override implicit val ergoAddressEncoder: ErgoAddressEncoder = ergoSettings.chainSettings.addressEncoder

  private val ergoAddress: Directive1[ErgoAddress] = entity(as[String]).flatMap(handleErgoAddress)

  private def handleErgoAddress(value: String): Directive1[ErgoAddress] =
    ergoAddressEncoder.fromString(fromJsonOrPlain(value)) match {
      case Success(addr) => provide(addr)
      case _ => reject(ValidationRejection("Wrong address format"))
    }

  override val route: Route =
  if(ergoSettings.nodeSettings.extraIndex)
    pathPrefix("blockchain") {
      getIndexedHeightR ~
      getTxByIdR ~
      getTxByIndexR ~
      getTxsByAddressR ~
      getTxsByAddressGetRoute ~
      getTxRangeR ~
      getBoxByIdR ~
      getBoxByIndexR ~
      getBoxesByTokenIdR ~
      getBoxesByTokenIdUnspentR ~
      getBoxesByAddressR ~
      getBoxesByAddressGetRoute ~
      getBoxesByAddressUnspentR ~
      getBoxesByAddressUnspentGetRoute ~
      getBoxRangeR ~
      getBoxesByErgoTreeR ~
      getBoxesByErgoTreeUnspentR ~
      getTokenInfoByIdR ~
      getTokenInfoByIdsR ~
      getAddressBalanceTotalR ~
      getAddressBalanceTotalGetRoute
    }
  else
    pathPrefix("blockchain") {
      indexerNotEnabledR
    }

  private def getHistory: Future[ErgoHistoryReader] =
    (readersHolder ? GetDataFromHistory[ErgoHistoryReader](r => r)).mapTo[ErgoHistoryReader]

  private def getHistoryWithMempool: Future[(ErgoHistoryReader,ErgoMemPoolReader)] =
    (readersHolder ? GetReaders).mapTo[Readers].map(r => (r.h, r.m))

  private def getAddress(tree: ErgoTree)(history: ErgoHistoryReader): Option[IndexedErgoAddress] =
    history.typedExtraIndexById[IndexedErgoAddress](hashErgoTree(tree))

  private def getAddress(addr: ErgoAddress)(history: ErgoHistoryReader): Option[IndexedErgoAddress] =
    getAddress(addr.script)(history)

  private def getTxById(id: ModifierId)(history: ErgoHistoryReader): Option[IndexedErgoTransaction] =
    history.typedExtraIndexById[IndexedErgoTransaction](id) match {
      case Some(tx) => Some(tx.retrieveBody(history))
      case None     => None
    }

  private def getTxByIdF(id: ModifierId): Future[Option[IndexedErgoTransaction]] =
    getHistory.map { history =>
      getTxById(id)(history)
    }

  private def getIndexedHeightF: Future[Json] =
    getHistory.map { history =>
      Json.obj(
        "indexedHeight" -> getIndex(ExtraIndexer.IndexedHeightKey, history).getInt.asJson,
        "fullHeight" -> history.fullBlockHeight.asJson
      )
    }

  private def getIndexedHeightR: Route = (pathPrefix("indexedHeight") & get) {
    ApiResponse(getIndexedHeightF)
  }

  private def indexerNotEnabledR: Route = get {
    InternalError("Extra indexing is not enabled")
  }

  private def getTxByIdR: Route = (get & pathPrefix("transaction" / "byId") & modifierId) { id =>
    ApiResponse(getTxByIdF(id))
  }

  private def getTxByIndex(index: Long)(history: ErgoHistoryReader): Option[IndexedErgoTransaction] =
    getTxById(history.typedExtraIndexById[NumericTxIndex](bytesToId(NumericTxIndex.indexToBytes(index))).get.m)(history)

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
        case Some(addr) => (addr.retrieveTxs(history, offset, limit), addr.txCount)
        case None       => (Seq.empty[IndexedErgoTransaction], 0L)
      }
    }

  private def validateAndGetTxsByAddress(address: ErgoAddress,
                                         offset: Int,
                                         limit: Int): Route = {
    if (limit > MaxItems) {
      BadRequest(s"No more than $MaxItems transactions can be requested")
    } else {
      ApiResponse(getTxsByAddress(address, offset, limit))
    }
  }

  private def getTxsByAddressR: Route = (post & pathPrefix("transaction" / "byAddress") & ergoAddress & paging) { (address, offset, limit) =>
    validateAndGetTxsByAddress(address, offset, limit)
  }

  private def getTxsByAddressGetRoute: Route = (pathPrefix("transaction" / "byAddress") & get & addressPass & paging) { (address, offset, limit) =>
    validateAndGetTxsByAddress(address, offset, limit)
  }

  private def getTxRange(offset: Int, limit: Int): Future[Seq[ModifierId]] =
    getHistory.map { history =>
      val base: Long = getIndex(GlobalTxIndexKey, history).getLong - offset
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
        case Some(addr) =>
          val lim = 200
          println(addr.retrieveBoxes(history, 0, lim).reverse.map(_.id).mkString("\",\"")) // a1
          println(addr.retrieveBoxes(history, lim, lim).reverse.map(_.id).mkString("\",\""))      // a2
          println(addr.retrieveBoxes(history, 2 * lim, lim).reverse.map(_.id).mkString("\",\"")) // a3
          println(addr.retrieveBoxes(history, 3 * lim, lim).reverse.map(_.id).mkString("\",\"")) // a4
          println(addr.retrieveBoxes(history, 4 * lim, lim).reverse.map(_.id).mkString("\",\"")) // a5
          println(addr.retrieveBoxes(history, 5 * lim, lim).reverse.map(_.id).mkString("\",\""))
          println(addr.retrieveBoxes(history, 6 * lim, lim).reverse.map(_.id).mkString("\",\""))
          println(addr.retrieveBoxes(history, 7 * lim, lim).reverse.map(_.id).mkString("\",\""))
          println(addr.retrieveBoxes(history, 8 * lim, lim).reverse.map(_.id).mkString("\",\""))
          println(addr.retrieveBoxes(history, 9 * lim, lim).reverse.map(_.id).mkString("\",\""))
          println(addr.boxCount)
          val boxes = addr.retrieveBoxes(history, offset, limit).reverse
          (boxes, addr.boxCount)
        case None       => (Seq.empty[IndexedErgoBox], 0L)
      }
    }

  private def validateAndGetBoxesByAddress(address: ErgoAddress,
                                           offset: Int,
                                           limit: Int) = {
    if (limit > MaxItems) {
      BadRequest(s"No more than $MaxItems boxes can be requested")
    } else {
      ApiResponse(getBoxesByAddress(address, offset, limit))
    }
  }

  private def getBoxesByAddressR: Route = (post & pathPrefix("box" / "byAddress") & ergoAddress & paging) { (address, offset, limit) =>
    validateAndGetBoxesByAddress(address, offset, limit)
  }

  private def getBoxesByAddressGetRoute: Route = (pathPrefix("box" / "byAddress") & get & addressPass & paging) { (address, offset, limit) =>
    validateAndGetBoxesByAddress(address, offset, limit)
  }

  private def getBoxesByAddressUnspent(addr: ErgoAddress, offset: Int, limit: Int, sortDir: Direction, unconfirmed: Boolean): Future[Seq[IndexedErgoBox]] =
    getHistoryWithMempool.map { case (history, mempool) =>
      getAddress(addr)(history)
        .getOrElse(IndexedErgoAddress(hashErgoTree(addr.script)))
        .retrieveUtxos(history, mempool, offset, limit, sortDir, unconfirmed)
    }

  private def validateAndGetBoxesByAddressUnspent(address: ErgoAddress,
                                                  offset: Int,
                                                  limit: Int,
                                                  dir: Direction,
                                                  unconfirmed: Boolean): Route = {
    if (limit > MaxItems) {
      BadRequest(s"No more than $MaxItems boxes can be requested")
    } else if (dir == SortDirection.INVALID) {
      BadRequest("Invalid parameter for sort direction, valid values are \"ASC\" and \"DESC\"")
    } else {
      ApiResponse(getBoxesByAddressUnspent(address, offset, limit, dir, unconfirmed))
    }
  }

  private def getBoxesByAddressUnspentR: Route =
    (post & pathPrefix("box" / "unspent" / "byAddress") & ergoAddress & paging & sortDir & unconfirmed) {
      (address, offset, limit, dir, unconfirmed) =>
        validateAndGetBoxesByAddressUnspent(address, offset, limit, dir, unconfirmed)
    }

  private def getBoxesByAddressUnspentGetRoute: Route =
    (pathPrefix("box" / "unspent" / "byAddress") & get & addressPass & paging & sortDir & unconfirmed) {
      (address, offset, limit, dir, unconfirmed) =>
        validateAndGetBoxesByAddressUnspent(address, offset, limit, dir, unconfirmed)
    }

  private def getBoxRange(offset: Int, limit: Int): Future[Seq[ModifierId]] =
    getHistory.map { history =>
      val base: Long = getIndex(GlobalBoxIndexKey, history).getLong - offset
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
        case Some(iEa) => (iEa.retrieveBoxes(history, offset, limit).reverse, iEa.boxCount)
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

  private def getBoxesByErgoTreeUnspent(tree: ErgoTree, offset: Int, limit: Int, sortDir: Direction, unconfirmed: Boolean): Future[Seq[IndexedErgoBox]] =
    getHistoryWithMempool.map { case (history, mempool) =>
      getAddress(tree)(history)
        .getOrElse(IndexedErgoAddress(hashErgoTree(tree)))
        .retrieveUtxos(history, mempool, offset, limit, sortDir, unconfirmed)
    }

  private def getBoxesByErgoTreeUnspentR: Route = (post & pathPrefix("box" / "unspent" / "byErgoTree") & ergoTree & paging & sortDir & unconfirmed) { (tree, offset, limit, dir, unconfirmed) =>
    if(limit > MaxItems) {
      BadRequest(s"No more than $MaxItems boxes can be requested")
    }else if (dir == SortDirection.INVALID) {
      BadRequest("Invalid parameter for sort direction, valid values are 'ASC' and 'DESC'")
    }else {
      ApiResponse(getBoxesByErgoTreeUnspent(tree, offset, limit, dir, unconfirmed))
    }
  }

  private def getTokenInfoByIds(ids: Seq[ModifierId]): Future[Seq[IndexedToken]] = {
    getHistory.map { history =>
      ids.flatMap(id => history.typedExtraIndexById[IndexedToken](uniqueId(id)))
    }
  }

  private def getTokenInfoById(id: ModifierId): Future[Option[IndexedToken]] = {
    getHistory.map { history =>
      history.typedExtraIndexById[IndexedToken](uniqueId(id))
    }
  }

  private def getTokenInfoByIdR: Route = (get & pathPrefix("token" / "byId") & modifierId) { id =>
    ApiResponse(getTokenInfoById(id))
  }

  private def getTokenInfoByIdsR: Route = (post & pathPrefix("tokens") & entity(as[Seq[ModifierId]])) { ids =>
    ApiResponse(getTokenInfoByIds(ids))
  }

  private def getBoxesByTokenId(id: ModifierId, offset: Int, limit: Int): Future[(Seq[IndexedErgoBox],Long)] =
    getHistory.map { history =>
      history.typedExtraIndexById[IndexedToken](uniqueId(id)) match {
        case Some(token) => (token.retrieveBoxes(history, offset, limit), token.boxCount)
        case None        => (Seq.empty[IndexedErgoBox], 0L)
      }
    }

  private def getBoxesByTokenIdR: Route = (get & pathPrefix("box" / "byTokenId") & modifierId & paging) { (id, offset, limit) =>
    ApiResponse(getBoxesByTokenId(id, offset, limit))
  }

  private def getBoxesByTokenIdUnspent(id: ModifierId, offset: Int, limit: Int, sortDir: Direction, unconfirmed: Boolean): Future[Seq[IndexedErgoBox]] =
    getHistoryWithMempool.map { case (history, mempool) =>
      history.typedExtraIndexById[IndexedToken](uniqueId(id))
        .getOrElse(IndexedToken(id))
        .retrieveUtxos(history, mempool, offset, limit, sortDir, unconfirmed)
    }

  private def getBoxesByTokenIdUnspentR: Route = (get & pathPrefix("box" / "unspent" / "byTokenId") & modifierId & paging & sortDir & unconfirmed) { (id, offset, limit, dir, unconfirmed) =>
    if (limit > MaxItems) {
      BadRequest(s"No more than $MaxItems boxes can be requested")
    } else if (dir == SortDirection.INVALID) {
      BadRequest("Invalid parameter for sort direction, valid values are 'ASC' and 'DESC'")
    } else {
      ApiResponse(getBoxesByTokenIdUnspent(id, offset, limit, dir, unconfirmed))
    }
  }

  private def getUnconfirmedForAddress(address: ErgoAddress)(mempool: ErgoMemPoolReader): BalanceInfo = {
    val bal: BalanceInfo = BalanceInfo()
    mempool.getAll.map(_.transaction).foreach(tx => {
      tx.outputs.foreach(box => {
        if(address.equals(ExtraIndexer.getAddress(box.ergoTree))) bal.add(box)
      })
    })
    bal
  }

  private def getAddressBalanceTotal(address: ErgoAddress): Future[(BalanceInfo,BalanceInfo)] = {
    getHistoryWithMempool.map { case (history, mempool) =>
      getAddress(address)(history) match {
        case Some(addr) =>
          (addr.balanceInfo.get.retrieveAdditionalTokenInfo(history), getUnconfirmedForAddress(address)(mempool).retrieveAdditionalTokenInfo(history))
        case None =>
          (BalanceInfo(), getUnconfirmedForAddress(address)(mempool).retrieveAdditionalTokenInfo(history))
      }
    }
  }

  private def getAddressBalanceTotalR: Route = (post & pathPrefix("balance") & ergoAddress) { address =>
    ApiResponse(getAddressBalanceTotal(address))
  }

  /** Parses address in the url (i.e. `balanceForAddress/{address}` into [[ErgoAddress]] using [[ErgoAddressEncoder]]. */
  private val addressPass: Directive1[ErgoAddress] = pathPrefix(Segment).flatMap(handleErgoAddress)

  private def getAddressBalanceTotalGetRoute: Route =
    (pathPrefix("balanceForAddress") & get & addressPass) { address =>
      ApiResponse(getAddressBalanceTotal(address))
    }

}
