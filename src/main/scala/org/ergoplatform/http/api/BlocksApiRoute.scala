package org.ergoplatform.http.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.{Directive, Route}
import akka.pattern.ask
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.history.popow.PoPowHeader
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.ErgoReadersHolder.GetDataFromHistory
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.settings.{Algos, ErgoSettings}
import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedModifier
import scorex.core.api.http.ApiError.BadRequest
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings
import scorex.util.ModifierId

import scala.concurrent.Future

case class BlocksApiRoute(viewHolderRef: ActorRef, readersHolder: ActorRef, ergoSettings: ErgoSettings)
                         (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ApiCodecs {

  val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  override val route: Route = pathPrefix("blocks") {
    getBlocksR ~
      postBlocksR ~
      getLastHeadersR ~
      getChainSliceR ~
      getBlockIdsAtHeightR ~
      getBlockHeaderByHeaderIdR ~
      getBlockTransactionsByHeaderIdR ~
      getFullBlockByHeaderIdR ~
      getModifierByIdR ~
      getPoPowHeaderByHeaderIdR ~
      getPoPowHeaderByHeightR
  }

  private val maxHeadersInOneQuery = ergoSettings.chainSettings.epochLength * 2

  private def getHistory: Future[ErgoHistoryReader] =
    (readersHolder ? GetDataFromHistory[ErgoHistoryReader](r => r)).mapTo[ErgoHistoryReader]

  private def getHeaderIdsAtHeight(h: Int): Future[Json] =
    getHistory.map { history =>
      history.headerIdsAtHeight(h).map(Algos.encode).asJson
    }

  private def getLastHeaders(n: Int): Future[Json] =
    getHistory.map { history =>
      history.lastHeaders(n).map(_.asJson).asJson
    }

  private def getHeaderIds(offset: Int, limit: Int): Future[Json] =
    getHistory.map { history =>
      history.headerIdsAt(offset, limit).toList.asJson
    }

  private def getFullBlockByHeaderId(headerId: ModifierId): Future[Option[ErgoFullBlock]] =
    getHistory.map { history =>
      history.typedModifierById[Header](headerId).flatMap(history.getFullBlock)
    }

  private def getModifierById(modifierId: ModifierId): Future[Option[ErgoPersistentModifier]] =
    getHistory.map(_.modifierById(modifierId))

  private def getChainSlice(fromHeight: Int, toHeight: Int): Future[Json] =
    getHistory.map { history =>
      val maxHeaderOpt = if (toHeight >= 0) {
        history.headerIdsAtHeight(toHeight)
          .headOption
          .flatMap(history.typedModifierById[Header](_))
          .orElse(history.bestHeaderOpt)
      } else {
        history.bestHeaderOpt
      }
      val headers = maxHeaderOpt
        .toIndexedSeq
        .flatMap { maxHeader =>
          history.headerChainBack(maxHeadersInOneQuery, maxHeader, _.height <= fromHeight + 1)
        }
      headers.toList.asJson
    }

  private def getPoPowHeaderById(headerId: ModifierId): Future[Option[PoPowHeader]] =
    getHistory.map(_.getPoPowHeader(headerId))

  private def getPoPowHeaderByHeight(height: Int): Future[Option[PoPowHeader]] =
    getHistory.map(_.getPoPowHeader(height))

  private val chainPagination: Directive[(Int, Int)] =
    parameters("fromHeight".as[Int] ? 0, "toHeight".as[Int] ? -1)

  def getBlocksR: Route = (pathEndOrSingleSlash & get & paging) { (offset, limit) =>
    ApiResponse(getHeaderIds(offset, limit))
  }

  def postBlocksR: Route = (post & entity(as[ErgoFullBlock])) { block =>
    if (ergoSettings.chainSettings.powScheme.validate(block.header).isSuccess) {
      log.info("Received a new valid block through the API: " + block)

      viewHolderRef ! LocallyGeneratedModifier(block.header)
      block.blockSections.foreach { viewHolderRef ! LocallyGeneratedModifier(_) }

      ApiResponse.OK
    } else {
      BadRequest("Block is invalid")
    }
  }

  def getChainSliceR: Route = (pathPrefix("chainSlice") & chainPagination) { (fromHeight, toHeight) =>
    ApiResponse(getChainSlice(fromHeight, toHeight))
  }

  def getModifierByIdR: Route = (pathPrefix("modifier") & modifierId & get) { id =>
    ApiResponse(getModifierById(id))
  }

  def getLastHeadersR: Route = (pathPrefix("lastHeaders" / IntNumber) & get) { count =>
    ApiResponse(getLastHeaders(count))
  }

  def getBlockIdsAtHeightR: Route = (pathPrefix("at" / IntNumber) & get) { height =>
    ApiResponse(getHeaderIdsAtHeight(height))
  }

  def getBlockHeaderByHeaderIdR: Route = (modifierId & pathPrefix("header") & get) { id =>
    ApiResponse(getFullBlockByHeaderId(id).map(_.map(_.header)))
  }

  def getBlockTransactionsByHeaderIdR: Route = (modifierId & pathPrefix("transactions") & get) { id =>
    ApiResponse(getFullBlockByHeaderId(id).map(_.map(_.blockTransactions)))
  }

  def getFullBlockByHeaderIdR: Route = (modifierId & get) { id =>
    ApiResponse(getFullBlockByHeaderId(id))
  }

  def getPoPowHeaderByHeaderIdR: Route = (pathPrefix("popowHeaderById") & modifierId & get) { headerId =>
    ApiResponse(getPoPowHeaderById(headerId))
  }

  def getPoPowHeaderByHeightR: Route = (pathPrefix("popowHeaderByHeight" / IntNumber) & get) { headerId =>
    ApiResponse(getPoPowHeaderByHeight(headerId))
  }

}
