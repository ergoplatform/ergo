package org.ergoplatform.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.local.ErgoMiner.{MiningStatusRequest, MiningStatusResponse}
import org.ergoplatform.modifiers.history.Header
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

case class BlocksApiRoute(viewHolderRef: ActorRef, readersHolder: ActorRef, miner: ActorRef, ergoSettings: ErgoSettings)
                         (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ApiCodecs {

  val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  override val route: Route = (pathPrefix("blocks") & withCors) {
    getBlocksR ~
      postBlocksR ~
      getLastHeadersR ~
      getBlockIdsAtHeightR ~
      getBlockHeaderByHeaderIdR ~
      getBlockTransactionsByHeaderIdR ~
      getFullBlockByHeaderIdR ~
      getModifierByIdR ~
      candidateBlockR
  }

  private def getHistory: Future[ErgoHistoryReader] =
    (readersHolder ? GetDataFromHistory[ErgoHistoryReader](r => r)).mapTo[ErgoHistoryReader]

  private def getHeaderIdsAtHeight(h: Int): Future[Json] = getHistory.map { history =>
    history.headerIdsAtHeight(h).map(Algos.encode).asJson
  }

  private def getLastHeaders(n: Int): Future[Json] = getHistory.map { history =>
    history.lastHeaders(n).headers.map(_.asJson).asJson
  }

  private def getHeaderIds(offset: Int, limit: Int): Future[Json] = getHistory.map { history =>
    history.headerIdsAt(offset, limit).toList.asJson
  }

  private def getFullBlockByHeaderId(headerId: ModifierId): Future[Option[ErgoFullBlock]] = getHistory.map { history =>
    history.typedModifierById[Header](headerId).flatMap(history.getFullBlock)
  }

  private def getModifierById(modifierId: ModifierId): Future[Option[ErgoPersistentModifier]] = getHistory
    .map { _.modifierById(modifierId) }

  def getBlocksR: Route = (pathEndOrSingleSlash & get & paging) { (offset, limit) =>
    ApiResponse(getHeaderIds(offset, limit))
  }

  def postBlocksR: Route = (post & entity(as[ErgoFullBlock])) { block =>
    if (ergoSettings.chainSettings.powScheme.verify(block.header)) {
      log.info("Received a new valid block through the API: " + block)

      viewHolderRef ! LocallyGeneratedModifier(block.header)
      block.blockSections.foreach { viewHolderRef ! LocallyGeneratedModifier(_) }

      ApiResponse.OK
    } else {
      BadRequest("Block is invalid")
    }
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

  def candidateBlockR: Route = (path("candidateBlock") & pathEndOrSingleSlash & get) {
    ApiResponse((miner ? MiningStatusRequest).mapTo[MiningStatusResponse])
  }

  def getFullBlockByHeaderIdR: Route = (modifierId & get) { id =>
    ApiResponse(getFullBlockByHeaderId(id))
  }
}
