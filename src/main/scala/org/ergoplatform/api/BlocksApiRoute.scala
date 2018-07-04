package org.ergoplatform.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.local.ErgoMiner.MiningStatusRequest
import org.ergoplatform.local.MiningStatus
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.nodeView.ErgoReadersHolder.GetDataFromHistory
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.ergoplatform.utils.JsonEncoders
import scorex.core.ModifierId
import scorex.core.api.http.ApiResponse

import scala.concurrent.Future

case class BlocksApiRoute(readersHolder: ActorRef, miner: ActorRef, ergoSettings: ErgoSettings)
                         (implicit val context: ActorRefFactory, encoders: JsonEncoders) extends ErgoBaseApiRoute {

  import encoders._

  val settings = ergoSettings.scorexSettings.restApi

  override val route: Route = (pathPrefix("blocks") & withCors) {
    getBlocksR ~
      postBlocksR ~
      getLastHeadersR ~
      getBlockIdsAtHeightR ~
      getBlockHeaderByHeaderIdR ~
      getBlockTransactionsByHeaderIdR ~
      getFullBlockByHeaderIdR ~
      candidateBlockR
  }

  private def getHistory = (readersHolder ? GetDataFromHistory[ErgoHistoryReader](r => r)).mapTo[ErgoHistoryReader]

  private def getHeaderIdsAtHeight(h: Int): Future[Json] = getHistory.map { history =>
    history.headerIdsAtHeight(h).map(Algos.encode).asJson
  }

  private def getLastHeaders(n: Int): Future[Json] = getHistory.map { history =>
    history.lastHeaders(n).headers.map(_.asJson).asJson
  }

  private def getHeaderIds(limit: Int, offset: Int): Future[Json] = getHistory.map { history =>
    history.headerIdsAt(limit, offset).map(Algos.encode).asJson
  }

  private def getFullBlockByHeaderId(headerId: ModifierId): Future[Option[ErgoFullBlock]] = getHistory.map { history =>
    history.typedModifierById[Header](headerId).flatMap(history.getFullBlock)
  }

  def getBlocksR: Route = (pathEndOrSingleSlash & get & paging) { (offset, limit) =>
    ApiResponse(getHeaderIds(limit, offset))
  }

  def postBlocksR: Route = post {
    ???
    /*
        entity(as[ErgoFullBlock]) { block =>
          complete {
            if (powScheme.verify(block.header)) {
              log.info("Received a new valid block through the API: " + block)

              nodeViewActorRef ! LocallyGeneratedModifier(block.header)
              nodeViewActorRef ! LocallyGeneratedModifier(block.blockTransactions)
              block.aDProofs.foreach { adp =>
                nodeViewActorRef ! LocallyGeneratedModifier(adp)
              }
              StatusCodes.OK
            } else {
                StatusCodes.BadRequest -> "invalid.block"
            }
          }
    */
  }

  def getLastHeadersR: Route = (pathPrefix("lastHeaders" / IntNumber) & get) { count =>
    ApiResponse(getLastHeaders(count))
  }

  def getBlockIdsAtHeightR: Route = (pathPrefix("at" / IntNumber) & get) { height =>
    ApiResponse(getHeaderIdsAtHeight(height))
  }

  def getBlockHeaderByHeaderIdR: Route = (headerId & pathPrefix("header") & get) { id =>
    ApiResponse(getFullBlockByHeaderId(id).map(_.map(_.header)))
  }

  def getBlockTransactionsByHeaderIdR: Route = (headerId & pathPrefix("transactions") & get) { id =>
    ApiResponse(getFullBlockByHeaderId(id).map(_.map(_.blockTransactions)))
  }

  def candidateBlockR: Route = (path("candidateBlock") & pathEndOrSingleSlash & get) {
    ApiResponse((miner ? MiningStatusRequest).mapTo[MiningStatus])
  }

  def getFullBlockByHeaderIdR: Route = (headerId & get) { id =>
    ApiResponse(getFullBlockByHeaderId(id))
  }
}
