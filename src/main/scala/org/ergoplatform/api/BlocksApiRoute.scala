package org.ergoplatform.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.local.ErgoMiner.{MiningStatusRequest, MiningStatusResponse}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.nodeView.ErgoReadersHolder.GetDataFromHistory
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.settings.{Algos, ErgoSettings}
import scorex.core.ModifierId
import scorex.core.settings.RESTApiSettings
import scorex.core.utils.ScorexLogging

import scala.concurrent.Future

case class BlocksApiRoute(readersHolder: ActorRef, miner: ActorRef, ergoSettings: ErgoSettings)
                         (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ScorexLogging {

  override val route: Route = pathPrefix("blocks") {
    getBlocksR ~
      postBlocksR ~
      getLastHeadersR ~
      getBlockIdsAtHeightR ~
      getBlockHeaderByHeaderIdR ~
      getBlockTransactionsByHeaderIdR ~
      getFullBlockByHeaderIdR ~
      candidateBlockR
  }

  override val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

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
    getHeaderIds(limit, offset).okJson()
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

  def getLastHeadersR: Route = (pathPrefix("lastHeaders" / IntNumber) & get) { count => getLastHeaders(count).okJson() }

  def getBlockIdsAtHeightR: Route = (pathPrefix("at" / IntNumber) & get) { height =>
    getHeaderIdsAtHeight(height).okJson()
  }

  def getBlockHeaderByHeaderIdR: Route = (headerId & pathPrefix("header") & get) { id =>
    getFullBlockByHeaderId(id).map(_.map(_.header.asJson)).okJson()
  }

  def getBlockTransactionsByHeaderIdR: Route = (headerId & pathPrefix("transactions") & get) { id =>
    getFullBlockByHeaderId(id).map(_.map(_.blockTransactions.asJson)).okJson()
  }

  def candidateBlockR: Route = (path("candidateBlock") & pathEndOrSingleSlash & get) {
    (miner ? MiningStatusRequest).mapTo[MiningStatusResponse].map(_.asJson).okJson()
  }

  def getFullBlockByHeaderIdR: Route = (headerId & get) { id =>
    getFullBlockByHeaderId(id).map(_.map(_.asJson)).okJson()
  }
}
