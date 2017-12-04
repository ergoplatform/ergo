package org.ergoplatform.api.routes

import javax.ws.rs.Path

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.syntax._
import io.swagger.annotations._
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{DigestState, UtxoState}
import org.ergoplatform.nodeView.wallet.ErgoWallet
import scorex.core.NodeViewHolder.GetDataFromCurrentView
import scorex.core.api.http.{ScorexApiResponse, SuccessApiResponse}
import scorex.core.settings.RESTApiSettings
import scorex.crypto.encode.Base58

import scala.concurrent.Future

@Path("/history")
@Api(value = "/history", produces = "application/json")
case class HistoryApiRoute(nodeViewActorRef: ActorRef, settings: RESTApiSettings, digest: Boolean)
                          (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute {

  override val route = pathPrefix("history") {
    concat(height, bestHeader, bestFullBlock, lastHeaders, modifierById, currentDifficulty, blockIdByHeight,
      headersAt, fullBlocksAt)
  }

  private val request = if (digest) {
    GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, ErgoHistory](_.history)
  } else {
    GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, ErgoHistory](_.history)
  }

  private def getHistory = (nodeViewActorRef ? request).mapTo[ErgoHistory]

  private def getHeight: Future[ScorexApiResponse] = getHistory.map { h =>
    SuccessApiResponse(Map(
      "headers-height" -> h.headersHeight,
      "full-height" -> h.fullBlockHeight
    ).asJson)
  }

  private def getBestHeader: Future[Option[ScorexApiResponse]] = getHistory.map{ _.bestHeaderOpt }.map {
    _.map { header => SuccessApiResponse(header.json) }
  }

  private def getBestFullBlock: Future[Option[ScorexApiResponse]] = getHistory.map{ _.bestFullBlockOpt }.map {
    _.map { block => SuccessApiResponse(block.json)}
  }

  private def getHeaderIdsAtHeight(h: Int): Future[Option[ScorexApiResponse]] = getHistory.map{ _.headerIdsAtHeight(h) }.map {
    headerIds => headerIds.headOption
      .map(_ => SuccessApiResponse(headerIds.map(h => Base58.encode(h)).asJson))
  }

  private def getLastHeaders(n: Int): Future[ScorexApiResponse] = getHistory.map{ _.lastHeaders(n) }.map { v =>
    SuccessApiResponse(Map("headers" -> v.headers.map(_.json)).asJson)
  }

  private def getModifierById(id: String): Future[Option[ScorexApiResponse]] = getHistory.map{ _.modifierById(id) }.map {
    _.map { modifier => SuccessApiResponse(modifier.json)}
  }

  private def getCurrentDifficulty: Future[ScorexApiResponse] = getHistory.map{ _.requiredDifficulty }.map { v =>
    SuccessApiResponse(Map("difficulty" -> v.toLong).asJson)
  }

  @Path("/height")
  @ApiOperation(value = "Current history tree height", httpMethod = "GET")
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with current height")))
  def height: Route = path("height") {
    get {
      toJsonResponse(getHeight)
    }
  }

  @Path("/best-header")
  @ApiOperation(value = "Current history best header", notes = "Optional response.", httpMethod = "GET")
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with best header")))
  def bestHeader: Route = path("best-header") {
    get {
      toJsonOptionalResponse(getBestHeader)
    }
  }

  @Path("/header-ids-by-height")
  @ApiOperation(value = "Header ids by height", notes = "Optional response.", httpMethod = "GET")
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with array of header ids")))
  def blockIdByHeight: Route = path("best-full-block" / IntNumber) { h =>
    get {
      toJsonOptionalResponse(getHeaderIdsAtHeight(h))
    }
  }

  @Path("/best-full-block")
  @ApiOperation(value = "Current history best full block", notes = "Optional response.", httpMethod = "GET")
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with best full block")))
  def bestFullBlock: Route = path("best-full-block") {
    get {
      toJsonOptionalResponse(getBestFullBlock)
    }
  }

  @Path("/last-headers/{count}")
  @ApiOperation(value = "Last {count} headers.", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "count",
      value = "Count of messages to get",
      required = false,
      paramType = "path",
      dataType = "Int",
      defaultValue = "10")
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with last {count} headers")))
  def lastHeaders: Route = path("last-headers" / IntNumber.?) { n =>
    get {
      toJsonResponse(getLastHeaders(n.getOrElse(10)))
    }
  }

  @Path("/headers/at/{height}")
  @ApiOperation(value = "All headers at height {height}. First one is from the best chain", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "height",
      value = "Height to get headers",
      required = false,
      paramType = "path",
      dataType = "Int")
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with last {count} headers")))
  def headersAt: Route = path("headers" / "at" / IntNumber) { height =>
    get {
      toJsonResponse {
        getHistory.map { history =>
          val headers = history.headerIdsAtHeight(height).flatMap(id => history.typedModifierById[Header](id))
          SuccessApiResponse(Map("headers" -> headers.map(_.json)).asJson)
        }
      }
    }
  }

  @Path("/full-blocks/at/{height}")
  @ApiOperation(value = "All full blocks at height {height}. First one is from the best chain", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "height",
      value = "Height to get headers",
      required = false,
      paramType = "path",
      dataType = "Int")
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with last {count} headers")))
  def fullBlocksAt: Route = path("full-blocks" / "at" / IntNumber) { height =>
    get {
      toJsonResponse {
        getHistory.map { history =>
          val headers = history.headerIdsAtHeight(height).flatMap(id => history.typedModifierById[Header](id))
          SuccessApiResponse(Map("full-blocks" -> headers.flatMap(history.getFullBlock).map(_.json)).asJson)
        }
      }
    }
  }

  @Path("/modifier/{id}")
  @ApiOperation(value = "Get modifier by Id", notes = "Optional response.", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "id", value = "Modifier Id", required = true, paramType = "path", dataType = "String")
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with modifier")))
  def modifierById: Route = path("modifier" / Segment) { id =>
    get {
      toJsonOptionalResponse(getModifierById(id))
    }
  }

  @Path("/current-difficulty")
  @ApiOperation(value = "Current difficulty", httpMethod = "GET")
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with current difficulty")))
  def currentDifficulty: Route = path("current-difficulty") {
    get {
      toJsonResponse(getCurrentDifficulty)
    }
  }
}
