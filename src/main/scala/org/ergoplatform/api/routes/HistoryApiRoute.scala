package org.ergoplatform.api.routes

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.Route
import io.circe.syntax._
import io.swagger.annotations._
import org.ergoplatform.api.services.HistoryService
import scorex.core.api.http.{ApiError, ApiRoute, ScorexApiResponse, SuccessApiResponse}
import scorex.core.settings.Settings

import scala.concurrent.Future

@Path("/history")
@Api(value = "/history", produces = "application/json")
case class HistoryApiRoute(service: HistoryService,
                           override val settings: Settings)
                          (implicit val context: ActorRefFactory) extends ApiRoute {

  implicit val ec = context.dispatcher

  private def getHeight: Future[ScorexApiResponse] = service.getHeight.map { v =>
    SuccessApiResponse(Map("height" -> v).asJson)
  }

  private def getBestHeader: Future[Option[ScorexApiResponse]] = service.getBestHeader.map {
    _.map { header => SuccessApiResponse(header.json) }
  }

  private def getBestFullBlock: Future[Option[ScorexApiResponse]] = service.getBestFullBlock.map {
    _.map { block => SuccessApiResponse(block.json)}
  }

  private def getLastHeaders(n: Int): Future[ScorexApiResponse] = service.getLastHeaders(n).map { v =>
    SuccessApiResponse(Map("headers" -> v.headers.map(_.json)).asJson)
  }

  private def getModifierById(id: String): Future[Option[ScorexApiResponse]] = service.getModifierById(id).map {
    _.map { modifier => SuccessApiResponse(modifier.json)}
  }

  private def getCurrentDifficulty: Future[ScorexApiResponse] = service.getCurrentDifficulty.map { v =>
    SuccessApiResponse(Map("difficulty" -> v.toLong).asJson)
  }

  private def toJsonResponse(fn: ScorexApiResponse): Route = {
    val resp = complete(HttpEntity(ContentTypes.`application/json`, fn.toJson.spaces2))
    withCors(resp)
  }

  private def toJsonResponse(fn: Future[ScorexApiResponse]): Route = onSuccess(fn) { toJsonResponse }

  private def toJsonOptionalResponse(fn: Future[Option[ScorexApiResponse]]): Route = {
    onSuccess(fn) {
      case Some(v) => toJsonResponse(v)
      case None => toJsonResponse(ApiError(404, "not-found"))
    }
  }

  override val route = pathPrefix("history") {
    concat(height, bestHeader, bestFullBlock, lastHeaders, modifierById, currentDifficulty)
  }

  @Path("/height")
  @ApiOperation(value = "Current history tree height", httpMethod = "GET, POST")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with current height")
  ))
  def height: Route = path("height") {
    (get | post) {
      toJsonResponse(getHeight)
    }
  }

  @Path("/best-header")
  @ApiOperation(value = "Current history best header", notes = "Optional response.", httpMethod = "GET, POST")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with with best header")
  ))
  def bestHeader: Route = path("best-header") {
    (get | post) {
      toJsonOptionalResponse(getBestHeader)
    }
  }

  @Path("/best-full-block")
  @ApiOperation(value = "Current history best full block", notes = "Optional response.", httpMethod = "GET, POST")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with with best full block")
  ))
  def bestFullBlock: Route = path("best-full-block") {
    (get | post) {
      toJsonOptionalResponse(getBestFullBlock)
    }
  }

  @Path("/last-headers/{count}")
  @ApiOperation(value = "Last {count} headers.", httpMethod = "GET, POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "count", value = "Count of messages to get", required = false, paramType = "path", dataType = "Int", defaultValue = "10")
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with with last {count} headers")
  ))
  def lastHeaders: Route = path("last-headers" / IntNumber.?) { n =>
    (get | post) {
      toJsonResponse(getLastHeaders(n.getOrElse(10)))
    }
  }

  @Path("/modifier/{id}")
  @ApiOperation(value = "Get modifier by Id", notes = "Optional response.", httpMethod = "GET, POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "id", value = "Modifier Id", required = true, paramType = "path", dataType = "String")
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with with modifier")
  ))
  def modifierById: Route = path("modifier" / Segment) { id =>
    (get | post) {
      toJsonOptionalResponse(getModifierById(id))
    }
  }

  @Path("/current-difficulty")
  @ApiOperation(value = "Current difficulty", httpMethod = "GET, POST")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with current difficulty")
  ))
  def currentDifficulty: Route = path("current-difficulty") {
    (get |post) {
      toJsonResponse(getCurrentDifficulty)
    }
  }
}
