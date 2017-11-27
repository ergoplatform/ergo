package org.ergoplatform.api.routes

import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import akka.http.scaladsl.server.Route
import scorex.core.api.http.{ApiRoute, ScorexApiResponse}

import scala.concurrent.Future

trait ErgoBaseApiRoute extends ApiRoute {

  implicit val ec = context.dispatcher

  protected def toJsonResponse(fn: ScorexApiResponse): Route = {
    val resp = complete(HttpEntity(ContentTypes.`application/json`, fn.toJson.spaces2))
    withCors(resp)
  }

  protected def toJsonResponse(fn: Future[ScorexApiResponse]): Route = onSuccess(fn) { toJsonResponse }

  protected def toJsonOptionalResponse(fn: Future[Option[ScorexApiResponse]]): Route = {
    onSuccess(fn) {
      case Some(v) => toJsonResponse(v)
      case None => withCors(complete(StatusCodes.NotFound))
    }
  }

}
