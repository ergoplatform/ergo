package org.ergoplatform.api

import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import akka.http.scaladsl.server.{Directive, Directive1, Route}
import io.circe.Json
import org.ergoplatform.settings.Algos
import scorex.core.ModifierId
import scorex.core.api.http.ApiRoute

import scala.concurrent.Future
import scala.util.Success

trait ErgoBaseApiRoute extends ApiRoute {

  implicit val ec = context.dispatcher

  protected def toJsonResponse(js: Json): Route = {
    val resp = complete(HttpEntity(ContentTypes.`application/json`, js.spaces2))
    withCors(resp)
  }

  protected def toJsonResponse(fn: Future[Json]): Route = onSuccess(fn) {
    toJsonResponse
  }

  protected def toJsonOptionalResponse(fn: Future[Option[Json]]): Route = {
    onSuccess(fn) {
      case Some(v) => toJsonResponse(v)
      case None => withCors(complete(StatusCodes.NotFound))
    }
  }

  val paging: Directive[(Int, Int)] = parameters("offset".as[Int] ? 0, "limit".as[Int] ? 50)

  val headerId: Directive1[ModifierId] = pathPrefix(Segment).flatMap { h =>
    Algos.decode(h) match {
      case Success(header) => provide(ModifierId @@ header)
      case _ => reject
    }
  }

  implicit class OkJsonResp(fn: Future[Json]) {
    def okJson(): Route = toJsonResponse(fn)
  }

  implicit class OkJsonOptResp(fn: Future[Option[Json]]) {
    def okJson(): Route = toJsonOptionalResponse(fn)
  }

}
