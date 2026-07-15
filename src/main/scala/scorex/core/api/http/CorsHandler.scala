package scorex.core.api.http

import akka.http.scaladsl.marshalling.ToResponseMarshallable.apply
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Directive.addByNameNullaryApply
import akka.http.scaladsl.server.{Directive0, Directives, Route}

/**
  * Provides tools for handling a Cross-Origin Resource Sharing spec workflow
  * (including `OPTIONS` pre-flight requests).
  */
trait CorsHandler extends Directives {

  protected def corsAllowedOrigin: Option[String] = Some("*")

  protected def corsAllowedHeaders: Seq[String] =
    Seq("Authorization", "Content-Type", "X-Requested-With", "api_key")

  private lazy val corsResponseHeaders: List[ModeledHeader] =
    corsAllowedOrigin.toList.flatMap {
      case "*" =>
        List[ModeledHeader](
          `Access-Control-Allow-Origin`.*,
          `Access-Control-Allow-Headers`(corsAllowedHeaders: _*)
        )
      case origin =>
        List[ModeledHeader](
          `Access-Control-Allow-Origin`(HttpOrigin(origin)),
          `Access-Control-Allow-Credentials`(true),
          `Access-Control-Allow-Headers`(corsAllowedHeaders: _*)
        )
    }

  def corsHandler(r: Route): Route = corsAllowedOrigin.fold(r) { _ =>
    addAccessControlHeaders {
      preflightRequestHandler ~ r
    }
  }

  def addCorsHeaders(response: HttpResponse): HttpResponse =
    if (corsResponseHeaders.isEmpty) response
    else response.withHeaders(corsResponseHeaders)

  private def addAccessControlHeaders: Directive0 =
    respondWithHeaders(corsResponseHeaders)

  private def preflightRequestHandler: Route = options {
    complete {
      HttpResponse(StatusCodes.OK)
        .withHeaders(`Access-Control-Allow-Methods`(OPTIONS, POST, PUT, GET, DELETE))
    }
  }

}
