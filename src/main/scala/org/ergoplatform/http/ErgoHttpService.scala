package org.ergoplatform.http

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.HttpHeader
import akka.http.scaladsl.server.{ExceptionHandler, RejectionHandler, Route}
import akka.http.scaladsl.server.Directive0
import akka.http.scaladsl.server.directives.RouteDirectives
import scorex.core.api.http.{ApiErrorHandler, ApiRejectionHandler, ApiRoute, CorsHandler}
import akka.http.scaladsl.model.headers._

import scala.collection.immutable

final case class ErgoHttpService(
  apiRoutes: Seq[ApiRoute],
  swaggerRoute: SwaggerRoute,
  panelRoute: NodePanelRoute
)(implicit val system: ActorSystem) extends CorsHandler {

  def rejectionHandler: RejectionHandler = ApiRejectionHandler.rejectionHandler

  def exceptionHandler: ExceptionHandler = ApiErrorHandler.exceptionHandler

  private val corsResponseHeaders: List[ModeledHeader] = List[ModeledHeader](
    `Access-Control-Allow-Origin`.*,
    `Access-Control-Allow-Credentials`(true),
    `Access-Control-Allow-Headers`("Authorization", "Content-Type", "X-Requested-With", "api_key",
      "openai-conversation-id",
      "openai-ephemeral-user-id",
      "baggage",
      "sentry-trace"
    )
  )

  override def respondWithHeaders(responseHeaders: immutable.Seq[HttpHeader]): Directive0 = {
    super.respondWithHeaders(corsResponseHeaders)
  }

  val compositeRoute: Route =
    handleRejections(rejectionHandler) {
      handleExceptions(exceptionHandler) {
        corsHandler {
          apiR ~
            apiSpecR ~
            swaggerRoute.route ~
            panelRoute.route ~
            redirectToSwaggerR
        }
      }
    }

  private def apiR: Route =
    apiRoutes.map(_.route).reduceOption(_ ~ _).getOrElse(RouteDirectives.reject)

  private def apiSpecR: Route =
    (get & path("api-docs" / "openapi.yaml")) {
      getFromResource("api/openapi.yaml")
    }

  private def redirectToSwaggerR: Route = path("" | "/") {
    redirect("/swagger", StatusCodes.PermanentRedirect)
  }
}
