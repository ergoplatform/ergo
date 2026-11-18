package org.ergoplatform.http

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.{ExceptionHandler, RejectionHandler, Route}
import akka.http.scaladsl.server.Directive0
import akka.http.scaladsl.server.directives.RouteDirectives
import scorex.core.api.http.{ApiErrorHandler, ApiRejectionHandler, ApiRoute, CorsHandler}
import scorex.util.ScorexLogging

final case class ErgoHttpService(
  apiRoutes: Seq[ApiRoute],
  swaggerRoute: SwaggerRoute,
  panelRoute: NodePanelRoute
)(implicit val system: ActorSystem) extends CorsHandler with ScorexLogging {

  def rejectionHandler: RejectionHandler = ApiRejectionHandler.rejectionHandler

  def exceptionHandler: ExceptionHandler = ApiErrorHandler.exceptionHandler

  override protected def corsAllowedOrigin: Option[String] =
    swaggerRoute.settings.corsAllowedOrigin

  override protected def corsAllowedHeaders: Seq[String] =
    super.corsAllowedHeaders ++ Seq(
      "openai-conversation-id",
      "openai-ephemeral-user-id",
      "baggage",
      "sentry-trace"
    )

  /**
    * Logs every query served by the node's HTTP interface: method, relative URI (path and query
    * string), response status and how long it took.
    *
    * Bodies are deliberately not logged, as requests carry secrets (a mnemonic on
    * `/wallet/restore`, a password on `/wallet/unlock`, and so on) and responses can be large.
    *
    * Off by default, since the root logger is at INFO. To switch it on, add to `logback.xml`:
    * {{{
    *   <logger name="org.ergoplatform.http.ErgoHttpService" level="DEBUG"/>
    * }}}
    * When it is off, the message is never built: `log.debug` is a macro guarded by `isDebugEnabled`.
    */
  private val logQueries: Directive0 =
    extractRequest.flatMap { request =>
      val startTime = System.currentTimeMillis()
      mapResponse { response =>
        val elapsedMs = System.currentTimeMillis() - startTime
        log.debug(s"${request.method.value} ${request.uri.toRelative} - " +
          s"${response.status.intValue()} in $elapsedMs ms")
        response
      }
    }

  val compositeRoute: Route =
    logQueries {
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
