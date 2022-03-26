package org.ergoplatform.http

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.{ExceptionHandler, RejectionHandler, Route}
import akka.http.scaladsl.server.directives.RouteDirectives
import scorex.core.api.http.{ApiErrorHandler, ApiRejectionHandler, ApiRoute, CorsHandler}


final case class ErgoHttpService(
  apiRoutes: Seq[ApiRoute],
  swaggerRoute: SwaggerRoute,
  panelRoute: NodePanelRoute
)(implicit val system: ActorSystem) extends CorsHandler {

  def rejectionHandler: RejectionHandler = ApiRejectionHandler.rejectionHandler

  def exceptionHandler: ExceptionHandler = ApiErrorHandler.exceptionHandler

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
