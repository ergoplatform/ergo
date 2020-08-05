package org.ergoplatform.http

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.RouteDirectives
import scorex.core.api.http.{ApiRoute, CorsHandler}

final case class ErgoHttpService(
  apiRoutes: Seq[ApiRoute],
  swaggerRoute: SwaggerRoute,
  panelRoute: NodePanelRoute
)(implicit val system: ActorSystem) extends CorsHandler {

  val compositeRoute: Route =
    corsHandler {
      apiR ~
        apiSpecR ~
        swaggerRoute.route ~
        panelRoute.route ~
        redirectToSwaggerR ~
        redirectToPanelR
    }

  private def apiR: Route =
    apiRoutes.map(_.route).reduceOption(_ ~ _).getOrElse(RouteDirectives.reject)

  private def apiSpecR: Route =
    (get & path("api-docs" / "openapi.yaml")) {
      getFromResource("api/openapi.yaml")
    }

  private def redirectToPanelR: Route = path("/panel/" | "/panel/*") {
    redirect("/panel", StatusCodes.PermanentRedirect)
  }

  private def redirectToSwaggerR: Route = path("" | "/") {
    redirect("/swagger", StatusCodes.PermanentRedirect)
  }
}
