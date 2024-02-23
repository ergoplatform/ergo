package org.ergoplatform.http

import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.{Directives, Route}
import org.ergoplatform.settings.RESTApiSettings

final case class SwaggerRoute(settings: RESTApiSettings, swaggerConfig: String)
  extends Directives {

  val route: Route =
    swaggerConfR ~
    path("swagger")(getFromResource("swagger-ui/index.html")) ~
    getFromResourceDirectory("swagger-ui")

  private def swaggerConfR: Route = (get & path("api-docs" / "swagger.conf")) {
    complete(HttpEntity(ContentTypes.`application/json`, swaggerConfig))
  }

}
