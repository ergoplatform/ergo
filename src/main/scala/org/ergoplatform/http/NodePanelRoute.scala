package org.ergoplatform.http

import akka.http.scaladsl.server.{Directives, Route}

final case class NodePanelRoute() extends Directives {

  val route: Route =
    pathPrefix("panel")(getFromResource("panel/index.html")) ~
      getFromResourceDirectory("panel")
}
