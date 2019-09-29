package org.ergoplatform.http

import akka.actor.ActorSystem
import akka.http.scaladsl.server.{Directives, Route}

final case class NodePanelRoute()(implicit system: ActorSystem)
  extends Directives {

  val route: Route =
    pathPrefix("panel")(getFromResource("panel/index.html")) ~
      getFromResourceDirectory("panel")
}
