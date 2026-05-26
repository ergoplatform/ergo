package org.ergoplatform.http.api

import akka.actor.{ActorRefFactory, ActorSystem}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import io.circe.syntax._
import org.ergoplatform.ErgoApp
import org.ergoplatform.ErgoApp.RemoteShutdown
import org.ergoplatform.http.api.NodeConfigCodecs._
import org.ergoplatform.settings.{ErgoSettings, PersistError, RESTApiSettings, SettingsHolder}
import scorex.core.api.http.ApiResponse

import scala.concurrent.duration._

case class NodeApiRoute(ergoSettings: ErgoSettings,
                        settingsHolder: SettingsHolder)
                       (implicit system: ActorSystem, val context: ActorRefFactory) extends ErgoBaseApiRoute {

  val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  override val route: Route = (pathPrefix("node") & withAuth) {
    shutdown ~ getConfig ~ putConfig
  }

  private val shutdownDelay = 5.seconds

  private def shutdown: Route = (path("shutdown") & post) {
    system.scheduler.scheduleOnce(shutdownDelay)(ErgoApp.shutdownSystem(RemoteShutdown))
    ApiResponse(s"The node will be shut down in $shutdownDelay")
  }

  private def getConfig: Route = (path("config") & get) {
    ApiResponse(NodeConfigView.from(settingsHolder.current).asJson)
  }

  private def putConfig: Route = (path("config") & put & entity(as[NodeConfigPatch])) { patch =>
    val candidate = applyPatch(settingsHolder.current, patch)
    settingsHolder.trySwap(candidate) match {
      case Right(updated) =>
        ApiResponse(NodeConfigView.from(updated).asJson)
      case Left(PersistError.NoWritableConfig) =>
        ApiError(StatusCodes.Conflict, "no.writable.config")(PersistError.NoWritableConfig.message)
      case Left(e: PersistError.ConfigFileUnsupported) =>
        ApiError(StatusCodes.Conflict, "config.file.unsupported")(e.message)
      case Left(e: PersistError.IoFailure) =>
        ApiError.InternalError(e.message)
    }
  }
}
