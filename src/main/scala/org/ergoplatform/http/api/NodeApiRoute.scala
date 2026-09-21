import akka.actor.ActorRef
import akka.pattern.ask
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.RollbackToHeight
import scorex.util.ModifierId
import scala.util.Try

case class NodeApiRoute(ergoSettings: ErgoSettings, nodeViewActorRef: ActorRef)(implicit system: ActorSystem, val context: ActorRefFactory) extends ErgoBaseApiRoute {

  val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  override val route: Route = (pathPrefix("node") & withAuth) {
      shutdown ~ forceRollback
    }

  private val shutdownDelay = 5.seconds

  private def shutdown: Route = (pathPrefix("shutdown") & post) {
    system.scheduler.scheduleOnce(shutdownDelay)(ErgoApp.shutdownSystem(RemoteShutdown))
    ApiResponse(s"The node will be shut down in $shutdownDelay")
  }
  
  private def forceRollback: Route = (pathPrefix("forceRollback") & post & entity(as[Json])) { json =>
    json.hcursor.downField("height").as[Int] match {
      case Right(height) =>
         val result = (nodeViewActorRef ? RollbackToHeight(height)).mapTo[Try[ModifierId]]
         ApiResponse(result.map(_.map(id => Json.obj("rolledBackTo" -> id.asJson))))
      case Left(e) =>
         ApiError.BadRequest(s"Bad request: $e")
    }
  }
}