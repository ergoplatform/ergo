package org.ergoplatform

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpMethods, HttpRequest}
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer
import akka.util.ByteString
import io.circe.generic.auto._
import io.circe.parser._
import org.ergoplatform.BootstrapController.GenesisSettings
import org.ergoplatform.settings.BootstrapSettings
import scorex.util.ScorexLogging

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

final class BootstrapController(settings: BootstrapSettings)(implicit val as: ActorSystem)
  extends ScorexLogging {

  implicit val ec: ExecutionContext = as.getDispatcher
  implicit val mat: ActorMaterializer = ActorMaterializer()

  def waitForBootSettings(): (Seq[String], String) = {
    def tryFetchSettings: GenesisSettings = {
      Try(Await.result(getSettings(settings.resourceUri), atMost = 5.second)) match {
        case Success(Some(genesisSettings)) => genesisSettings
        case Success(_) =>
          log.info(s"Wrong response format, retrying in ${settings.pollDelay.toSeconds}s")
          Thread.sleep(settings.pollDelay.toMillis)
          tryFetchSettings
        case Failure(e) =>
          log.info(s"Failed to fetch genesis settings: ${e.getMessage}. Retrying in ${settings.pollDelay.toSeconds}s")
          Thread.sleep(settings.pollDelay.toMillis)
          tryFetchSettings
      }
    }
    tryFetchSettings.unapply
  }

  private def getSettings(uri: String): Future[Option[GenesisSettings]] =
    Http().singleRequest(HttpRequest(method = HttpMethods.GET, uri = uri))
      .flatMap { resp =>
        Unmarshal(resp.entity)
          .to[ByteString]
          .map(bs => decode[GenesisSettings](bs.utf8String).toOption)
      }

}

object BootstrapController {

  final case class GenesisSettings(noPremineProof: Seq[String],
                                   genesisStateDigestHex: String) {
    def unapply: (Seq[String], String) = noPremineProof -> genesisStateDigestHex
  }

}
