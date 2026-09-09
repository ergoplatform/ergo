package org.ergoplatform.http

import akka.Done
import akka.actor.{ActorSystem, CoordinatedShutdown}
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Route
import org.ergoplatform.ErgoApp
import scorex.util.ScorexLogging

import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/** Owns REST startup and the binding throughout coordinated shutdown. */
private[ergoplatform] object HttpServerLifecycle extends ScorexLogging {

  def start(route: Route)(bind: Route => Future[ServerBinding])
           (implicit system: ActorSystem): Future[ServerBinding] = {
    implicit val ec = system.dispatcher
    val shutdown = CoordinatedShutdown(system)
    val ownership = new Object
    val accepting = new AtomicBoolean(false)
    var stopping = false
    var acquired: Option[ServerBinding] = None
    var bindingFailed = false
    val selected = Promise[Option[ServerBinding]]()
    val acquisitionTimeout = shutdown.timeout(CoordinatedShutdown.PhaseServiceUnbind) / 2
    // Leave time within the phase for connection cleanup after the hard deadline.
    val drainDeadline = shutdown.timeout(CoordinatedShutdown.PhaseServiceRequestsDone) / 2
    val guardedRoute: Route = context =>
      if (accepting.get()) route(context)
      else context.complete(HttpResponse(StatusCodes.ServiceUnavailable))

    // Register before initiating the bind, including shutdown during pending startup.
    shutdown.addTask(CoordinatedShutdown.PhaseServiceUnbind, "http-unbind") { () =>
      ownership.synchronized {
        stopping = true
        accepting.set(false)
        if (acquired.isDefined || bindingFailed) selected.trySuccess(acquired)
      }
      val cutoff = system.scheduler.scheduleOnce(acquisitionTimeout) {
        ownership.synchronized { selected.trySuccess(None) }
      }
      selected.future.onComplete(_ => cutoff.cancel())
      selected.future.flatMap {
        case Some(server) => server.unbind()
        case None => Future.successful(Done)
      }
    }
    shutdown.addTask(CoordinatedShutdown.PhaseServiceRequestsDone, "http-drain") { () =>
      // Never spend this phase waiting for startup, even if the previous phase timed out.
      val serverToDrain = ownership.synchronized {
        stopping = true
        accepting.set(false)
        selected.trySuccess(acquired)
        selected.future.value.get.get
      }
      serverToDrain match {
        case Some(server) => server.terminate(drainDeadline).map(_ => Done)
        case None => Future.successful(Done)
      }
    }

    Try(bind(guardedRoute)).fold(Future.failed, identity).andThen {
      case Success(server) =>
        val arrivedAfterCutoff = ownership.synchronized {
          acquired = Some(server)
          if (stopping) !selected.trySuccess(acquired)
          else {
            accepting.set(true)
            false
          }
        }
        if (arrivedAfterCutoff) {
          // The closed gate keeps late bindings away from application dependencies.
          server.terminate(1.millis).failed.foreach { error =>
            log.error("Failed to terminate a late REST API binding", error)
          }
        } else log.info(s"REST API bound to ${server.localAddress}")
      case Failure(error) =>
        ownership.synchronized {
          bindingFailed = true
          if (stopping) selected.trySuccess(None)
        }
        log.error("REST API startup failed; shutting down the node", error)
        shutdown.run(ErgoApp.InternalShutdown)
    }
  }
}
