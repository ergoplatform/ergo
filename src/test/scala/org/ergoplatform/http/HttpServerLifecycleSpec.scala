package org.ergoplatform.http

import akka.Done
import akka.actor.{ActorSystem, CoordinatedShutdown}
import akka.http.scaladsl.Http.{HttpServerTerminated, HttpTerminated, ServerBinding}
import akka.http.scaladsl.model.{HttpRequest, HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.Route
import com.typesafe.config.ConfigFactory
import org.ergoplatform.ErgoApp
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.InetSocketAddress
import java.util.UUID
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration._

class HttpServerLifecycleSpec extends AnyFlatSpec with Matchers {

  private def withSystem(test: ActorSystem => Unit): Unit = {
    withSystemConfig("")(test)
  }

  private def withSystemConfig(overrides: String)(test: ActorSystem => Unit): Unit = {
    val config = ConfigFactory.parseString(overrides).withFallback(ConfigFactory.parseString("""
      akka.coordinated-shutdown {
        terminate-actor-system = off
        run-by-actor-system-terminate = off
        run-by-jvm-shutdown-hook = off
        default-phase-timeout = 5s
      }
    """)).withFallback(ConfigFactory.load())
    val system = ActorSystem(s"http-lifecycle-${UUID.randomUUID()}", config)
    try test(system)
    finally Await.result(system.terminate(), 5.seconds)
  }

  private def start(bind: => Future[ServerBinding])(implicit system: ActorSystem): Future[ServerBinding] =
    HttpServerLifecycle.start(complete(HttpResponse()))(_ => bind)

  "HTTP lifecycle" should "retain a successful binding and drain its response before dependencies stop" in withSystem { implicit system =>
    implicit val ec = system.dispatcher
    val shutdown = CoordinatedShutdown(system)
    val walletOpen = new AtomicBoolean(true)
    val unbound = Promise[Unit]()
    val draining = Promise[FiniteDuration]()
    val response = Promise[HttpResponse]()
    val delivered = Promise[HttpResponse]()
    val admitted = Promise[Done]()
    val handler = Promise[HttpRequest => Future[HttpResponse]]()
    val walletClosed = Promise[Done]()
    val binding = ServerBinding(new InetSocketAddress("127.0.0.1", 0))(
      () => { unbound.trySuccess(()); Future.successful(()) },
      deadline => {
        draining.success(deadline)
        delivered.future.map(_ => HttpServerTerminated: HttpTerminated)
      }
    )
    shutdown.addTask(CoordinatedShutdown.PhaseServiceStop, "close-wallet") { () =>
      walletOpen.set(false)
      walletClosed.success(Done)
      Future.successful(Done)
    }

    val route: Route = complete {
      admitted.trySuccess(Done)
      response.future.map { result =>
        walletOpen.get() shouldBe true
        result
      }
    }
    val startup = HttpServerLifecycle.start(route) { guarded =>
      handler.success(Route.toFunction(guarded))
      Future.successful(binding)
    }
    Await.result(startup, 5.seconds) shouldBe binding
    val serve = Await.result(handler.future, 5.seconds)
    delivered.completeWith(serve(HttpRequest()))
    Await.result(admitted.future, 5.seconds) shouldBe Done
    unbound.isCompleted shouldBe false
    val stopped = shutdown.run(ErgoApp.RemoteShutdown)
    val deadline = Await.result(draining.future, 5.seconds)
    unbound.isCompleted shouldBe true
    deadline should be > Duration.Zero
    deadline should be < shutdown.timeout(CoordinatedShutdown.PhaseServiceRequestsDone)
    walletClosed.isCompleted shouldBe false
    stopped.isCompleted shouldBe false
    Await.result(serve(HttpRequest()), 5.seconds).status shouldBe StatusCodes.ServiceUnavailable

    // An admitted ordinary response can still read its dependency during the drain.
    walletOpen.get() shouldBe true
    response.success(HttpResponse(entity = "wallet response"))
    Await.result(delivered.future, 5.seconds).status shouldBe StatusCodes.OK
    Await.result(stopped, 5.seconds) shouldBe Done
    Await.result(walletClosed.future, 5.seconds) shouldBe Done
    Await.result(binding.whenTerminated, 5.seconds) shouldBe HttpServerTerminated
  }

  it should "shut down already started resources after an asynchronous bind failure" in withSystem { implicit system =>
    val shutdown = CoordinatedShutdown(system)
    val resourceClosed = Promise[Done]()
    shutdown.addTask(CoordinatedShutdown.PhaseServiceStop, "close-resource") { () =>
      resourceClosed.success(Done)
      Future.successful(Done)
    }
    val pending = Promise[ServerBinding]()
    val startup = start(pending.future)
    val failure = new IllegalStateException("binding unavailable")
    pending.failure(failure)

    intercept[IllegalStateException](Await.result(startup, 5.seconds)) shouldBe failure
    Await.result(resourceClosed.future, 5.seconds) shouldBe Done
    shutdown.shutdownReason() shouldBe Some(ErgoApp.InternalShutdown)
  }

  it should "own an exception thrown while initiating the binding" in withSystem { implicit system =>
    val shutdown = CoordinatedShutdown(system)
    val resourceClosed = Promise[Done]()
    shutdown.addTask(CoordinatedShutdown.PhaseServiceStop, "close-resource") { () =>
      resourceClosed.success(Done)
      Future.successful(Done)
    }
    val failure = new IllegalArgumentException("invalid binding configuration")
    val startup = start(throw failure)

    intercept[IllegalArgumentException](Await.result(startup, 5.seconds)) shouldBe failure
    Await.result(resourceClosed.future, 5.seconds) shouldBe Done
    shutdown.shutdownReason() shouldBe Some(ErgoApp.InternalShutdown)
  }

  it should "include a pending binding when shutdown starts before startup completes" in withSystem { implicit system =>
    val shutdown = CoordinatedShutdown(system)
    val pending = Promise[ServerBinding]()
    val shutdownStarted = Promise[Done]()
    val terminated = Promise[Done]()
    shutdown.addTask(CoordinatedShutdown.PhaseBeforeServiceUnbind, "shutdown-started") { () =>
      shutdownStarted.success(Done)
      Future.successful(Done)
    }
    val startup = start(pending.future)
    val stopped = shutdown.run(ErgoApp.RemoteShutdown)
    Await.result(shutdownStarted.future, 5.seconds)
    val binding = ServerBinding(new InetSocketAddress("127.0.0.1", 0))(
      () => Future.successful(()),
      _ => { terminated.success(Done); Future.successful(HttpServerTerminated) }
    )
    pending.success(binding)

    Await.result(startup, 5.seconds) shouldBe binding
    Await.result(stopped, 5.seconds) shouldBe Done
    Await.result(terminated.future, 5.seconds) shouldBe Done
  }

  it should "deny application requests until binding ownership is established" in withSystem { implicit system =>
    val pending = Promise[ServerBinding]()
    val handler = Promise[HttpRequest => Future[HttpResponse]]()
    val calls = new AtomicInteger(0)
    val route: Route = complete {
      calls.incrementAndGet()
      HttpResponse()
    }
    val startup = HttpServerLifecycle.start(route) { guarded =>
      handler.success(Route.toFunction(guarded))
      pending.future
    }
    val serve = Await.result(handler.future, 5.seconds)
    Await.result(serve(HttpRequest()), 5.seconds).status shouldBe StatusCodes.ServiceUnavailable
    calls.get() shouldBe 0
    val binding = ServerBinding(new InetSocketAddress("127.0.0.1", 0))(
      () => Future.successful(()), _ => Future.successful(HttpServerTerminated))
    pending.success(binding)
    Await.result(startup, 5.seconds) shouldBe binding
    Await.result(serve(HttpRequest()), 5.seconds).status shouldBe StatusCodes.OK
    calls.get() shouldBe 1
    Await.result(CoordinatedShutdown(system).run(ErgoApp.RemoteShutdown), 5.seconds) shouldBe Done
  }

  it should "terminate a binding arriving after the acquisition window without admitting dependency calls" in
    withSystemConfig("akka.coordinated-shutdown.default-phase-timeout = 400ms") { implicit system =>
      val pending = Promise[ServerBinding]()
      val handler = Promise[HttpRequest => Future[HttpResponse]]()
      val calls = new AtomicInteger(0)
      val stopped = Promise[Done]()
      val terminated = Promise[FiniteDuration]()
      val shutdown = CoordinatedShutdown(system)
      val route: Route = complete {
        calls.incrementAndGet()
        HttpResponse()
      }
      val startup = HttpServerLifecycle.start(route) { guarded =>
        handler.success(Route.toFunction(guarded))
        pending.future
      }
      shutdown.addTask(CoordinatedShutdown.PhaseServiceStop, "stop-dependencies") { () =>
        stopped.success(Done)
        Future.successful(Done)
      }
      Await.result(shutdown.run(ErgoApp.RemoteShutdown), 5.seconds) shouldBe Done
      Await.result(stopped.future, 5.seconds) shouldBe Done
      val binding = ServerBinding(new InetSocketAddress("127.0.0.1", 0))(
        () => Future.successful(()),
        deadline => { terminated.success(deadline); Future.successful(HttpServerTerminated) })
      pending.success(binding)
      Await.result(startup, 5.seconds) shouldBe binding
      Await.result(terminated.future, 5.seconds) should be > Duration.Zero
      val serve = Await.result(handler.future, 5.seconds)
      Await.result(serve(HttpRequest()), 5.seconds).status shouldBe StatusCodes.ServiceUnavailable
      calls.get() shouldBe 0
    }
}
