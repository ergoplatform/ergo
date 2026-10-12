package org.ergoplatform.it.util

import java.util.concurrent.{ScheduledThreadPoolExecutor, ThreadFactory, TimeUnit, TimeoutException}
import org.ergoplatform.it.api.NodeApi.NodeInfo

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/** Bounded, single-flight observations for integration assertions. */
final class ConvergenceObservations(implicit ec: ExecutionContext) extends AutoCloseable {
  private val timer = new ScheduledThreadPoolExecutor(1, new ThreadFactory {
    override def newThread(runnable: Runnable): Thread = {
      val thread = new Thread(runnable, "convergence-observations")
      thread.setDaemon(true)
      thread
    }
  })
  timer.setRemoveOnCancelPolicy(true)

  private def bounded[A](future: Future[A], budget: FiniteDuration): Future[A] = {
    val result = Promise[A]()
    val timeout = timer.schedule(new Runnable {
      override def run(): Unit = result.tryFailure(new TimeoutException("Observation deadline"))
    }, math.max(0L, budget.toNanos), TimeUnit.NANOSECONDS)
    future.onComplete { value =>
      result.tryComplete(value)
      timeout.cancel(false)
    }
    result.future
  }

  final class Probe[A](request: () => Future[A]) {
    private var pending: Option[Future[A]] = None

    def sample(budget: FiniteDuration): Future[Either[String, A]] = {
      val response = synchronized {
        val current = pending.filterNot(_.isCompleted).getOrElse {
          Try(request()) match {
            case Success(value) => value
            case Failure(error) => Future.failed(error)
          }
        }
        pending = Some(current)
        current
      }
      bounded(response, budget).map(value => Right(value): Either[String, A]).recover {
        case scala.util.control.NonFatal(error) => Left(error.getClass.getSimpleName)
      }
    }
  }

  def probe[A](request: => Future[A]): Probe[A] = new Probe(() => request)

  def until[A](deadline: Deadline, interval: FiniteDuration, sampleBudget: FiniteDuration)
              (observe: FiniteDuration => Future[A])(accept: A => Boolean)
              (failure: => String): Future[A] = {
    def expired: Future[A] = Future.failed(new TimeoutException(failure))

    def loop(): Future[A] = {
      if (deadline.isOverdue()) expired
      else {
        val remaining = deadline.timeLeft
        bounded(observe(sampleBudget.min(remaining)), remaining).flatMap { value =>
          if (deadline.isOverdue()) expired
          else if (accept(value)) Future.successful(value)
          else {
            val next = Promise[Unit]()
            timer.schedule(new Runnable {
              override def run(): Unit = next.trySuccess(())
            }, interval.min(deadline.timeLeft).max(Duration.Zero).toNanos, TimeUnit.NANOSECONDS)
            next.future.flatMap(_ => loop())
          }
        }.recoverWith {
          case _: TimeoutException => expired
        }
      }
    }

    loop()
  }

  override def close(): Unit = timer.shutdownNow()
}

object ConvergenceObservations {
  def sameBestBlock(infoA: NodeInfo, infoB: NodeInfo, minHeight: Int): Boolean = {
    val sameHeight = infoA.bestBlockHeightOpt.nonEmpty && infoA.bestBlockHeightOpt == infoB.bestBlockHeightOpt
    val sameBlock = infoA.bestBlockIdOpt.nonEmpty && infoA.bestBlockIdOpt == infoB.bestBlockIdOpt
    val highEnough = infoA.bestBlockHeightOpt.exists(_ >= minHeight)
    sameHeight && sameBlock && highEnough
  }

  def selectedHeadersAgree(headers: Seq[Seq[String]]): Boolean =
    headers.nonEmpty && headers.forall(_.headOption.exists(_.nonEmpty)) &&
      headers.map(_.head).distinct.size == 1

  def headerId(value: String): String =
    if (value.matches("[0-9a-fA-F]{64}")) value else "invalid-header-id"
}
