package org.ergoplatform.it

import io.netty.util.Timer

import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}
import scala.concurrent.{ExecutionContext, Future, Promise}

package object util {
  implicit class TimerExt(val timer: Timer) extends AnyVal {
    def schedule[A](f: => Future[A], delay: FiniteDuration): Future[A] = {
      val p = Promise[A]
      try {
        timer.newTimeout(_ => p.completeWith(f), delay.toMillis, MILLISECONDS)
      } catch {
        case t: Throwable => p.failure(t)
      }
      p.future
    }

    def retryUntil[A](f: => Future[A], cond: A => Boolean, retryInterval: FiniteDuration)
                     (implicit ec: ExecutionContext): Future[A] =
      f.flatMap(v => if (cond(v)) Future.successful(v) else schedule(retryUntil(f, cond, retryInterval), retryInterval))
  }
  implicit class RichEither[A](e: Either[String, A]) {
    def toFuture: Future[A] = Future.fromTry(e.left.map(new Exception(_)).toTry)

    def get: A = e.fold(e => throw new Exception(e), identity)
  }
}
