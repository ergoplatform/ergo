package scorex.core

import java.security.SecureRandom
import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

package object utils {

  @tailrec
  final def untilTimeout[T](timeout: FiniteDuration,
                            delay: FiniteDuration = 100.milliseconds)(fn: => T): T = {
    Try {
      fn
    } match {
      case Success(x) => x
      case _ if timeout > delay =>
        Thread.sleep(delay.toMillis)
        untilTimeout(timeout - delay, delay)(fn)
      case Failure(e) => throw e
    }
  }

  def randomBytes(howMany: Int): Array[Byte] = {
    val r = new Array[Byte](howMany)
    new SecureRandom().nextBytes(r) //overrides r
    r
  }

  def concatBytes(seq: Traversable[Array[Byte]]): Array[Byte] = {
    val length: Int = seq.map(_.length).sum
    val result: Array[Byte] = new Array[Byte](length)
    var pos: Int = 0
    seq.foreach { array =>
      System.arraycopy(array, 0, result, pos, array.length)
      pos += array.length
    }
    result
  }

  implicit class MapPimp[K, V](underlying: Map[K, V]) {
    /**
      * One liner for updating a Map with the possibility to handle case of missing Key
      * @param k map key
      * @param f function that is passed Option depending on Key being present or missing, returning new Value
      * @return new Map with value updated under given key
      */
    def adjust(k: K)(f: Option[V] => V): Map[K, V] = underlying.updated(k, f(underlying.get(k)))
  }

  implicit class MapPimpMutable[K, V](underlying: mutable.Map[K, V]) {
    /**
      * One liner for updating a Map with the possibility to handle case of missing Key
      * @param k map key
      * @param f function that is passed Option depending on Key being present or missing, returning new Value
      * @return Option depending on map being updated or not
      */
    def adjust(k: K)(f: Option[V] => V): Option[V] = underlying.put(k, f(underlying.get(k)))

    /**
      * One liner for updating a Map with the possibility to handle case of missing Key
      * @param k map key
      * @param f function that is passed Option depending on Key being present or missing,
      *          returning Option signaling whether to update or not
      * @return new Map with value updated under given key
      */
    def flatAdjust(k: K)(f: Option[V] => Option[V]): Option[V] =
      f(underlying.get(k)) match {
        case None    => None
        case Some(v) => underlying.put(k, v)
      }
  }

}
