package scorex.core

import java.security.SecureRandom
import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

package object utils {

  @deprecated("Use scorex.util.ScorexLogging instead.", "scorex-util 0.1.0")
  type ScorexLogging = scorex.util.ScorexLogging

  /**
    * @param block - function to profile
    * @return - execution time in seconds and function result
    */
  def profile[R](block: => R): (Float, R) = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    ((t1 - t0).toFloat / 1000000000, result)
  }

  def toTry(b: Boolean, msg: String): Try[Unit] = b match {
    case true => Success(Unit)
    case false => Failure(new Exception(msg))
  }

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

  def concatFixLengthBytes(seq: Traversable[Array[Byte]]): Array[Byte] = seq.headOption match {
    case None => Array[Byte]()
    case Some(head) => concatFixLengthBytes(seq, head.length)
  }


  def concatFixLengthBytes(seq: Traversable[Array[Byte]], length: Int): Array[Byte] = {
    val result: Array[Byte] = new Array[Byte](seq.toSeq.length * length)
    var index = 0
    seq.foreach { s =>
      Array.copy(s, 0, result, index, length)
      index += length
    }
    result
  }


  implicit class MapPimp[K, V](underlying: mutable.Map[K, V]) {
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
