package org.ergoplatform.nodeView.mempool

import scorex.util.ScorexLogging

import scala.collection.immutable.TreeMap
import scala.concurrent.duration.FiniteDuration


/**
  * Time-based expiring TreeMap in front of size-limited FIFO collection of BloomFilters,
  * so that lower number of elements is tested for presence accurately and huge number of elements only approximately.
  * Bloom filters do not have means of element expiration, so a collection of BloomFilters that gradually expire is a viable alternative.
  * Ie. we expire whole bloom filters instead of expiring elements and we check all bloom filters for element presence
  *
  * @param frontCacheMaxSize maximum number of elements to keep in cache, following elems are kept in bloom filters
  * @param frontCacheElemExpirationMs for how long to keep elems in cache
  * @param frontCache time expiring cache in front of boom filters
  */
case class ExpiringCache(frontCacheMaxSize: Int,
                         frontCacheElemExpirationMs: Long,
                         frontCache: TreeMap[String, Long]) extends ScorexLogging {

  /**
    * Puts an element into this Cache.
    * Ensures that subsequent invocations of mightContain with the same element will always return True
    * @return new copy of this instance
    */
  def put(elem: String): ExpiringCache = {
    val now = System.currentTimeMillis()
    val updatedCache = frontCache.dropWhile {
      case (_, timestamp) =>
        now - timestamp > frontCacheElemExpirationMs
    }

    if (updatedCache.size > frontCacheMaxSize) {
      log.warn("Invalid modifiers cache overfull")
    }
    this.copy(frontCache = updatedCache + (elem -> now))
  }

  def size: Int = frontCache.size

  /**
    * Returns True if the element might have been put in this Cache, False if this is definitely not the case.
    */
  def contains(elem: String): Boolean =
    frontCache.contains(elem)
}

object ExpiringCache {

  /**
    * @param frontCacheSize Maximum number of elements to store in front-cache
    * @param frontCacheExpiration Maximum period to keep cached elements before expiration
    */
  def empty(frontCacheSize: Int, frontCacheExpiration: FiniteDuration): ExpiringCache = {
    ExpiringCache(
      frontCacheMaxSize          = frontCacheSize,
      frontCacheElemExpirationMs = frontCacheExpiration.toMillis,
      frontCache                 = TreeMap.empty[String, Long]
    )
  }
}
