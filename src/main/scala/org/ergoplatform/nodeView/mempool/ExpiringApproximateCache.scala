package org.ergoplatform.nodeView.mempool

import com.google.common.hash.{BloomFilter, Funnels}
import scorex.util.ScorexLogging

import java.nio.charset.Charset
import scala.collection.immutable.TreeMap
import scala.concurrent.duration.FiniteDuration

/**
  * Any Cache that is not accurate but it involves some level of probability (eg. bloom filter based cache)
  * @tparam T type of cache's element
  */
sealed trait ApproximateCacheLike[T] {
  def put(elem: T): ApproximateCacheLike[T]
  def mightContain(elem: T): Boolean
  def approximateElementCount: Long
}

/**
  * Time-based expiring TreeMap in front of size-limited FIFO collection of BloomFilters,
  * so that lower number of elements is tested for presence accurately and huge number of elements only approximately.
  * Bloom filters do not have means of element expiration, so a collection of BloomFilters that gradually expire is a viable alternative.
  * Ie. we expire whole bloom filters instead of expiring elements and we check all bloom filters for element presence
  *
  * @param bloomFilterQueueSize how many bloom filters at maximum to keep in FIFO queue
  * @param bloomFilterQueue fifo collection of bloom filters with tracking index
  * @param frontCacheMaxSize maximum number of elements to keep in cache, following elems are kept in bloom filters
  * @param frontCacheElemExpirationMs for how long to keep elems in cache
  * @param frontCache time expiring cache in front of boom filters
  */
case class ExpiringApproximateCache(
  bloomFilterQueueSize: Int,
  bloomFilterQueue: Vector[(Long, BloomFilter[String])],
  frontCacheMaxSize: Int,
  frontCacheElemExpirationMs: Long,
  frontCache: TreeMap[String, Long]
) extends ApproximateCacheLike[String] with ScorexLogging {

  require(frontCacheMaxSize > 0)

  private def createNewFilter: BloomFilter[String] =
    BloomFilter.create[String](
      Funnels.stringFunnel(Charset.forName("UTF-8")),
      frontCacheMaxSize,
      0.001d   // 0.1 % false positive rate
    )

  /**
    * Puts an element into this Cache.
    * Ensures that subsequent invocations of mightContain with the same element will always return True
    * @return new copy of this instance
    */
  override def put(elem: String): ExpiringApproximateCache = {
    val now = System.currentTimeMillis()
    var updatedCache = frontCache
    var updatedFilters = bloomFilterQueue

    if (frontCache.size == frontCacheMaxSize) {
      // if the cache is full, we clear expired records
      val filteredCache = frontCache.filter(kv => now - kv._2 < frontCacheElemExpirationMs)

      // if cache more than half-full still, move all the elements to a Bloom filter
      if (filteredCache.size > frontCacheMaxSize / 2) {
        log.debug(s"Reset front cache, ${filteredCache.size} elements will be put into Bloom filter")
        val bf = createNewFilter
        filteredCache.keysIterator.foreach(bf.put)
        updatedCache = TreeMap.empty

        bloomFilterQueue.headOption match {
          case None =>
            updatedFilters = Vector(0L -> bf)
          case Some((idx, _)) =>
            val normalizedFilters = if (bloomFilterQueue.size >= bloomFilterQueueSize) {
              bloomFilterQueue.dropRight(1)
            } else {
              bloomFilterQueue
            }
            updatedFilters = ((idx + 1) -> bf) +: normalizedFilters
        }
      } else {
        log.debug(s"Cleared expiring front cache, old size ${updatedCache.size}, new size ${filteredCache.size}")
        updatedCache = filteredCache
      }
    }

    log.debug(s"Adding $elem to front cache, its size after operation ${updatedCache.size + 1} ")
    this.copy(frontCache = updatedCache + (elem -> now), bloomFilterQueue = updatedFilters)
  }

  /**
    * Returns True if the element might have been put in this Cache, False if this is definitely not the case.
    */
  override def mightContain(elem: String): Boolean =
    frontCache.contains(elem) || bloomFilterQueue.exists(_._2.mightContain(elem))

  /**
    * Returns an estimate for the total number of distinct elements that have been added to this
    * Bloom filter. This approximation is reasonably accurate if Bloom Filter has not overflown
    */
  override def approximateElementCount: Long =
    frontCache.size + bloomFilterQueue.foldLeft(0L) {
      case (acc, bf) => acc + bf._2.approximateElementCount()
    }
}

object ExpiringApproximateCache {

  private val BloomFiltersNumber = 4 // hard-coded Bloom filters number for all the cache instances

  /**
    * @param frontCacheSize Maximum number of elements to store in front-cache
    * @param frontCacheExpiration Maximum period to keep cached elements before expiration
    */
  def empty(frontCacheSize: Int, frontCacheExpiration: FiniteDuration): ExpiringApproximateCache = {
    ExpiringApproximateCache(
      bloomFilterQueueSize       = BloomFiltersNumber,
      bloomFilterQueue           = Vector.empty,
      frontCacheMaxSize          = frontCacheSize,
      frontCacheElemExpirationMs = frontCacheExpiration.toMillis,
      frontCache                 = TreeMap.empty[String, Long]
    )
  }

}
