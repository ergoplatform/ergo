package org.ergoplatform.nodeView.mempool

import com.google.common.hash.{BloomFilter, Funnels}
import java.nio.charset.Charset
import scala.collection.immutable.TreeMap
import scala.concurrent.duration.FiniteDuration

sealed trait ApproximateCacheLike[T] {
  def put(elem: T): ExpiringApproximateCache
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
  * @param bloomFilterApproxElemCount approximate element size of a single bloom filter
  * @param bloomFilterQueue fifo collection of bloom filters with tracking index
  * @param frontCacheMaxSize maximum number of elements to keep in cache, following elems are kept in bloom filters
  * @param frontCacheElemExpirationMs for how long to keep elems in cache
  * @param frontCache time expiring cache in front of boom filters
  */
case class ExpiringApproximateCache(
  bloomFilterQueueSize: Int,
  bloomFilterApproxElemCount: Int,
  bloomFilterQueue: Vector[(Long, BloomFilter[String])],
  frontCacheMaxSize: Int,
  frontCacheElemExpirationMs: Long,
  frontCache: TreeMap[String, Long]
) extends ApproximateCacheLike[String] {

  private def createNewFilter =
    BloomFilter.create[String](
      Funnels.stringFunnel(Charset.forName("UTF-8")),
      bloomFilterApproxElemCount,
      0.05d
    )

  /**
    * Puts an element into this Cache.
    * Ensures that subsequent invocations of mightContain with the same element will always return True
    * @return new copy of this instance
    */
  override def put(elem: String): ExpiringApproximateCache = {
    val now = System.currentTimeMillis()
    val updatedCache = frontCache.dropWhile {
      case (_, timestamp) =>
        now - timestamp > frontCacheElemExpirationMs
    }

    if (updatedCache.size < frontCacheMaxSize) {
      this.copy(frontCache = updatedCache + (elem -> now))
    } else {
      bloomFilterQueue.headOption match {
        case None =>
          val bf = createNewFilter
          bf.put(elem)
          this.copy(bloomFilterQueue = Vector(0L -> bf))
        case Some((_, headBf))
            if headBf.approximateElementCount() < bloomFilterApproxElemCount =>
          headBf.put(elem)
          this
        case Some((idx, _)) =>
          val newIdx = idx + 1
          val bf     = createNewFilter
          bf.put(elem)
          val newFilters =
            if (bloomFilterQueue.size >= bloomFilterQueueSize)
              bloomFilterQueue.dropRight(1)
            else
              bloomFilterQueue
          this.copy(bloomFilterQueue = (newIdx -> bf) +: newFilters)
      }
    }
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

  /**
    * @param bloomFilterCapacity Maximum number of elements to store in bloom filters
    * @param bloomFilterExpirationRate fraction of 1 that represents a rate of expiration as the cache grows in size,
    *                       only values from open interval (0,1) are valid
    * @param frontCacheSize Maximum number of elements to store in front-cache
    * @param frontCacheExpiration Maximum period to keep cached elements before expiration
    */
  def empty(
             bloomFilterCapacity: Int,
             bloomFilterExpirationRate: Double,
             frontCacheSize: Int,
             frontCacheExpiration: FiniteDuration
  ): ExpiringApproximateCache = {
    require(
      bloomFilterExpirationRate > 0 && bloomFilterExpirationRate < 1,
      "expirationRate must be (0 - 1) exclusive"
    )
    ExpiringApproximateCache(
      bloomFilterQueueSize = Math.round(1 / bloomFilterExpirationRate).toInt,
      bloomFilterApproxElemCount =
        Math.round(bloomFilterCapacity * bloomFilterExpirationRate).toInt,
      bloomFilterQueue           = Vector.empty,
      frontCacheMaxSize          = frontCacheSize,
      frontCacheElemExpirationMs = frontCacheExpiration.toMillis,
      frontCache                 = TreeMap.empty[String, Long]
    )
  }
}
