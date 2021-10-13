package org.ergoplatform.nodeView.mempool

import com.google.common.hash.{BloomFilter, Funnels}

import java.nio.charset.Charset
import scala.collection.immutable.TreeMap
import scala.concurrent.duration.FiniteDuration

sealed trait BloomFilterLike[T] {
  def put(elem: T): ExpiringFifoBloomFilter
  def mightContain(elem: T): Boolean
  def approximateElementCount: Long
}

/**
  * Size-limited FIFO collection of BloomFilters which do not have means of element expiration,
  * so a collection of BloomFilters that gradually expire is a viable alternative.
  * Ie. we expire whole bloom filters instead of expiring elements and we check all bloom filters for element presence
  *
  * @param bloomFilterQueueSize how many bloom filters at maximum to keep in FIFO queue
  * @param bloomFilterApproxElemCount approximate element size of a single bloom filter
  * @param bloomFilterQueue fifo collection of bloom filters with tracking index
  */
case class ExpiringFifoBloomFilter(
  bloomFilterQueueSize: Int,
  bloomFilterApproxElemCount: Int,
  bloomFilterQueue: Vector[(Long, BloomFilter[String])],
  frontCacheMaxSize: Int,
  frontCacheElemExpirationMs: Long,
  frontCache: TreeMap[String, Long]
) extends BloomFilterLike[String] {

  private def createNewFilter =
    BloomFilter.create[String](
      Funnels.stringFunnel(Charset.forName("UTF-8")),
      bloomFilterApproxElemCount,
      0.05d
    )

  /**
    * Puts an element into this BloomFilter.
    * Ensures that subsequent invocations of mightContain with the same element will always return
    * @return new copy of this instance
    */
  override def put(elem: String): ExpiringFifoBloomFilter = {
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
    * Returns True if the element might have been put in this Bloom filter, False if this is definitely not the case.
    */
  override def mightContain(elem: String): Boolean =
    frontCache.contains(elem) || bloomFilterQueue.exists(_._2.mightContain(elem))

  /**
    * Returns an estimate for the total number of distinct elements that have been added to this
    * Bloom filter. This approximation is reasonably accurate if Bloom Filter has not overflown
    */
  override def approximateElementCount: Long =
    bloomFilterQueue.foldLeft(0L) {
      case (acc, bf) => acc + bf._2.approximateElementCount()
    }
}

object ExpiringFifoBloomFilter {

  /**
    * @param bloomFilterCapacity Maximum number of elements to store in bloom filters
    * @param bloomFilterExpirationRate fraction of 1 that represents a rate of expiration as the cache grows in size,
    *                       only values from open interval (0,1) are valid
    * @param cacheSize Maximum number of elements to store in cache
    * @param cacheExpiration Maximum period to keep cached elements before expiration
    */
  def empty(
    bloomFilterCapacity: Int,
    bloomFilterExpirationRate: Double,
    cacheSize: Int,
    cacheExpiration: FiniteDuration
  ): ExpiringFifoBloomFilter = {
    require(
      bloomFilterExpirationRate > 0 && bloomFilterExpirationRate < 1,
      "expirationRate must be (0 - 1) exclusive"
    )
    ExpiringFifoBloomFilter(
      bloomFilterQueueSize = Math.round(1 / bloomFilterExpirationRate).toInt,
      bloomFilterApproxElemCount =
        Math.round(bloomFilterCapacity * bloomFilterExpirationRate).toInt,
      bloomFilterQueue           = Vector.empty,
      frontCacheMaxSize          = cacheSize,
      frontCacheElemExpirationMs = cacheExpiration.toMillis,
      frontCache                 = TreeMap.empty[String, Long]
    )
  }
}
