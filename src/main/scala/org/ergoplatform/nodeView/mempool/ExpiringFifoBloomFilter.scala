package org.ergoplatform.nodeView.mempool

import com.google.common.hash.{BloomFilter, Funnels}

import java.nio.charset.Charset

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
  * @param fifoQueueSize how many bloom filters at maximum to keep in FIFO queue
  * @param approxElemCountPerBloomFilter approximate element size of a bloom filter
  * @param bloomFilterQueue fifo collection of bloom filters with tracking index
  */
case class ExpiringFifoBloomFilter(
  fifoQueueSize: Int,
  approxElemCountPerBloomFilter: Int,
  bloomFilterQueue: Vector[(Long, BloomFilter[String])]
) extends BloomFilterLike[String] {

  private def createNewFilter =
    BloomFilter.create[String](
      Funnels.stringFunnel(Charset.forName("UTF-8")),
      approxElemCountPerBloomFilter,
      0.05d
    )

  override def put(elem: String): ExpiringFifoBloomFilter =
    bloomFilterQueue.headOption match {
      case None =>
        val bf = createNewFilter
        bf.put(elem)
        this.copy(bloomFilterQueue = Vector(0L -> bf))
      case Some((_, headBf))
          if headBf.approximateElementCount() < approxElemCountPerBloomFilter =>
        headBf.put(elem)
        this
      case Some((idx, _)) =>
        val newIdx = idx + 1
        val bf     = createNewFilter
        bf.put(elem)
        val newFilters =
          if (bloomFilterQueue.size >= fifoQueueSize)
            bloomFilterQueue.dropRight(1)
          else
            bloomFilterQueue
        this.copy(bloomFilterQueue = (newIdx -> bf) +: newFilters)
    }

  override def mightContain(elem: String): Boolean =
    bloomFilterQueue.exists(_._2.mightContain(elem))

  override def approximateElementCount: Long =
    bloomFilterQueue.foldLeft(0L) {
      case (acc, bf) => acc + bf._2.approximateElementCount()
    }
}

object ExpiringFifoBloomFilter {

  def empty(capacity: Int, expirationRate: Double): ExpiringFifoBloomFilter = {
    require(
      expirationRate > 0 && expirationRate < 1,
      "expirationRate must be (0 - 1) exclusive"
    )
    ExpiringFifoBloomFilter(
      fifoQueueSize                 = Math.round(1 / expirationRate).toInt,
      approxElemCountPerBloomFilter = Math.round(capacity * expirationRate).toInt,
      bloomFilterQueue              = Vector.empty
    )
  }
}
