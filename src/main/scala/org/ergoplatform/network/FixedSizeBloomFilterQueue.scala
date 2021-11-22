package org.ergoplatform.network

import com.google.common.hash.{BloomFilter, Funnels}

import java.nio.charset.Charset

/**
  * Any approximate data structure
  * @tparam T type of element
  */
sealed trait BloomFilterLike[T] {
  def putAll(elems: Seq[T]): BloomFilterLike[T]
  def mightContain(elem: T): Boolean
  def approximateElementCount: Long
}

/**
  * Fixed size queue of bloomfilters
  * @param bloomFilterQueueSize how many bloom filters at maximum to keep in FIFO queue
  * @param bloomFilterQueue fifo collection of bloom filters
  */
case class FixedSizeBloomFilterQueue(
  bloomFilterQueueSize: Int,
  bloomFilterQueue: Vector[BloomFilter[String]]
) extends BloomFilterLike[String] {

  private def createNewFilter(approxElemCount: Int) =
    BloomFilter.create[String](
      Funnels.stringFunnel(Charset.forName("UTF-8")),
      approxElemCount,
      0.05d
    )

  /**
    * Puts elements into underlying bloomfilters.
    * Ensures that subsequent invocations of mightContain with the elements will always return True
    * @return new copy of this instance
    */
  override def putAll(elems: Seq[String]): FixedSizeBloomFilterQueue = {
    val bf = createNewFilter(elems.size)
    elems.foreach(e => bf.put(e))
    if (bloomFilterQueue.size < bloomFilterQueueSize) {
      this.copy(bloomFilterQueue = bf +: bloomFilterQueue)
    } else {
      this.copy(bloomFilterQueue = bf +: bloomFilterQueue.dropRight(1))
    }
  }

  /**
    * Returns True if the element might have been put in these bloom filters, False if this is definitely not the case.
    */
  override def mightContain(elem: String): Boolean =
    bloomFilterQueue.exists(_.mightContain(elem))

  /**
    * Returns an estimate for the total number of distinct elements that have been added to these
    * Bloom filters. This approximation is reasonably accurate if Bloom Filters have not overflown
    */
  override def approximateElementCount: Long =
    bloomFilterQueue.foldLeft(0L) {
      case (acc, bf) => acc + bf.approximateElementCount()
    }
}

object FixedSizeBloomFilterQueue {
  /**
    * Create empty FixedSizeBloomFilterQueue
    * @param bloomFilterQueueSize how many bloom filters at maximum to keep in FIFO queue
    * @return new FixedSizeBloomFilterQueue
    */
  def empty(bloomFilterQueueSize: Int): FixedSizeBloomFilterQueue =
    FixedSizeBloomFilterQueue(bloomFilterQueueSize, bloomFilterQueue = Vector.empty)
}
