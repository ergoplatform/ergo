package org.ergoplatform.network

import com.google.common.hash.{BloomFilter, Funnels}
import org.ergoplatform.network.FixedSizeApproximateCacheQueue.UnderlyingCache
import java.nio.charset.Charset

/**
  * Any approximate data structure
  * @tparam T type of element
  */
sealed trait ApproximateCacheQueueLike[T] {
  def putAll(elems: Seq[T]): ApproximateCacheQueueLike[T]
  def mightContain(elem: T): Boolean
  def approximateElementCount: Long
}

/**
  * Fixed size queue of caches, each representing something like a block
  * @param cacheQueueSize how many caches at maximum to keep in FIFO queue
  * @param cacheQueue fifo collection of caches
  */
case class FixedSizeApproximateCacheQueue(
  cacheQueueSize: Int,
  cacheQueue: Vector[UnderlyingCache]
) extends ApproximateCacheQueueLike[String] {

  /**
    * Puts elements into underlying caches.
    * Ensures that subsequent invocations of mightContain with the elements will always return True
    * @return new copy of this instance
    */
  override def putAll(elems: Seq[String]): FixedSizeApproximateCacheQueue = {
    val cache = UnderlyingCache.newCache(elems)
    if (cacheQueue.size < cacheQueueSize) {
      this.copy(cacheQueue = cache +: cacheQueue)
    } else {
      this.copy(cacheQueue = cache +: cacheQueue.dropRight(1))
    }
  }

  /**
    * Returns True if the element might have been put in these caches, False if this is definitely not the case.
    */
  override def mightContain(elem: String): Boolean =
    cacheQueue.exists(_.mightContain(elem))

  /**
    * Returns an estimate for the total number of distinct elements that have been added to these
    * caches. This approximation is reasonably accurate if approximate caches have not overflown
    */
  override def approximateElementCount: Long =
    cacheQueue.foldLeft(0L) {
      case (acc, bf) => acc + bf.approximateElementCount
    }
}

object FixedSizeApproximateCacheQueue {
  val elemCountApproxThreshold = 1000

  sealed trait UnderlyingCache {
    def mightContain(elem: String): Boolean
    def approximateElementCount: Long
  }

  object UnderlyingCache {

    def newCache(elems: Seq[String]): UnderlyingCache =
      if (elems.size > elemCountApproxThreshold) {
        ApproxCache.newApproxCache(elems)
      } else {
        ConciseCache.newConciseCache(elems)
      }
  }

  case class ConciseCache(xs: Set[String]) extends UnderlyingCache {
    override def mightContain(elem: String): Boolean = xs.contains(elem)
    override def approximateElementCount: Long       = xs.size
  }

  object ConciseCache {
    def newConciseCache(elems: Seq[String]): ConciseCache = ConciseCache(elems.to[Set])
  }

  case class ApproxCache(bf: BloomFilter[String]) extends UnderlyingCache {
    override def mightContain(elem: String): Boolean = bf.mightContain(elem)
    override def approximateElementCount: Long       = bf.approximateElementCount()
  }

  object ApproxCache {

    def newApproxCache(xs: Seq[String]): ApproxCache = {
      val bf = createNewFilter(xs.size)
      xs.foreach(e => bf.put(e))
      ApproxCache(bf)
    }

    private def createNewFilter(approxElemCount: Int) =
      BloomFilter.create[String](
        Funnels.stringFunnel(Charset.forName("UTF-8")),
        approxElemCount,
        0.05d
      )
  }

  /**
    * Create empty FixedSizeBloomFilterQueue
    * @param cacheQueueSize how many bloom filters at maximum to keep in FIFO queue
    * @return new FixedSizeBloomFilterQueue
    */
  def empty(cacheQueueSize: Int): FixedSizeApproximateCacheQueue =
    FixedSizeApproximateCacheQueue(cacheQueueSize, cacheQueue = Vector.empty)
}
