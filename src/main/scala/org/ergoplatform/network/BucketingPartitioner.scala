package org.ergoplatform.network

object BucketingPartitioner {

  /**
    * Evenly distributes elements into unique buckets
    * @param buckets to distribute elements into
    * @param maxElements maximum elements to fetch
    * @param maxElementsPerBucket maximum elements to distribute per bucket
    * @param fetchMaxElems function that returns elements by type, given a limit (it depends on interpretation of the provider)
    * @return elements evenly grouped under unique bucket-type keys
    */
  def distribute[B, T, I](buckets: Iterable[B], maxElements: Int, maxElementsPerBucket: Int)
                (fetchMaxElems: Int => Map[T, Seq[I]]): Map[(B, T), Seq[I]] = {

    if (buckets.isEmpty) {
      Map.empty
    } else {
      val bucketsCount = buckets.size
      val maxElementsToFetch = Math.min(maxElements, bucketsCount * maxElementsPerBucket)
      if (maxElementsToFetch <= 0) {
        Map.empty
      } else {
        fetchMaxElems(maxElementsToFetch).foldLeft(Map.empty[(B, T), Seq[I]]) { case (acc, (elemType, elements)) =>
          val elementsSize = elements.size
          if (elementsSize <= bucketsCount) {
            acc ++ buckets.zip(elements.map(_ :: Nil)).map { case (p, elems) => (p, elemType) -> elems }
          } else {
            val (quot, rem) = (elementsSize / bucketsCount, elementsSize % bucketsCount)
            val (smaller, bigger) = elements.splitAt(elementsSize - rem * (quot + 1))
            acc ++ buckets.zip((smaller.grouped(quot) ++ bigger.grouped(quot + 1)).toSeq).map { case (p, elems) => (p, elemType) -> elems }
          }
        }
      }
    }
  }
}
