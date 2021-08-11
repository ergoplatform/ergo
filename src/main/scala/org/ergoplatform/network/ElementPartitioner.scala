package org.ergoplatform.network

/**
  * Allows for partitioning elements into arbitrarily sized buckets given min/max limits
  */
object ElementPartitioner {

  /**
    * Evenly distributes elements under unique bucket-type keys given min/max limits
    * @param buckets to distribute elements into
    * @param maxElements maximum elements to fetch
    * @param minElementsPerBucket minimum elements to distribute per bucket
    * @param maxElementsPerBucket maximum elements to distribute per bucket
    * @param fetchMaxElems function that returns elements by type, given a limit (it depends on interpretation of the provider)
    * @return elements evenly grouped under unique bucket-type keys
    */
  def distribute[B, T, I](
    buckets: Iterable[B],
    maxElements: Int,
    minElementsPerBucket: Int,
    maxElementsPerBucket: Int
  )(fetchMaxElems: Int => Map[T, Seq[I]]): Map[(B, T), Seq[I]] = {

    if (buckets.isEmpty) {
      Map.empty
    } else {
      val bucketsCount       = buckets.size
      val maxElementsToFetch = Math.min(maxElements, bucketsCount * maxElementsPerBucket)
      if (maxElementsToFetch <= 0) {
        Map.empty
      } else {
        fetchMaxElems(maxElementsToFetch).foldLeft(Map.empty[(B, T), Seq[I]]) {
          case (acc, (elemType, elements)) =>
            val elementsSize = elements.size
            if (elementsSize < 1) {
              acc
            } else {
              val lessBuckets =
                if (elementsSize / bucketsCount < minElementsPerBucket) {
                  buckets.take(Math.max(elementsSize / minElementsPerBucket, 1)) // there must always be at least one bucket
                } else buckets
              // now let's distribute elements evenly into buckets
              val (quot, rem) =
                (elementsSize / lessBuckets.size, elementsSize % lessBuckets.size)
              val (smaller, bigger) = elements.splitAt(elementsSize - rem * (quot + 1))
              acc ++ lessBuckets
                .zip((smaller.grouped(quot) ++ bigger.grouped(quot + 1)).toSeq)
                .map { case (p, elems) => (p, elemType) -> elems }
            }
        }
      }
    }
  }
}
