package org.ergoplatform.network

/**
  * Allows for partitioning elements into arbitrarily sized buckets given min/max limits
  */
object ElementPartitioner {

  /**
    * Evenly distributes elements under unique bucket-type keys given min/max limits,
    * considering which buckets can serve which elements.
    *
    * @param buckets to distribute elements into
    * @param minElementsPerBucket minimum elements to distribute per bucket
    * @param fetchedElems elements to distribute over the buckets
    * @param canServe predicate checking whether a bucket can serve an element of a given type
    * @return elements evenly grouped under unique bucket-type keys
    */
  def distribute[B, T, I](buckets: Iterable[B],
                          minElementsPerBucket: Int,
                          fetchedElems: Map[T, Seq[I]],
                          canServe: (B, T, I) => Boolean = (_: B, _: T, _: I) => true): Map[(B, T), Seq[I]] = {
    val bucketsSeq = buckets.toSeq
    val bucketsCount = bucketsSeq.size
    if (bucketsCount == 0) {
      Map.empty
    } else {
      fetchedElems.foldLeft(Map.empty[(B, T), Seq[I]]) {
        case (acc, (elemType, elements)) =>
          val servableElements = elements.filter(e => bucketsSeq.exists(b => canServe(b, elemType, e)))
          val elementsSize = servableElements.size
          if (elementsSize < 1) {
            acc
          } else {
            val universalBuckets = bucketsSeq.filter(b => servableElements.forall(e => canServe(b, elemType, e)))
            val allBucketsUniversal = universalBuckets.size == bucketsCount
            if (allBucketsUniversal) {
              val lessBuckets =
                if (elementsSize / bucketsCount < minElementsPerBucket) {
                  bucketsSeq.take(Math.max(elementsSize / Math.max(minElementsPerBucket, 1), 1)) // there must always be at least one bucket
                } else {
                  bucketsSeq
                }
              // now let's distribute elements evenly into buckets
              val (quot, rem) =
                (elementsSize / lessBuckets.size, elementsSize % lessBuckets.size)
              val (smaller, bigger) = servableElements.splitAt(elementsSize - rem * (quot + 1))
              acc ++ lessBuckets
                .zip((smaller.grouped(quot) ++ bigger.grouped(quot + 1)).toSeq)
                .map { case (p, elems) => (p, elemType) -> elems }
            } else {
              val desiredMaxBuckets =
                if (minElementsPerBucket > 0 && elementsSize / bucketsCount < minElementsPerBucket) {
                  Math.max(elementsSize / minElementsPerBucket, 1)
                } else {
                  bucketsCount
                }
              val candidateBuckets =
                if (universalBuckets.nonEmpty && desiredMaxBuckets <= universalBuckets.size) {
                  universalBuckets.take(desiredMaxBuckets)
                } else {
                  bucketsSeq.filter(b => servableElements.exists(e => canServe(b, elemType, e)))
                }

              val indexedElements = servableElements.zipWithIndex
              val orderedByConstraint = indexedElements.sortBy { case (elem, idx) =>
                (candidateBuckets.count(b => canServe(b, elemType, elem)), idx)
              }

              val assignments = orderedByConstraint.foldLeft(Map.empty[B, Vector[(I, Int)]]) {
                case (assignedMap, (elem, idx)) =>
                  val eligible = candidateBuckets.filter(b => canServe(b, elemType, elem))
                  eligible.sortBy(b => assignedMap.getOrElse(b, Vector.empty).size).headOption match {
                    case Some(chosenBucket) =>
                      val current = assignedMap.getOrElse(chosenBucket, Vector.empty)
                      assignedMap.updated(chosenBucket, current :+ (elem -> idx))
                    case None =>
                      assignedMap
                  }
              }

              val consolidated =
                if (minElementsPerBucket > 1 && assignments.size > 1) {
                  val underfullBuckets = candidateBuckets.filter { b =>
                    val sz = assignments.getOrElse(b, Vector.empty).size
                    sz > 0 && sz < minElementsPerBucket
                  }
                  underfullBuckets.foldLeft(assignments) { case (currMap, smallBucket) =>
                    val items = currMap.getOrElse(smallBucket, Vector.empty)
                    val otherActive = currMap.keys.filter(_ != smallBucket).toSeq
                    if (items.nonEmpty && otherActive.nonEmpty && items.forall { case (e, _) =>
                      otherActive.exists(ob => canServe(ob, elemType, e))
                    }) {
                      val withoutSmall = currMap - smallBucket
                      items.foldLeft(withoutSmall) { case (m, (e, i)) =>
                        val targets = otherActive.filter(ob => canServe(ob, elemType, e))
                        targets.sortBy(ob => m.getOrElse(ob, Vector.empty).size).headOption match {
                          case Some(t) => m.updated(t, m.getOrElse(t, Vector.empty) :+ (e -> i))
                          case None => m
                        }
                      }
                    } else {
                      currMap
                    }
                  }
                } else {
                  assignments
                }

              val resultForType = consolidated.collect {
                case (bucket, items) if items.nonEmpty =>
                  (bucket, elemType) -> items.sortBy(_._2).map(_._1)
              }
              acc ++ resultForType
            }
          }
      }
    }
  }

}
