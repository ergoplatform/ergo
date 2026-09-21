package org.ergoplatform.utils

import scala.collection.immutable

/**
 * An immutable map implementation that preserves insertion order for Byte keys.
 * This implementation is consensus-critical and must maintain insertion order
 * across all Scala versions and potential translations to JS and Rust.
 *
 * Unlike scala.collection.Map which only preserves insertion order for up to 4 elements
 * (using Map1...Map4 specialized implementations), this implementation preserves
 * insertion order for all sizes.
 *
 * @tparam V the type of values stored in the map
 */
sealed trait SigmaMap[+V] {
  /**
   * Returns the value associated with the key, or None if the key is not found.
   */
  def get(key: Byte): Option[V]

  /**
   * Returns a new map with the key-value pair added.
   * If the key already exists, its value is updated and the original position is preserved.
   */
  def updated[V1 >: V](key: Byte, value: V1): SigmaMap[V1]

  /**
   * Returns a new map with the key removed.
   */
  def removed(key: Byte): SigmaMap[V]

  /**
   * Returns true if the map contains the key.
   */
  def contains(key: Byte): Boolean

  /**
   * Returns the number of key-value pairs in the map.
   */
  def size: Int

  /**
   * Returns true if the map is empty.
   */
  def isEmpty: Boolean = size == 0

  /**
   * Returns an iterator over the key-value pairs in insertion order.
   */
  def iterator: Iterator[(Byte, V)]

  /**
   * Applies a function to all key-value pairs in insertion order.
   */
  def foreach[U](f: ((Byte, V)) => U): Unit = iterator.foreach(f)

  /**
   * Returns a new map with all key-value pairs that satisfy the predicate.
   */
  def filter(p: ((Byte, V)) => Boolean): SigmaMap[V]

  /**
   * Converts the map to a Seq of key-value pairs in insertion order.
   */
  def toSeq: Seq[(Byte, V)] = iterator.toSeq

  /**
   * Converts the map to a standard Scala Map (may not preserve order in all cases).
   */
  def toMap: Map[Byte, V] = iterator.toMap
}

object SigmaMap {
  /**
   * Creates an empty SigmaMap.
   */
  def empty[V]: SigmaMap[V] = SigmaMapImpl(Vector.empty, Map.empty)

  /**
   * Creates a SigmaMap from a sequence of key-value pairs.
   * If duplicate keys exist, the last occurrence wins and the first position is preserved.
   */
  def apply[V](entries: (Byte, V)*): SigmaMap[V] = {
    entries.foldLeft(empty[V]) { case (map, (k, v)) => map.updated(k, v) }
  }

  /**
   * Creates a SigmaMap from a standard Scala Map.
   * Note: The iteration order of the input map determines the insertion order.
   */
  def fromMap[V](map: Map[Byte, V]): SigmaMap[V] = {
    map.foldLeft(empty[V]) { case (smap, (k, v)) => smap.updated(k, v) }
  }

  /**
   * Internal implementation using a Vector for ordering and a Map for fast lookups.
   *
   * @param insertionOrder Vector of keys in insertion order
   * @param values Map of keys to values for O(1) lookup
   */
  private case class SigmaMapImpl[+V](
    insertionOrder: Vector[Byte],
    values: Map[Byte, V]
  ) extends SigmaMap[V] {

    override def get(key: Byte): Option[V] = values.get(key)

    override def updated[V1 >: V](key: Byte, value: V1): SigmaMap[V1] = {
      if (values.contains(key)) {
        // Key exists: update value, preserve position
        SigmaMapImpl(insertionOrder, values.updated(key, value))
      } else {
        // New key: append to insertion order
        SigmaMapImpl(insertionOrder :+ key, values.updated(key, value))
      }
    }

    override def removed(key: Byte): SigmaMap[V] = {
      if (values.contains(key)) {
        SigmaMapImpl(
          insertionOrder.filterNot(_ == key),
          values - key
        )
      } else {
        this
      }
    }

    override def contains(key: Byte): Boolean = values.contains(key)

    override def size: Int = values.size

    override def iterator: Iterator[(Byte, V)] = {
      insertionOrder.iterator.map(key => (key, values(key)))
    }

    override def filter(p: ((Byte, V)) => Boolean): SigmaMap[V] = {
      val filteredPairs = iterator.filter(p).toVector
      val newOrder = filteredPairs.map(_._1)
      val newValues = filteredPairs.toMap
      SigmaMapImpl(newOrder, newValues)
    }

    override def toString: String = {
      iterator.map { case (k, v) => s"$k -> $v" }.mkString("SigmaMap(", ", ", ")")
    }

    override def equals(obj: Any): Boolean = obj match {
      case that: SigmaMapImpl[_] =>
        this.insertionOrder == that.insertionOrder && this.values == that.values
      case _ => false
    }

    override def hashCode(): Int = {
      insertionOrder.hashCode() * 31 + values.hashCode()
    }
  }
}