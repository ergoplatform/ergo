package org.ergoplatform.db

import scala.collection.mutable

trait KVStore[K, V] extends AutoCloseable {

  def get(key: K): Option[V]

  def getAll: Iterable[(K, V)]

  def getOrElse(key: K, default: => V): V =
    get(key).getOrElse(default)

  def get(keys: Iterable[K]): Iterable[(K, Option[V])] = {
    val bf = mutable.ArrayBuffer.empty[(K, Option[V])]
    keys.foreach(k => bf += (k -> get(k)))
    bf
  }

}
