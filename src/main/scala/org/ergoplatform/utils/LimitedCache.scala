package org.ergoplatform.utils

import java.util.concurrent.atomic.AtomicInteger

import scorex.core.ModifierId

import scala.collection.mutable

class LimitedCache[A](limit: Int) {

  protected type MapKey = scala.collection.mutable.WrappedArray.ofByte

  protected def key(id: ModifierId): MapKey = new mutable.WrappedArray.ofByte(id)

  private val map = mutable.HashMap[MapKey, (A, Int)]()
  private var iterator: AtomicInteger = new AtomicInteger(0)

  def put(id: ModifierId, m: A): Unit = {
    map.put(key(id), (m, iterator.incrementAndGet()))
    if (map.size > limit) map.remove(map.minBy(_._2._2)._1)
  }

  def get(id: ModifierId): Option[A] = map.get(key(id)).map(_._1)

  def delete(id: ModifierId): Unit = map.remove(key(id))

}
