package org.ergoplatform.utils

import io.iohk.iodb.ByteArrayWrapper

import scala.collection.mutable

object AssetUtils {
  @inline
  def mergeAssets(into: mutable.Map[ByteArrayWrapper, Long],
                  from: Map[ByteArrayWrapper, Long]): Unit = {
    from.foreach { case (id, amount) =>
      into.put(id, into.getOrElse(id, 0L) + amount)
    }
  }
}
