package org.ergoplatform.utils

import scorex.core.ModifierId

import scala.collection.mutable

object AssetUtils {
  @inline
  def mergeAssets(into: mutable.Map[ModifierId, Long],
                  from: Map[ModifierId, Long]): Unit = {
    from.foreach { case (id, amount) =>
      into.put(id, into.getOrElse(id, 0L) + amount)
    }
  }

  @inline
  def subtractAssets(from: mutable.Map[ModifierId, Long],
                     subtractor: Map[ModifierId, Long]): Unit = {
    subtractor.foreach { case (id, subtractAmt) =>
      val fromAmt = from(id)
      if (fromAmt == subtractAmt) {
        from.remove(id)
      } else {
        from.put(id, fromAmt - subtractAmt)
      }
    }
  }
}
