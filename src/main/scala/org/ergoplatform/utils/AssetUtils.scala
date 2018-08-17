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
}
