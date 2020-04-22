package org.ergoplatform.wallet

import scorex.util.ModifierId

import scala.collection.mutable

object AssetUtils {

  @inline
  def mergeAssetsMut(into: mutable.Map[ModifierId, Long], from: Map[ModifierId, Long]*): Unit = {
    from.foreach(_.foreach {
      case (id, amount) =>
        into.put(id, into.getOrElse(id, 0L) + amount)
    })
  }

  @inline
  def mergeAssets(from: Map[ModifierId, Long], to: Map[ModifierId, Long] = Map.empty): Map[ModifierId, Long] = {
    from.foldLeft(to) { case (acc, (id, amount)) =>
      acc.updated(id, acc.getOrElse(id, 0L) + amount)
    }
  }

  @inline
  def subtractAssetsMut(from: mutable.Map[ModifierId, Long], subtractor: Map[ModifierId, Long]): Unit = {
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
