package org.ergoplatform.wallet

import scorex.util.ModifierId

import scala.collection.mutable

object AssetUtils {

  @inline
  def mergeAssetsMut(into: mutable.Map[ModifierId, Long], from: TokensMap*): Unit = {
    from.foreach(_.foreach {
      case (id, amount) =>
        into.put(id, into.getOrElse(id, 0L) + amount)
    })
  }

  @inline
  def mergeAssets(from: TokensMap, to: TokensMap = Map.empty): TokensMap = {
    from.foldLeft(to) { case (acc, (id, amount)) =>
      acc.updated(id, acc.getOrElse(id, 0L) + amount)
    }
  }

  @inline
  def subtractAssetsMut(from: mutable.Map[ModifierId, Long], subtractor: TokensMap): Unit = {
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
