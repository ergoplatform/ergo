package org.ergoplatform.wallet

import scorex.util.ModifierId

import scala.collection.mutable

object AssetUtils {

  @inline
  def mergeAssetsMut(into: mutable.Map[ModifierId, Long], from: TokensMap*): Unit = {
    from.foreach(_.foreach {
      case (id, amount) =>
        into.put(id, Math.addExact(into.getOrElse(id, 0L), amount))
    })
  }

  @inline
  def mergeAssets(to: TokensMap, from: TokensMap*): TokensMap = {
    from.foldLeft(to){ case (to, from) =>
      from.foldLeft(to) { case (acc, (id, amount)) =>
        acc.updated(id, Math.addExact(acc.getOrElse(id, 0L), amount))
      }
    }
  }

  @inline
  def subtractAssets(from: TokensMap, subtractor: TokensMap*): TokensMap = {
    subtractor.foldLeft(from){ case (from, substractor) =>
      substractor.foldLeft(from) { case (acc, (id, amount)) =>
        val currAmt = acc.getOrElse(id,
          throw new IllegalArgumentException(s"Cannot subtract $amount of token $id: token not found in $acc"))
        require(amount >= 0, s"Cannot subtract negative amount from token $id: $amount")
        require(currAmt >= amount, s"Not enough amount of token $id -> $currAmt to subtract $amount")
        if (currAmt == amount) {
          acc - id
        } else {
          acc.updated(id, currAmt - amount)
        }
      }
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
