package org.ergoplatform.wallet.boxes

import scorex.util.ModifierId
import scala.annotation.tailrec
import scala.collection.mutable
import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBoxAssets
import org.ergoplatform.ErgoBoxAssetsHolder

object BoxSelectors {

  // TODO: move all back to BoxSelection trait 

  final case class SelectionResult[T <: ErgoBoxAssets](boxes: Seq[T], changeBoxes: Seq[ErgoBoxAssets])

  def select[T <: ErgoBoxAssets](
    inputBoxes: Iterator[T],
    targetBalance: Long,
    targetAssets: Map[ModifierId, Long]
  ): Option[SelectionResult[T]] =
    select(inputBoxes, _ => true, targetBalance, targetAssets)

  def select[T <: ErgoBoxAssets](
    inputBoxes: Iterator[T],
    externalFilter: T => Boolean,
    targetBalance: Long,
    targetAssets: Map[ModifierId, Long]
  ): Option[SelectionResult[T]] = {
    //mutable structures to collect results
    val res            = mutable.Buffer[T]()
    var currentBalance = 0L
    val currentAssets  = mutable.Map[ModifierId, Long]()

    def pickUp(unspentBox: T) = {
      currentBalance = currentBalance + unspentBox.value
      mergeAssetsMut(currentAssets, unspentBox.tokens)
      res += unspentBox
    }

    def balanceMet = currentBalance >= targetBalance
    def assetsMet = targetAssets.forall {
      case (id, targetAmt) => currentAssets.getOrElse(id, 0L) >= targetAmt
    }

    @tailrec
    def pickBoxes(
      boxesIterator: Iterator[T],
      filterFn: T => Boolean,
      successFn: => Boolean
    ): Boolean =
      if (successFn) {
        true
      } else if (!boxesIterator.hasNext) {
        false
      } else {
        val box = boxesIterator.next()
        if (filterFn(box)) pickUp(box)
        pickBoxes(boxesIterator, filterFn, successFn)
      }

    //first, we pick all the boxes until ergo target balance is met
    if (pickBoxes(inputBoxes, externalFilter, balanceMet)) {
      //then we pick boxes until all the target asset amounts are met (we pick only boxes containing needed assets).
      //If this condition is satisfied on the previous step, we will do one extra call to pickBoxes
      //with no touching the iterator (which is not that much).
      if (pickBoxes(
            inputBoxes,
            bc =>
              externalFilter(bc) && bc.tokens.exists {
                case (id, _) =>
                  val targetAmt       = targetAssets.getOrElse(id, 0L)
                  lazy val currentAmt = currentAssets.getOrElse(id, 0L)
                  targetAmt > 0 && targetAmt > currentAmt
              },
            assetsMet
          )) {
        subtractAssetsMut(currentAssets, targetAssets)
        val changeBoxesAssets: Seq[mutable.Map[ModifierId, Long]] =
          currentAssets.grouped(ErgoBox.MaxTokens).toSeq
        val changeBalance = currentBalance - targetBalance
        formChangeBoxes(changeBalance, changeBoxesAssets).map(changeBoxes =>
          SelectionResult(res, changeBoxes)
        )
      } else {
        None
      }
    } else {
      None
    }

  }

  def calcChange[T <: ErgoBoxAssets](
    boxes: Seq[T],
    targetBalance: Long,
    targetAssets: Map[ModifierId, Long]
  ): Option[Seq[ErgoBoxAssets]] = {
    val compactedBalance = boxes.map(_.value).sum
    val compactedAssets  = mutable.Map[ModifierId, Long]()
    mergeAssetsMut(compactedAssets, boxes.map(_.tokens): _*)

    subtractAssetsMut(compactedAssets, targetAssets)
    val changeBoxesAssets: Seq[mutable.Map[ModifierId, Long]] =
      compactedAssets.grouped(ErgoBox.MaxTokens).toSeq
    val changeBalance = compactedBalance - targetBalance
    formChangeBoxes(changeBalance, changeBoxesAssets)
  }

  def formChangeBoxes(
    changeBalance: Long,
    changeBoxesAssets: Seq[mutable.Map[ModifierId, Long]]
  ): Option[Seq[ErgoBoxAssets]] = {
    //at least 1 ergo token should be assigned per a created box
    if (changeBoxesAssets.size > changeBalance) {
      None
    } else {
      val changeBoxes = if (changeBoxesAssets.nonEmpty) {
        val baseChangeBalance = changeBalance / changeBoxesAssets.size

        val changeBoxesNoBalanceAdjusted = changeBoxesAssets.map { a =>
          ErgoBoxAssetsHolder(baseChangeBalance, a.toMap)
        }

        val modifiedBoxOpt = changeBoxesNoBalanceAdjusted.headOption.map { firstBox =>
          ErgoBoxAssetsHolder(
            changeBalance - baseChangeBalance * (changeBoxesAssets.size - 1),
            firstBox.tokens
          )
        }

        modifiedBoxOpt.toSeq ++ changeBoxesNoBalanceAdjusted.tail
      } else if (changeBalance > 0) {
        Seq(ErgoBoxAssetsHolder(changeBalance))
      } else {
        Seq.empty
      }
      Some(changeBoxes)
    }
  }

  private def mergeAssetsMut(
    into: mutable.Map[ModifierId, Long],
    from: Map[ModifierId, Long]*
  ): Unit = {
    from.foreach(_.foreach {
      case (id, amount) =>
        into.put(id, into.getOrElse(id, 0L) + amount)
    })
  }

  private def subtractAssetsMut(
    from: mutable.Map[ModifierId, Long],
    subtractor: Map[ModifierId, Long]
  ): Unit = {
    subtractor.foreach {
      case (id, subtractAmt) =>
        val fromAmt = from(id)
        if (fromAmt == subtractAmt) {
          from.remove(id)
        } else {
          from.put(id, fromAmt - subtractAmt)
        }
    }
  }

}
