package org.ergoplatform.wallet.boxes

import org.ergoplatform.ErgoBoxAssets
import org.ergoplatform.ErgoBoxAssetsHolder
import org.ergoplatform.ErgoBox.MaxTokens
import org.ergoplatform.wallet.boxes.BoxSelector.BoxSelectionResult
import scorex.util.ModifierId
import scala.collection.mutable


/**
  * An interface which is exposing a method to select unspent boxes according to target amounts in Ergo tokens and
  * assets and possible user-defined filter. The interface could have many instantiations implementing
  * different strategies.
  */
trait BoxSelector {

  /**
    * A method which is selecting boxes to spend in order to collect needed amounts of ergo tokens and assets.
    *
    * @param inputBoxes    - unspent boxes to choose from.
    * @param filterFn      - user-provided filter function for boxes. From inputBoxes, only ones to be chosen for which
    *                      filterFn(box) returns true
    * @param targetBalance - ergo balance to be met
    * @param targetAssets  - assets balances to be met
    * @return None if select() is failing to pick appropriate boxes, otherwise Some(res), where res contains boxes
    *         to spend as well as monetary values and assets for boxes containing change
    *         (wrapped in a special BoxSelectionResult class).
    */
  def select[T <: ErgoBoxAssets](inputBoxes: Iterator[T],
             filterFn: T => Boolean,
             targetBalance: Long,
             targetAssets: Map[ModifierId, Long]): Option[BoxSelectionResult[T]]
  
  def select[T <: ErgoBoxAssets](inputBoxes: Iterator[T],
    targetBalance: Long,
    targetAssets: Map[ModifierId, Long]
  ): Option[BoxSelectionResult[T]] =
    select(inputBoxes, _ => true, targetBalance, targetAssets)

  protected  def mergeAssetsMut(
    into: mutable.Map[ModifierId, Long],
    from: Map[ModifierId, Long]*
  ): Unit = {
    from.foreach(_.foreach {
      case (id, amount) =>
        into.put(id, into.getOrElse(id, 0L) + amount)
    })
  }

  protected  def subtractAssetsMut(
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

  protected def calcChange[T <: ErgoBoxAssets](
    boxes: Seq[T],
    targetBalance: Long,
    targetAssets: Map[ModifierId, Long]
  ): Option[Seq[ErgoBoxAssets]] = {
    val compactedBalance = boxes.map(_.value).sum
    val compactedAssets  = mutable.Map[ModifierId, Long]()
    mergeAssetsMut(compactedAssets, boxes.map(_.tokens): _*)

    subtractAssetsMut(compactedAssets, targetAssets)
    val changeBoxesAssets: Seq[mutable.Map[ModifierId, Long]] =
      compactedAssets.grouped(MaxTokens).toSeq
    val changeBalance = compactedBalance - targetBalance
    formChangeBoxes(changeBalance, changeBoxesAssets)
  }

  protected def formChangeBoxes(
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

}

object BoxSelector {

  final case class BoxSelectionResult[T <: ErgoBoxAssets](boxes: Seq[T], changeBoxes: Seq[ErgoBoxAssets])

}
