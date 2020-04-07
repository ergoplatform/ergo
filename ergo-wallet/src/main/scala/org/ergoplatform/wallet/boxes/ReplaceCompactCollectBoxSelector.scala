package org.ergoplatform.wallet.boxes

import org.ergoplatform.wallet.boxes.BoxSelector.BoxSelectionResult
import org.ergoplatform.wallet.boxes.BoxSelector.BoxSelectionError
import org.ergoplatform.ErgoBoxAssets
import scorex.util.ModifierId
import org.ergoplatform.wallet.Utils._

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * A box selector which is parameterized by maximum number of inputs a transaction can have, and optimal number of inputs.
  *
  * Say, the selector is given boxes denoted by their values (1,2,3,4,...10). Then the selector is working as follows:
  *
  * 1) the selector first picking up boxes in given order (1,2,3,4,...) by using DefaultBoxSelector
  * 2) if number of inputs exceeds the limit, the selector is sorting remaining boxes(actually, only 4*maximum inputs of them) by value in descending order and replaces small-value boxes in the inputs by big-value from the tail (1,2,3,4 => 10)
  * 3) if the number of inputs still exceeds the limit, the selector is trying to throw away the dust if possible. E.g. if inputs are (100, 200, 1, 2, 1000), target value is 1300 and maximum number of inputs is 3, the selector kicks out (1, 2)
  * 4) if number of inputs after the previous steps is below optimal, the selector is trying to append the dust, by sorting remaining boxes in ascending order and appending them till optimal number of inputs.
  *
  * @param maxInputs     - maximum number of inputs a transaction can have
  * @param optimalInputs - optimal number of inputs, when transaction is still not expensive. The box selector is
  *                      trying to add dust if a transaction has less inputs than this.
  */
class ReplaceCompactCollectBoxSelector(maxInputs: Int, optimalInputs: Int) extends BoxSelector {

  import ReplaceCompactCollectBoxSelector._

  val ScanDepthFactor = 10

  /**
    * A method which is selecting boxes to spend in order to collect needed amounts of ergo tokens and assets.
    *
    * @param inputBoxes    - unspent boxes to choose from.
    * @param filterFn      - user-provided filter function for boxes. From inputBoxes, only ones to be chosen for which
    *                      filterFn(box) returns true
    * @param targetBalance - ergo balance to be met
    * @param targetAssets  - assets balances to be met
    * @return Left(error) if select() is failing to pick appropriate boxes, otherwise Right(res), where res contains boxes
    *         to spend as well as monetary values and assets for boxes containing change
    *         (wrapped in a special BoxSelectionResult class).
    */
  override def select[T <: ErgoBoxAssets](inputBoxes: Iterator[T],
                      filterFn: T => Boolean,
                      targetBalance: Long,
                      targetAssets: Map[ModifierId, Long]): Either[BoxSelectionError, BoxSelectionResult[T]] = {
    DefaultBoxSelector.select(inputBoxes, filterFn, targetBalance, targetAssets).flatMapRight { initialSelection =>
      val tail = inputBoxes.take(maxInputs * ScanDepthFactor).filter(filterFn).toSeq
      (if (initialSelection.boxes.length > maxInputs) {
        replace(initialSelection, tail, targetBalance, targetAssets)
      } else {
        Right(initialSelection)
      }).flatMapRight { afterReplacement =>
        if (afterReplacement.boxes.length > maxInputs) {
          compress(afterReplacement, targetBalance, targetAssets)
        } else {
          Right(afterReplacement)
        }
      }.flatMapRight { afterCompaction =>
        if (afterCompaction.boxes.length > maxInputs) {
          Left(MaxInputsExceededError(s"${afterCompaction.boxes.length} boxes exceed max inputs in transaction ($maxInputs)"))
        } else if (afterCompaction.boxes.length < optimalInputs) {
          collectDust(afterCompaction, tail, targetBalance, targetAssets)
        } else {
          Right(afterCompaction)
        }
      }
    }
  }

  protected[boxes] def calcChange[T <: ErgoBoxAssets](
    boxes: Seq[T],
    targetBalance: Long,
    targetAssets: Map[ModifierId, Long]
  ): Either[BoxSelectionError, Seq[ErgoBoxAssets]] = {
    val compactedBalance = boxes.map(_.value).sum
    val compactedAssets  = mutable.Map[ModifierId, Long]()
    BoxSelector.mergeAssetsMut(compactedAssets, boxes.map(_.tokens): _*)
    DefaultBoxSelector.formChangeBoxes(compactedBalance, targetBalance, compactedAssets, targetAssets)
  }

  protected[boxes] def collectDust[T <: ErgoBoxAssets](bsr: BoxSelectionResult[T],
                  tail: Seq[T],
                  targetBalance: Long,
                  targetAssets: Map[ModifierId, Long]): Either[BoxSelectionError, BoxSelectionResult[T]] = {
    val diff = optimalInputs - bsr.boxes.length
    val dust = tail.sortBy(_.value).take(diff).filter(b => !bsr.boxes.contains(b))

    val boxes = bsr.boxes ++ dust
    calcChange(boxes, targetBalance, targetAssets).mapRight(changeBoxes => BoxSelectionResult(boxes, changeBoxes))
  }

  protected[boxes] def compress[T <: ErgoBoxAssets](bsr: BoxSelectionResult[T],
               targetBalance: Long,
               targetAssets: Map[ModifierId, Long]): Either[BoxSelectionError, BoxSelectionResult[T]] = {
    val boxes = bsr.boxes
    val diff = boxes.map(_.value).sum - targetBalance

    val boxesToThrowAway = boxes.filter(!_.tokens.keySet.exists(tid => targetAssets.keySet.contains(tid)))
    val sorted = boxesToThrowAway.sortBy(_.value)

    if (diff >= sorted.head.value) {
      var thrownValue = 0L
      val thrownBoxes = sorted.takeWhile { b =>
        thrownValue = thrownValue + b.value
        thrownValue <= diff
      }
      val compactedBoxes = boxes.filter(b => !thrownBoxes.contains(b))
      calcChange(compactedBoxes, targetBalance, targetAssets)
        .mapRight(changeBoxes => BoxSelectionResult(compactedBoxes, changeBoxes))
    } else {
      Right(bsr)
    }
  }

  protected[boxes] def replace[T <: ErgoBoxAssets](bsr: BoxSelectionResult[T],
              tail: Seq[T],
              targetBalance: Long,
              targetAssets: Map[ModifierId, Long]): Either[BoxSelectionError, BoxSelectionResult[T]] = {
    val bigBoxes = tail.sortBy(-_.value)
    val boxesToThrowAway = bsr.boxes.filter(!_.tokens.keySet.exists(tid => targetAssets.keySet.contains(tid)))
    val sorted = boxesToThrowAway.sortBy(_.value)

    type BoxesToAdd = Seq[T]
    type BoxesToDrop = Seq[T]
    type Operations = (BoxesToAdd, BoxesToDrop)

    @tailrec
    def replaceStep(candidates: Seq[T], toDrop: Seq[T], currentOps: Operations): Operations = {
      candidates match {
        case Seq() => currentOps
        case Seq(cand) if cand.value <= toDrop.headOption.map(_.value).getOrElse(Long.MaxValue) => currentOps
        case Seq(cand, cs@_*) =>
          var collected = 0L
          val (dropped, remain) = toDrop.partition { b =>
            collected = collected + b.value
            collected <= cand.value
          }
          replaceStep(cs, remain, (currentOps._1 :+ cand, currentOps._2 ++ dropped))
      }
    }

    val (toAdd, toDrop) = replaceStep(bigBoxes, sorted, (Seq(), Seq()))
    if (toAdd.nonEmpty) {
      val compactedBoxes = bsr.boxes.filter(b => !toDrop.contains(b)) ++ toAdd
      calcChange(compactedBoxes, targetBalance, targetAssets)
        .mapRight(changeBoxes => BoxSelectionResult(compactedBoxes, changeBoxes))
    } else {
      Right(bsr)
    }
  }

}

object ReplaceCompactCollectBoxSelector {
    final case class MaxInputsExceededError(val message: String) extends BoxSelectionError
}
