package org.ergoplatform.wallet.boxes

import org.ergoplatform.ErgoBox
import org.ergoplatform.wallet.boxes.BoxSelector.BoxSelectionResult
import scorex.util.ModifierId

import scala.annotation.tailrec

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

  val ScanDepthFactor = 10

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
  override def select(inputBoxes: Iterator[TrackedBox],
                      filterFn: TrackedBox => Boolean,
                      targetBalance: Long,
                      targetAssets: Map[ModifierId, Long]): Option[BoxSelector.BoxSelectionResult] = {
    DefaultBoxSelector.select(inputBoxes, filterFn, targetBalance, targetAssets).flatMap { initialSelection =>
      val tail = inputBoxes.take(maxInputs * ScanDepthFactor).filter(filterFn).toSeq
      (if (initialSelection.boxes.length > maxInputs) {
        replace(initialSelection, tail, targetBalance, targetAssets)
      } else Some(initialSelection)).flatMap { afterReplacement =>
        if (afterReplacement.boxes.length > maxInputs) {
          compress(afterReplacement, targetBalance, targetAssets)
        } else Some(afterReplacement)
      }.flatMap { afterCompaction =>
        if (afterCompaction.boxes.length > maxInputs) {
          None
        } else if (afterCompaction.boxes.length < optimalInputs) {
          collectDust(afterCompaction, tail, targetBalance, targetAssets)
        } else Some(afterCompaction)
      }
    }
  }

  protected[boxes] def collectDust(bsr: BoxSelectionResult,
                  tail: Seq[TrackedBox],
                  targetBalance: Long,
                  targetAssets: Map[ModifierId, Long]): Option[BoxSelectionResult] = {
    val diff = optimalInputs - bsr.boxes.length
    val afterCompactionIds = bsr.boxes.map(_.id).map(scorex.util.bytesToId)
    val dust = tail.sortBy(_.value).take(diff).filter(b => !afterCompactionIds.contains(b.boxId))

    val boxes = bsr.boxes ++ dust.map(_.box)
    calcChange(boxes, targetBalance, targetAssets).map(changeBoxes => BoxSelectionResult(boxes, changeBoxes))
  }

  protected[boxes] def compress(bsr: BoxSelectionResult,
               targetBalance: Long,
               targetAssets: Map[ModifierId, Long]): Option[BoxSelectionResult] = {
    val boxes = bsr.boxes
    val diff = boxes.map(_.value).sum - targetBalance

    val boxesToThrowAway = boxes.filter(!_.additionalTokens.toArray.map(_._1).exists(tid => targetAssets.keySet.contains(scorex.util.bytesToId(tid))))
    val sorted = boxesToThrowAway.sortBy(_.value)

    if (diff >= sorted.head.value) {
      var thrownValue = 0L
      val thrownBoxes = sorted.takeWhile { b =>
        thrownValue = thrownValue + b.value
        thrownValue <= diff
      }
      val compactedBoxes = boxes.filter(b => !thrownBoxes.contains(b))
      calcChange(compactedBoxes, targetBalance, targetAssets)
        .map(changeBoxes => BoxSelectionResult(compactedBoxes, changeBoxes))
    } else Some(bsr)
  }

  protected[boxes] def replace(bsr: BoxSelectionResult,
              tail: Seq[TrackedBox],
              targetBalance: Long,
              targetAssets: Map[ModifierId, Long]): Option[BoxSelectionResult] = {
    val bigBoxes = tail.sortBy(-_.value).map(_.box)
    val boxesToThrowAway = bsr.boxes.filter(!_.additionalTokens.toArray.map(_._1).exists(tid => targetAssets.keySet.contains(scorex.util.bytesToId(tid))))
    val sorted = boxesToThrowAway.sortBy(_.value)

    type BoxesToAdd = Seq[ErgoBox]
    type BoxesToDrop = Seq[ErgoBox]
    type Operations = (BoxesToAdd, BoxesToDrop)

    @tailrec
    def replaceStep(candidates: Seq[ErgoBox], toDrop: Seq[ErgoBox], currentOps: Operations): Operations = {
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
        .map(changeBoxes => BoxSelectionResult(compactedBoxes, changeBoxes))
    } else Some(bsr)
  }

}
