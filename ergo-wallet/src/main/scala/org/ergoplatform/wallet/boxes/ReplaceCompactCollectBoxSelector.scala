package org.ergoplatform.wallet.boxes

import org.ergoplatform.wallet.boxes.BoxSelector.BoxSelectionResult
import org.ergoplatform.wallet.boxes.BoxSelector.BoxSelectionError
import org.ergoplatform.ErgoBoxAssets
import org.ergoplatform.sdk.wallet.{TokensMap, AssetUtils}
import scorex.util.ModifierId
import org.ergoplatform.wallet.transactions.TransactionBuilder._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * A box selector which is parameterized by maximum number of inputs a transaction can have, and optimal number of inputs.
  *
  * Say, the selector is given boxes denoted by their values (1,2,3,4,...10). Then the selector is working as follows:
  *
  * 1) the selector first picking up boxes in given order (1,2,3,4,...) by using DefaultBoxSelector
  * 2) if number of inputs exceeds the limit, the selector is sorting remaining boxes(actually, only 10*maximum boxes) by value in descending order and replaces small-value boxes in the inputs by big-value from the tail (1,2,3,4 => 10)
  * 3) if the number of inputs still exceeds the limit, the selector is trying to throw away the dust if possible. E.g. if inputs are (100, 200, 1, 2, 1000), target value is 1300 and maximum number of inputs is 3, the selector kicks out (1, 2)
  * 4) if number of inputs after the previous steps is below optimal, the selector is trying to append the dust, by sorting remaining boxes in ascending order and appending them till optimal number of inputs.
  *
  * @param maxInputs     - maximum number of inputs a transaction can have
  * @param optimalInputs - optimal number of inputs, when transaction is still not expensive. The box selector is
  *                      trying to add dust if a transaction has less inputs than this.
  * @param reemissionDataOpt - re-emission settings. If provided, re-emission tokens are considered by box selector
  */
class ReplaceCompactCollectBoxSelector(maxInputs: Int,
                                       optimalInputs: Int,
                                       override val reemissionDataOpt: Option[ReemissionData])
  extends DefaultBoxSelector(reemissionDataOpt) {

  import ReplaceCompactCollectBoxSelector._

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
                                          targetAssets: TokensMap): Either[BoxSelectionError, BoxSelectionResult[T]] = {
    // First picking up boxes in given order (1,2,3,4,...) by using DefaultBoxSelector
    super.select(inputBoxes, filterFn, targetBalance, targetAssets).flatMapRight { initialSelection =>
      val tail = inputBoxes.take(maxInputs * BoxSelector.ScanDepthFactor).filter(filterFn).toSeq
      // if number of inputs exceeds the limit, the selector is sorting remaining boxes(actually, only 10*maximum
      // boxes) by value in descending order and replaces small-value boxes in the inputs by big-value from the tail (1,2,3,4 => 10)
      (if (initialSelection.inputBoxes.length > maxInputs) {
        replace(initialSelection, tail, targetBalance, targetAssets)
      } else {
        Right(initialSelection)
      }).flatMapRight { afterReplacement =>
        // if the number of inputs still exceeds the limit, the selector is trying to throw away the dust if possible.
        // E.g. if inputs are (100, 200, 1, 2, 1000), target value is 1300 and maximum number of inputs is 3,
        // the selector kicks out (1, 2)
        if (afterReplacement.inputBoxes.length > maxInputs) {
          compress(afterReplacement, targetBalance, targetAssets)
        } else {
          Right(afterReplacement)
        }
      }.flatMapRight { afterCompaction =>
        // if number of inputs after the previous steps is below optimal, the selector is trying to append the dust,
        // by sorting remaining boxes in ascending order and appending them till optimal number of inputs.
        if (afterCompaction.inputBoxes.length > maxInputs) {
          Left(MaxInputsExceededError(s"${afterCompaction.inputBoxes.length} boxes exceed max inputs in transaction ($maxInputs)"))
        } else if (afterCompaction.inputBoxes.length < optimalInputs) {
          collectDust(afterCompaction, tail, targetBalance, targetAssets)
        } else {
          Right(afterCompaction)
        }
      }
    }
  }

  protected[boxes] def calcChange[T <: ErgoBoxAssets](boxes: Seq[T],
                                                      targetBalance: Long,
                                                      targetAssets: TokensMap
                                                     ): Either[BoxSelectionError, Seq[ErgoBoxAssets]] = {
    val compactedBalance = boxes.foldLeft(0L) { case (sum, b) => sum + BoxSelector.valueOf(b, reemissionDataOpt) }
    val compactedAssets = mutable.Map[ModifierId, Long]()
    AssetUtils.mergeAssetsMut(compactedAssets, boxes.map(_.tokens): _*)
    super.formChangeBoxes(compactedBalance, targetBalance, compactedAssets, targetAssets)
  }

  protected[boxes] def collectDust[T <: ErgoBoxAssets](bsr: BoxSelectionResult[T],
                                                       tail: Seq[T],
                                                       targetBalance: Long,
                                                       targetAssets: TokensMap): Either[BoxSelectionError, BoxSelectionResult[T]] = {
    val diff = optimalInputs - bsr.inputBoxes.length

    // it is okay to not to consider reemission tokens here probably, so sorting is done by _.value just, not valueOf()
    val dust = tail.sortBy(_.value).take(diff).filter(b => !bsr.inputBoxes.contains(b))

    val boxes = bsr.inputBoxes ++ dust
    calcChange(boxes, targetBalance, targetAssets).mapRight(changeBoxes => selectionResultWithEip27Output(boxes, changeBoxes))
  }

  protected[boxes] def compress[T <: ErgoBoxAssets](bsr: BoxSelectionResult[T],
                                                    targetBalance: Long,
                                                    targetAssets: TokensMap): Either[BoxSelectionError, BoxSelectionResult[T]] = {
    val boxes = bsr.inputBoxes
    val diff = boxes.foldLeft(0L) { case (sum, b) => sum + BoxSelector.valueOf(b, reemissionDataOpt) } - targetBalance

    val targetAssetsKeys = targetAssets.keySet
    val sortedBoxesToThrowAway =
      boxes.filter(!_.tokens.keySet.exists(tid => targetAssetsKeys.contains(tid)))
        .sortBy(b => BoxSelector.valueOf(b, reemissionDataOpt))

    if (diff >= BoxSelector.valueOf(sortedBoxesToThrowAway.head, reemissionDataOpt)) {
      var thrownValue = 0L
      val thrownBoxes = sortedBoxesToThrowAway.takeWhile { b =>
        thrownValue = thrownValue + BoxSelector.valueOf(b, reemissionDataOpt)
        thrownValue <= diff
      }
      val compactedBoxes = boxes.filter(b => !thrownBoxes.contains(b))
      calcChange(compactedBoxes, targetBalance, targetAssets)
        .mapRight(changeBoxes => selectionResultWithEip27Output(compactedBoxes, changeBoxes))
    } else {
      Right(bsr)
    }
  }

  protected[boxes] def replace[T <: ErgoBoxAssets](bsr: BoxSelectionResult[T],
                                                   tail: Seq[T],
                                                   targetBalance: Long,
                                                   targetAssets: TokensMap): Either[BoxSelectionError, BoxSelectionResult[T]] = {
    val bigBoxes = tail.sortBy(b => -BoxSelector.valueOf(b, reemissionDataOpt))
    val boxesToThrowAway = bsr.inputBoxes.filter(!_.tokens.keySet.exists(tid => targetAssets.keySet.contains(tid)))
    val sorted = boxesToThrowAway.sortBy(b => BoxSelector.valueOf(b, reemissionDataOpt))

    val boxesToAdd = ListBuffer.empty[T]
    val boxesToDrop = mutable.HashSet.empty[T]

    @tailrec
    def replaceStep(candidates: Seq[T], toDrop: Seq[T]): Unit = {
      candidates match {
        case Seq() => ()
        case Seq(cand)
          if BoxSelector.valueOf(cand, reemissionDataOpt) <=
            toDrop.headOption.map(b => BoxSelector.valueOf(b, reemissionDataOpt)).getOrElse(Long.MaxValue) =>
          ()
        case Seq(cand, cs@_*) =>
          var collected = 0L
          val candValue = BoxSelector.valueOf(cand, reemissionDataOpt)
          val (dropped, remain) = toDrop.span { b =>
            collected = collected + BoxSelector.valueOf(b, reemissionDataOpt)
            collected <= candValue
          }
          boxesToAdd += cand
          boxesToDrop ++= dropped
          replaceStep(cs, remain)
      }
    }

    replaceStep(bigBoxes, sorted)
    if (boxesToAdd.nonEmpty) {
      val compactedBoxes = bsr.inputBoxes.filter(b => !boxesToDrop.contains(b)) ++ boxesToAdd
      calcChange(compactedBoxes, targetBalance, targetAssets)
        .mapRight(changeBoxes => selectionResultWithEip27Output(compactedBoxes, changeBoxes))
    } else {
      Right(bsr)
    }
  }

}

object ReplaceCompactCollectBoxSelector {
  final case class MaxInputsExceededError(message: String) extends BoxSelectionError
}
