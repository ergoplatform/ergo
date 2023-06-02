package org.ergoplatform.wallet.boxes

import org.ergoplatform.SigmaConstants.MaxBoxSize
import org.ergoplatform.sdk.wallet.TokensMap
import org.ergoplatform.wallet.boxes.BoxSelector.{BoxSelectionError, BoxSelectionResult}
import org.ergoplatform.{ErgoBoxAssets, ErgoBoxAssetsHolder}
import scorex.util.ScorexLogging


/**
  * An interface which is exposing a method to select unspent boxes according to target amounts in Ergo tokens and
  * assets and possible user-defined filter. The interface could have many instantiations implementing
  * different strategies.
  */
trait BoxSelector extends ScorexLogging {

  /**
    * Re-emission settings, if provided. Used to consider re-emission tokens
    * stored in boxes being spent.
    */
  def reemissionDataOpt: Option[ReemissionData]

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
  def select[T <: ErgoBoxAssets](inputBoxes: Iterator[T],
             filterFn: T => Boolean,
             targetBalance: Long,
             targetAssets: TokensMap): Either[BoxSelectionError, BoxSelectionResult[T]]
  
  def select[T <: ErgoBoxAssets](inputBoxes: Iterator[T],
    targetBalance: Long,
    targetAssets: TokensMap
  ): Either[BoxSelectionError, BoxSelectionResult[T]] =
    select(inputBoxes, _ => true, targetBalance, targetAssets)

  /**
    * Helper method to get total amount of re-emission tokens stored in input `boxes`.
    */
  def reemissionAmount[T <: ErgoBoxAssets](boxes: Seq[T]): Long = {
    reemissionDataOpt.map { reemissionData =>
      boxes.foldLeft(0L) { case (sum, b) => sum + b.tokens.getOrElse(reemissionData.reemissionTokenId, 0L) }
    }.getOrElse(0L)
  }

  def selectionResultWithEip27Output[T <: ErgoBoxAssets](inputBoxes: Seq[T],
                                                         changeBoxes: Seq[ErgoBoxAssets]): BoxSelectionResult[T] = {
    val reemissionAmt = reemissionAmount(inputBoxes)
    val payToReemissionBox = if(reemissionAmt > 0) {
      Some(ErgoBoxAssetsHolder(reemissionAmt))
    } else {
      None
    }
    new BoxSelectionResult(inputBoxes, changeBoxes, payToReemissionBox)
  }

}

object BoxSelector {

  // from https://github.com/ergoplatform/ergo/blob/2ce78a0380977b8ca354518edca93a5269ac9f53/src/main/scala/org/ergoplatform/settings/Parameters.scala#L258-L258
  private val MinValuePerByteDefault = 30 * 12
  val MinBoxValue: Long = (MaxBoxSize.value / 2L) * MinValuePerByteDefault

  /**
    * Factor which is showing how many inputs selector is going through to optimize inputs.
    * Bigger factor is slowing down inputs selection but minimizing chance of transaction failure.
    */
  val ScanDepthFactor = 300

  /**
    * Containter for box selector output
    *
    * @param inputBoxes - transaction inputs chosen by a selector
    * @param changeBoxes - change outputs
    * @param payToReemissionBox - pay-to-reemission output mde according to EIP-27, if needed
    */
  class BoxSelectionResult[T <: ErgoBoxAssets](val inputBoxes: Seq[T],
                                               val changeBoxes: Seq[ErgoBoxAssets],
                                               val payToReemissionBox: Option[ErgoBoxAssets])

  /**
    * Returns how much ERG can be taken from a box when it is spent.
    *
    * @param box - box which may be spent
    * @param reemissionDataOpt - re-emission data, if box selector is checking re-emission rules
    * @return if no re-emission tokens are there, returns ERG value of the box, otherwise,
    *         subtract amount of re-emission tokens in the box from its ERG value.
    */
  def valueOf[T <: ErgoBoxAssets](box: T, reemissionDataOpt: Option[ReemissionData]): Long = {
    reemissionDataOpt match {
      case Some(reemissionData) => box.value - box.tokens.getOrElse(reemissionData.reemissionTokenId, 0L)
      case None => box.value
    }
  }

  trait BoxSelectionError {
    def message: String
  }

}
