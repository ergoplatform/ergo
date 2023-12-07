package org.ergoplatform.http.api

import akka.actor.ActorRef
import akka.http.scaladsl.server.{Directive, Route, ValidationRejection}
import akka.pattern.ask
import io.circe.Encoder
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.wallet.{ErgoWalletReader, WalletBox}
import scorex.core.api.http.ApiResponse

import scala.concurrent.Future

trait WalletApiOperations extends ErgoBaseApiRoute {

  val readersHolder: ActorRef
  val MaxLimit = 2500

  private def isLegalOffset(offset: Int): Boolean = offset >= 0

  private def isLegalLimit(limit: Int): Boolean = limit >= 1 && limit <= MaxLimit

  private val isLegalBoxParamCombination: ((Int, Int, Int, Int, Int, Int)) => Boolean = {
    case (minConfNum, _, _, maxHeight, limit, offset) =>
      isLegalOffset(offset) && 
      isLegalLimit(limit) &&
      // maxInclusionHeight cannot be specified when we consider unconfirmed
      !(minConfNum == -1 && maxHeight != -1)
  }

  val boxParams: Directive[(Int, Int, Int, Int, Int, Int)] =
    parameters(
      "minConfirmations".as[Int] ? 0, 
      "maxConfirmations".as[Int] ? -1, 
      "minInclusionHeight".as[Int] ? 0, 
      "maxInclusionHeight".as[Int] ? -1,
      "limit".as[Int] ? 500,
      "offset".as[Int] ? 0  
    ).tfilter(
      isLegalBoxParamCombination,
      ValidationRejection("maxInclusionHeight cannot be specified when we consider unconfirmed")
    )

  /**
    * Filter function that filters boxes by height or number of confirmations.
    */
  val boxConfirmationHeightFilter: (WalletBox, Int, Int, Int, Int) => Boolean = {
    (bx: WalletBox, minConfNum: Int, maxConfNum: Int, minHeight: Int, maxHeight: Int) =>
      val minConstraints =
        bx.confirmationsNumOpt.getOrElse(0) >= minConfNum &&
          bx.trackedBox.inclusionHeightOpt.getOrElse(0) >= minHeight
      val maxConstraints =
        (maxConfNum == -1 || bx.confirmationsNumOpt.getOrElse(0) <= maxConfNum) &&
          (maxHeight == -1 || bx.trackedBox.inclusionHeightOpt.getOrElse(Int.MaxValue) <= maxHeight)
      minConstraints && maxConstraints
  }

  /**
    * Filter function that filters boxes by number of confirmations.
    */
  val boxConfirmationFilter: (WalletBox, Int, Int) => Boolean = {
    (bx: WalletBox, minConfNum: Int, maxConfNum: Int) =>
      val minConstraints = bx.confirmationsNumOpt.getOrElse(0) >= minConfNum
      val maxConstraints = maxConfNum == -1 || bx.confirmationsNumOpt.getOrElse(0) <= maxConfNum
      minConstraints && maxConstraints
  }


  protected def withWalletOp[T](op: ErgoWalletReader => Future[T])(toRoute: T => Route): Route = {
    onSuccess((readersHolder ? GetReaders).mapTo[Readers].flatMap(r => op(r.w)))(toRoute)
  }

  protected def withWallet[T: Encoder](op: ErgoWalletReader => Future[T]): Route = {
    withWalletOp(op)(ApiResponse.apply[T])
  }

}
