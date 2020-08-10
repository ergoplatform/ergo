package org.ergoplatform.http.api

import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.wallet.{ErgoWalletReader, WalletBox}
import akka.actor.ActorRef
import akka.http.scaladsl.server.{Directive, Route}
import io.circe.Encoder
import scorex.core.api.http.ApiResponse

import scala.concurrent.Future
import akka.pattern.ask

trait WalletApiOperations extends ErgoBaseApiRoute {

  val readersHolder: ActorRef

  val boxParams: Directive[(Int, Int)] =
    parameters("minConfirmations".as[Int] ? 0, "minInclusionHeight".as[Int] ? 0)


  /**
    * Filter function for wallet boxes used in box-related API calls.
    * Allows to filter out boxes by height or number of confirmations.
    */
  val boxFilterPredicate: (WalletBox, Int, Int) => Boolean = { (bx: WalletBox, minConfNum: Int, minHeight: Int) =>
    bx.confirmationsNumOpt.getOrElse(0) >= minConfNum &&
      bx.trackedBox.inclusionHeightOpt.getOrElse(-1) >= minHeight
  }


  protected def withWalletOp[T](op: ErgoWalletReader => Future[T])(toRoute: T => Route): Route = {
    onSuccess((readersHolder ? GetReaders).mapTo[Readers].flatMap(r => op(r.w)))(toRoute)
  }

  protected def withWallet[T: Encoder](op: ErgoWalletReader => Future[T]): Route = {
    withWalletOp(op)(ApiResponse.apply[T])
  }

}
