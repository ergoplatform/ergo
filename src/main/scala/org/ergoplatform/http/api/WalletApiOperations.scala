package org.ergoplatform.http.api

import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.wallet.ErgoWalletReader
import akka.actor.ActorRef
import akka.http.scaladsl.server.Route
import io.circe.Encoder
import scorex.core.api.http.ApiResponse
import scala.concurrent.Future
import akka.pattern.ask

trait WalletApiOperations extends ErgoBaseApiRoute {

  val readersHolder: ActorRef

  protected def withWalletOp[T](op: ErgoWalletReader => Future[T])(toRoute: T => Route): Route = {
    onSuccess((readersHolder ? GetReaders).mapTo[Readers].flatMap(r => op(r.w)))(toRoute)
  }

  protected def withWallet[T: Encoder](op: ErgoWalletReader => Future[T]): Route = {
    withWalletOp(op)(ApiResponse.apply[T])
  }

}
