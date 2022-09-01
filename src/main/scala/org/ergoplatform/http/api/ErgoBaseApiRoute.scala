package org.ergoplatform.http.api

import akka.actor.ActorRef
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.{Directive1, ValidationRejection}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.state.{ErgoStateReader, UtxoStateReader}
import org.ergoplatform.settings.{Algos, ErgoSettings}
import scorex.core.api.http.{ApiError, ApiRoute}
import scorex.util.{ModifierId, bytesToId}
import akka.pattern.ask
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import org.ergoplatform.nodeView.mempool.ErgoMemPool.ProcessingOutcome
import org.ergoplatform.nodeView.mempool.ErgoMemPool.ProcessingOutcome._

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.{Success, Try}

trait ErgoBaseApiRoute extends ApiRoute with ApiCodecs {

  implicit val ec: ExecutionContextExecutor = context.dispatcher

  val modifierId: Directive1[ModifierId] = pathPrefix(Segment).flatMap(handleModifierId)

  val modifierIdGet: Directive1[ModifierId] = parameters("id".as[String])
    .flatMap(handleModifierId)

  private def handleModifierId(value: String): Directive1[ModifierId] = {
    Algos.decode(value) match {
      case Success(bytes) => provide(bytesToId(bytes))
      case _ => reject(ValidationRejection("Wrong modifierId format"))
    }
  }

  private def getStateAndPool(readersHolder: ActorRef): Future[(ErgoStateReader, ErgoMemPoolReader)] = {
    (readersHolder ? GetReaders).mapTo[Readers].map { rs =>
      (rs.s, rs.m)
    }
  }

  /**
    * Send local transaction to ErgoNodeViewHolder
    * @return Transaction Id with status OK(200), or BadRequest(400)
    */
  protected def sendLocalTransactionRoute(nodeViewActorRef: ActorRef, unconfirmedTx: UnconfirmedTransaction): Route = {
    val resultFuture =
      (nodeViewActorRef ? LocallyGeneratedTransaction(unconfirmedTx))
        .mapTo[ProcessingOutcome]
        .flatMap {
          case _: Accepted => Future.successful(unconfirmedTx.transaction.id)
          case _: DoubleSpendingLoser => Future.failed(new IllegalArgumentException("Double spending attempt"))
          case d: Declined => Future.failed(d.e)
          case i: Invalidated => Future.failed(i.e)
        }
    completeOrRecoverWith(resultFuture) { ex =>
      ApiError.BadRequest(ex.getMessage)
    }
  }

  /**
    * Helper method to verify transaction against UTXO set (and unconfirmed outputs in the mempool), or check
    * transaction syntax only if UTXO set is not available (the node is running in "digest" mode)
    *
    * Used in /transactions (POST /transactions and /transactions/check methods) and /wallet (/wallet/payment/send
    * and /wallet/transaction/send) API methods to check submitted or generated transaction
    */
  protected def verifyTransaction(tx: ErgoTransaction,
                                  readersHolder: ActorRef,
                                  ergoSettings: ErgoSettings): Future[Try[UnconfirmedTransaction]] = {
    val now: Long = System.currentTimeMillis()
    val bytes = Some(tx.bytes)

    getStateAndPool(readersHolder)
      .map {
        case (utxo: UtxoStateReader, mp: ErgoMemPoolReader) =>
          val maxTxCost = ergoSettings.nodeSettings.maxTransactionCost
          utxo.withMempool(mp)
            .validateWithCost(tx, maxTxCost)
            .map(cost => UnconfirmedTransaction(tx, Some(cost), now, now, bytes, source = None))
        case _ =>
          tx.statelessValidity().map(_ => UnconfirmedTransaction(tx, None, now, now, bytes, source = None))
      }
  }

}
