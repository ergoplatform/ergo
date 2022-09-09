package org.ergoplatform.http.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.{Directive, Route}
import akka.pattern.ask
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.mempool.HistogramStats.getFeeHistogram
import org.ergoplatform.settings.ErgoSettings
import scorex.core.api.http.ApiError.BadRequest
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings

import scala.concurrent.Future

case class TransactionsApiRoute(readersHolder: ActorRef,
                                nodeViewActorRef: ActorRef,
                                ergoSettings: ErgoSettings)
                               (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ApiCodecs {

  override val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  val txPaging: Directive[(Int, Int)] = parameters("offset".as[Int] ? 0, "limit".as[Int] ? 50)

  override val route: Route = pathPrefix("transactions") {
    checkTransactionR ~
      getUnconfirmedTransactionsR ~
      sendTransactionR ~
      getFeeHistogramR ~
      getRecommendedFeeR ~
      getExpectedWaitTimeR
  }

  private def getMemPool: Future[ErgoMemPoolReader] = (readersHolder ? GetReaders).mapTo[Readers].map(_.m)

  private def getUnconfirmedTransactions(offset: Int, limit: Int): Future[Json] = getMemPool.map { p =>
    p.getAll.slice(offset, offset + limit).map(_.transaction.asJson).asJson
  }

  private def validateTransactionAndProcess(tx: ErgoTransaction)
                                           (processFn: UnconfirmedTransaction => Route): Route = {
    if (tx.size > ergoSettings.nodeSettings.maxTransactionSize) {
      BadRequest(s"Transaction $tx has too large size ${tx.size}")
    } else {
      onSuccess {
        verifyTransaction(tx, readersHolder, ergoSettings)
      } {
        _.fold(
          e => BadRequest(s"Malformed transaction: ${e.getMessage}"),
          utx => processFn(utx)
        )
      }
    }
  }


  def sendTransactionR: Route = (pathEnd & post & entity(as[ErgoTransaction])) { tx =>
    validateTransactionAndProcess(tx)(validTx => sendLocalTransactionRoute(nodeViewActorRef, validTx))
  }

  def checkTransactionR: Route = (path("check") & post & entity(as[ErgoTransaction])) { tx =>
    validateTransactionAndProcess(tx)(validTx => ApiResponse(validTx.transaction.id))
  }

  def getUnconfirmedTransactionsR: Route = (path("unconfirmed") & get & txPaging) { (offset, limit) =>
    ApiResponse(getUnconfirmedTransactions(offset, limit))
  }

  val feeHistogramParameters: Directive[(Int, Long)] = parameters("bins".as[Int] ? 10, "maxtime".as[Long] ? (60*1000L))

  def getFeeHistogramR: Route = (path("poolHistogram") & get & feeHistogramParameters) { (bins, maxtime) =>
    ApiResponse(getMemPool.map(p => getFeeHistogram(System.currentTimeMillis(), bins, maxtime, p.weightedTransactionIds(Int.MaxValue)).asJson))
  }

  val feeRequestParameters: Directive[(Int, Int)] = parameters("waitTime".as[Int] ? 1, "txSize".as[Int] ? 100)

  def getRecommendedFeeR: Route = (path("getFee") & get & feeRequestParameters) { (waitTime, txSize) =>
    ApiResponse(getMemPool.map(_.getRecommendedFee(waitTime,txSize).asJson))
  }

  val waitTimeRequestParameters: Directive[(Long, Int)] = parameters("fee".as[Long] ? 1000L, "txSize".as[Int] ? 100)

  def getExpectedWaitTimeR: Route = (path("waitTime") & get & waitTimeRequestParameters) { (fee, txSize) =>
    ApiResponse(getMemPool.map(_.getExpectedWaitTime(fee,txSize).asJson))
  }

}
