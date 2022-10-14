package org.ergoplatform.http.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.{Directive, Directive1, Route, ValidationRejection}
import akka.pattern.ask
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.ErgoBox.{BoxId, NonMandatoryRegisterId, TokenId}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.mempool.HistogramStats.getFeeHistogram
import org.ergoplatform.nodeView.state.{ErgoStateReader, UtxoStateReader}
import org.ergoplatform.settings.{Algos, ErgoSettings}
import scorex.core.api.http.ApiError.BadRequest
import scorex.core.api.http.{ApiError, ApiResponse}
import scorex.core.settings.RESTApiSettings
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Digest32
import scorex.util.encode.Base16
import sigmastate.SType
import sigmastate.Values.EvaluatedValue

import scala.concurrent.Future
import scala.util.Success

case class TransactionsApiRoute(readersHolder: ActorRef,
                                nodeViewActorRef: ActorRef,
                                ergoSettings: ErgoSettings)
                               (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ApiCodecs {

  override val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  val txPaging: Directive[(Int, Int)] = parameters("offset".as[Int] ? 0, "limit".as[Int] ? 50)

  val boxId: Directive1[BoxId] = pathPrefix(Segment).flatMap(handleBoxId)

  private def handleBoxId(value: String): Directive1[BoxId] = {
    ADKey @@ Base16.decode(value) match {
      case Success(boxId) =>
        provide(boxId)
      case _ =>
        reject(ValidationRejection("Wrong boxId format, it should be hex string"))
    }
  }

  val tokenId: Directive1[TokenId] = pathPrefix(Segment).flatMap(handleTokenId)

  private def handleTokenId(value: String): Directive1[TokenId] = {
    Digest32 @@ Algos.decode(value) match {
      case Success(tokenId) =>
        provide(tokenId)
      case _ =>
        reject(ValidationRejection("Wrong tokenId format, it should be hex string"))
    }
  }

  override val route: Route = pathPrefix("transactions") {
    checkTransactionR ~
      getUnconfirmedOutputByRegistersR ~
      getUnconfirmedOutputByTokenIdR ~
      getUnconfirmedOutputByErgoTreeR ~
      getUnconfirmedOutputByBoxIdR ~
      getUnconfirmedInputByBoxIdR ~
      getUnconfirmedTxsByErgoTreeR ~
      getUnconfirmedTxByIdR ~
      getUnconfirmedTransactionsR ~
      unconfirmedContainsR ~
      sendTransactionR ~
      getFeeHistogramR ~
      getRecommendedFeeR ~
      getExpectedWaitTimeR
  }

  private def getMemPool: Future[ErgoMemPoolReader] = (readersHolder ? GetReaders).mapTo[Readers].map(_.m)

  private def getState: Future[ErgoStateReader] = (readersHolder ? GetReaders).mapTo[Readers].map(_.s)

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

  /** Unconfirmed Txs */

  /** Check whether given transaction is present in mempool without returning it */
  def unconfirmedContainsR: Route = (pathPrefix("unconfirmed") & head & modifierId) { modifierId =>
    ApiResponse(getMemPool.map(_.modifierById(modifierId)))
  }

  /** Get unconfirmed transactions at given offset and limit*/
  def getUnconfirmedTransactionsR: Route = (path("unconfirmed") & get & txPaging) { (offset, limit) =>
    ApiResponse(getUnconfirmedTransactions(offset, limit))
  }

  /** Get unconfirmed transaction by its id */
  def getUnconfirmedTxByIdR: Route =
    (pathPrefix("unconfirmed" / "byTransactionId") & get & modifierId) { modifierId =>
      ApiResponse(getMemPool.map(_.modifierById(modifierId)))
    }

  /** Collect all transactions which inputs or outputs contain given ErgoTree hex */
  def getUnconfirmedTxsByErgoTreeR: Route =
    (pathPrefix("unconfirmed" / "byErgoTree") & post & entity(as[Json]) & txPaging) { case (body, offset, limit) =>
      body.as[String] match {
        case Left(ex) =>
          ApiError(StatusCodes.BadRequest, ex.getMessage())
        case Right(ergoTree) =>
          ApiResponse(
            getMemPool.flatMap { pool =>
              val allTxs = pool.getAll.slice(offset, offset + limit)
              val txsWithOutputMatch =
                allTxs
                  .collect { case tx if tx.transaction.outputs.exists(_.ergoTree.bytesHex == ergoTree) =>
                    tx.transaction
                  }.toSet

              getState.map {
                case state: UtxoStateReader =>
                  val txWithInputMatch =
                    allTxs
                      .collect { case tx if
                          tx.transaction.inputs.exists(i => state.boxById(i.boxId).exists(_.ergoTree.bytesHex == ergoTree)) =>
                        tx.transaction
                      }
                  txsWithOutputMatch ++ txWithInputMatch
                case _ =>
                  txsWithOutputMatch
              }
            }
          )
      }
  }

  /** Get input box by box id from unconfirmed transactions */
  def getUnconfirmedInputByBoxIdR: Route =
    (pathPrefix("unconfirmed" / "inputs" / "byBoxId") & get & boxId) { boxId =>
      ApiResponse(
        getMemPool.flatMap { pool =>
          getState.map {
            case state: UtxoStateReader =>
              pool.getAll
                .flatMap(_.transaction.inputs.filter(_.boxId.sameElements(boxId)).flatMap(i => state.boxById(i.boxId).toList))
                .headOption
            case _ =>
              Option.empty
          }
        }
      )
    }

  /** Get output box by box id from unconfirmed transactions */
  def getUnconfirmedOutputByBoxIdR: Route =
    (pathPrefix("unconfirmed" / "outputs" / "byBoxId") & get & boxId) { boxId =>
      ApiResponse(
        getMemPool.map(_.getAll.flatMap(_.transaction.outputs.filter(_.id.sameElements(boxId))).headOption)
      )
    }

  /** Collect all tx outputs which contain given ErgoTree hex */
  def getUnconfirmedOutputByErgoTreeR: Route =
    (pathPrefix("unconfirmed" / "outputs" / "byErgoTree") & post & entity(as[Json]) & txPaging) { (body, offset, limit) =>
      body.as[String] match {
        case Left(ex) =>
          ApiError(StatusCodes.BadRequest, ex.getMessage())
        case Right(ergoTree) =>
          ApiResponse(
            getMemPool
              .map(_.getAll.slice(offset, offset + limit).flatMap(_.transaction.outputs.filter(_.ergoTree.bytesHex == ergoTree)))
          )
      }
    }

  /** Collect all tx outputs which contain given TokenId hex */
  def getUnconfirmedOutputByTokenIdR: Route =
    (pathPrefix("unconfirmed" / "outputs" / "byTokenId") & get & tokenId) { tokenId =>
      ApiResponse(
        getMemPool.map(_.getAll.flatMap(_.transaction.outputs.filter(_.additionalTokens.exists(_._1.sameElements(tokenId)))))
      )
    }

  /** Collect all tx outputs which contain all given Registers */
  def getUnconfirmedOutputByRegistersR: Route =
    (pathPrefix("unconfirmed" / "outputs" / "byRegisters") & post & entity(as[Json]) & txPaging) { (body, offset, limit) =>
      body.as[Map[NonMandatoryRegisterId, EvaluatedValue[SType]]] match {
        case Left(ex) =>
          ApiError(StatusCodes.BadRequest, ex.getMessage())
        case Right(registers) if registers.isEmpty =>
          ApiError(StatusCodes.BadRequest, "Registers filter cannot be empty")
        case Right(registers) =>
          ApiResponse(
            getMemPool.map { pool =>
              pool
                .getAll.slice(offset, offset + limit)
                .flatMap(_.transaction.outputs.filter(o => registers.toSet.diff(o.additionalRegisters.toSet).isEmpty))
            }
          )
      }
    }

}
