package org.ergoplatform.it.api


import java.io.IOException
import java.util.concurrent.TimeoutException

import io.circe.{Decoder, Encoder, Json}
import io.netty.util.{HashedWheelTimer, Timer}
import org.asynchttpclient.Dsl.{get => _get, post => _post}
import org.asynchttpclient._
import org.asynchttpclient.util.HttpConstants
import org.slf4j.LoggerFactory
import scorex.core.utils.ScorexLogging

import scala.compat.java8.FutureConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import org.ergoplatform.it.util._
import io.circe.syntax._
import io.circe.parser._
import io.circe.generic.auto._

trait NodeApi {
  import NodeApi._

  def restAddress: String
  def nodeRestPort: Int
  def blockDelay: FiniteDuration

  protected val client: AsyncHttpClient = new DefaultAsyncHttpClient

  protected val timer: Timer = new HashedWheelTimer()

  protected val log = LoggerFactory.getLogger(s"${getClass.getName} $restAddress")

  def get(path: String, f: RequestBuilder => RequestBuilder = identity): Future[Response] =
    retrying(f(_get(s"http://$restAddress:$nodeRestPort$path")).build())

  def getWihApiKey(path: String, f: RequestBuilder => RequestBuilder = identity): Future[Response] = retrying {
    _get(s"http://$restAddress:$nodeRestPort$path")
      .setHeader("api_key", "integration-test-rest-api")
      .build()
  }

  def post(url: String, port: Int, path: String, f: RequestBuilder => RequestBuilder = identity): Future[Response] =
    retrying(f(
      _post(s"$url:$port$path").setHeader("api_key", "integration-test-rest-api")
    ).build())

  def postJson[A: Encoder](path: String, body: A): Future[Response] =
    post(path, body.asJson.toString())

  def post(path: String, body: String): Future[Response] =
    post(s"http://$restAddress", nodeRestPort, path,
      (rb: RequestBuilder) => rb.setHeader("Content-type", "application/json").setBody(body))

  def ergoJsonAnswerAs[A](body: String)(implicit d: Decoder[A]): A =
    parse(body).right.get.hcursor.get("data").right.get

  def blacklist(networkIpAddress: String, hostNetworkPort: Int): Future[Unit] =
    post("/debug/blacklist", s"$networkIpAddress:$hostNetworkPort").map(_ => ())

  def connectedPeers: Future[Seq[Peer]] = get("/peers/connected").map { r =>
    ergoJsonAnswerAs[Json](r.getResponseBody)
      .hcursor.downField("peers").as[Seq[Peer]].right.get
  }

  def blacklistedPeers: Future[Seq[BlacklistedPeer]] = get("/peers/blacklisted").map { r =>
    ergoJsonAnswerAs[Seq[BlacklistedPeer]](r.getResponseBody)
  }

  def connect(host: String, port: Int): Future[Unit] = postJson("/peers/connect", ConnectReq(host, port)).map(_ => ())

  def waitForPeers(targetPeersCount: Int): Future[Seq[Peer]] = waitFor[Seq[Peer]](_.connectedPeers, _.length >= targetPeersCount, 1.second)

  def height: Future[Int] = get("/history/height").map(r =>
    ergoJsonAnswerAs[Json](r.getResponseBody).hcursor.downField("height").as[Int].right.get)

//  def blockAt(height: Int) = get(s"/blocks/at/$height").as[Block]

//  def utx = get(s"/transactions/unconfirmed").as[Seq[Transaction]]

//  def utxSize = get(s"/transactions/unconfirmed/size").as[Json].map(_.hcursor.downField("size").as[Int].right.get)

//  def lastBlock: Future[Block] = get("/blocks/last").as[Block]

//  def blockSeq(from: Int, to: Int) = get(s"/blocks/seq/$from/$to").as[Seq[Block]]

//  def findTransactionInfo(txId: String): Future[Option[Transaction]] = transactionInfo(txId).transform {
//    case Success(tx) => Success(Some(tx))
//    case Failure(UnexpectedStatusCodeException(_, r)) if r.getStatusCode == 404 => Success(None)
//    case Failure(ex) => Failure(ex)
//  }

//  def waitForTransaction(txId: String): Future[Transaction] = waitFor[Option[Transaction]](_.transactionInfo(txId).transform {
//    case Success(tx) => Success(Some(tx))
//    case Failure(UnexpectedStatusCodeException(_, r)) if r.getStatusCode == 404 => Success(None)
//    case Failure(ex) => Failure(ex)
//  }, tOpt => tOpt.exists(_.id == txId), 1.second).map(_.get)

//  def waitForUtxIncreased(fromSize: Int): Future[Int] = waitFor[Int](
//    _.utxSize,
//    _ > fromSize,
//    100.millis
//  )

  def waitForHeight(expectedHeight: Int): Future[Int] = waitFor[Int](_.height, h => h >= expectedHeight, 1.second)

//  def transactionInfo(txId: String): Future[Transaction] = get(s"/transactions/info/$txId").as[Transaction]

  def status: Future[Status] = get("/debug/status").map(j => ergoJsonAnswerAs[Status](j.getResponseBody))

  def headerIdsByHeight(h: Int): Future[Seq[String]] = get(s"/history/header-ids-by-height/$h").map(j => ergoJsonAnswerAs[Seq[String]](j.getResponseBody))

//  def ensureTxDoesntExist(txId: String): Future[Unit] =
//    utx.zip(findTransactionInfo(txId)).flatMap({
//      case (utx, _) if utx.map(_.id).contains(txId) =>
//        Future.failed(new IllegalStateException(s"Tx $txId is in UTX"))
//      case (_, txOpt) if txOpt.isDefined =>
//        Future.failed(new IllegalStateException(s"Tx $txId is in blockchain"))
//      case _ =>
//        Future.successful(())
//    })

  def waitFor[A](f: this.type => Future[A], cond: A => Boolean, retryInterval: FiniteDuration): Future[A] =
    timer.retryUntil(f(this), cond, retryInterval)

//  def waitForNextBlock: Future[Block] = for {
//    currentBlock <- lastBlock
//    actualBlock <- findBlock(_.height > currentBlock.height, currentBlock.height)
//  } yield actualBlock
//
//  def findBlock(cond: Block => Boolean, from: Int = 1, to: Int = Int.MaxValue): Future[Block] = {
//    def load(_from: Int, _to: Int): Future[Block] = blockSeq(_from, _to).flatMap { blocks =>
//      blocks.find(cond).fold[Future[NodeApi.Block]] {
//        val maybeLastBlock = blocks.lastOption
//        if (maybeLastBlock.exists(_.height >= to)) {
//          Future.failed(new NoSuchElementException)
//        } else {
//          val newFrom = maybeLastBlock.fold(_from)(b => (b.height + 19).min(to))
//          val newTo = newFrom + 19
//          log.debug(s"Loaded ${blocks.length} blocks, no match found. Next range: [$newFrom, ${newFrom + 19}]")
//          timer.schedule(load(newFrom, newTo), blockDelay)
//        }
//      }(Future.successful)
//    }
//
//    load(from, (from + 19).min(to))
//  }

  def close(): Unit = {
    timer.stop()
  }

  def retrying(r: Request, interval: FiniteDuration = 1.second, statusCode: Int = HttpConstants.ResponseStatusCodes.OK_200): Future[Response] = {
    def executeRequest: Future[Response] = {
      log.trace(s"Executing request '$r'")
      client.executeRequest(r, new AsyncCompletionHandler[Response] {
        override def onCompleted(response: Response): Response = {
          if (response.getStatusCode == statusCode) {
            log.debug(s"Request: ${r.getUrl} \n Response: ${response.getResponseBody}")
            response
          } else {
            log.debug(s"Request:  ${r.getUrl} \n Unexpected status code(${response.getStatusCode}): ${response.getResponseBody}")
            throw UnexpectedStatusCodeException(r, response)
          }
        }
      }).toCompletableFuture.toScala
        .recoverWith {
          case e@(_: IOException | _: TimeoutException) =>
            log.debug(s"Failed to execute request '$r' with error: ${e.getMessage}")
            timer.schedule(executeRequest, interval)
        }
    }

    executeRequest
  }

}

object NodeApi extends ScorexLogging {

  case class UnexpectedStatusCodeException(request: Request, response: Response) extends Exception(s"Request: ${request.getUrl}\n" +
    s"Unexpected status code (${response.getStatusCode}): ${response.getResponseBody}")

  case class Peer(declaredAddress: String, nodeName: String, nodeNonce: String)

  case class BlacklistedPeer(hostname : String, timestamp: Long, reason: String)

  case class ConnectReq(host: String, port: Int)

  case class Block(signature: String, height: Int, timestamp: Long, generator: String, transactions: Seq[Transaction],
                   fee: Long)

  case class Transaction(`type`: Int, id: String, fee: Long, timestamp: Long)

  case class Status(status: String)

}
