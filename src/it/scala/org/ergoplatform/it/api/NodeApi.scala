package org.ergoplatform.it.api


import java.io.IOException
import java.util.concurrent.TimeoutException

import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import io.netty.util.{HashedWheelTimer, Timer}
import org.asynchttpclient.Dsl.{get => _get, post => _post}
import org.asynchttpclient._
import org.asynchttpclient.util.HttpConstants
import org.ergoplatform.it.util._
import org.slf4j.LoggerFactory
import scorex.core.utils.ScorexLogging

import scala.compat.java8.FutureConverters._
import org.ergoplatform.utils.ErgoTestHelpers.defaultExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration._

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

  def ergoJsonAnswerAs[A](body: String)(implicit d: Decoder[A]): A = parse(body).flatMap(_.as[A]) match {
    case Right(r) => r
    case Left(e) => throw e
  }

  def blacklist(networkIpAddress: String, hostNetworkPort: Int): Future[Unit] =
    post("/debug/blacklist", s"$networkIpAddress:$hostNetworkPort").map(_ => ())

  def connectedPeers: Future[Seq[Peer]] = get("/peers/connected").map { r =>
    ergoJsonAnswerAs[Seq[Peer]](r.getResponseBody)
  }

  def blacklistedPeers: Future[Seq[BlacklistedPeer]] = get("/peers/blacklisted").map { r =>
    ergoJsonAnswerAs[Seq[BlacklistedPeer]](r.getResponseBody)
  }

  def connect(addressAndPort: String): Future[Unit] = post("/peers/connect", addressAndPort).map(_ => ())

  def waitForPeers(targetPeersCount: Int): Future[Seq[Peer]] = waitFor[Seq[Peer]](_.connectedPeers, _.length >= targetPeersCount, 1.second)

  def height: Future[Int] = get("/info") flatMap { r =>
    val response = ergoJsonAnswerAs[Json](r.getResponseBody)
    val eitherHeight = response.hcursor.downField("fullHeight").as[Option[Int]]
    eitherHeight.fold[Future[Int]](
      e => Future.failed(new Exception(s"Error getting `fullHeight` from /info response: $e\n$response", e)),
      maybeHeight => Future.successful(maybeHeight.getOrElse(0))
    )
  }

  def waitForHeight(expectedHeight: Int): Future[Int] = waitFor[Int](_.height, h => h >= expectedHeight, 1.second)

  def waitForStartup: Future[this.type] = get("/info").map(_ => this)

  def status: Future[Status] = get("/info").map(j => Status(ergoJsonAnswerAs[Json](j.getResponseBody).noSpaces))

  def headerIdsByHeight(h: Int): Future[Seq[String]] = get(s"/blocks/at/$h").map(j => ergoJsonAnswerAs[Seq[String]](j.getResponseBody))

  def waitFor[A](f: this.type => Future[A], cond: A => Boolean, retryInterval: FiniteDuration): Future[A] =
    timer.retryUntil(f(this), cond, retryInterval)

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

  case class Peer(address: String, name: String)

  case class BlacklistedPeer(hostname: String, timestamp: Long, reason: String)

  case class Block(signature: String, height: Int, timestamp: Long, generator: String, transactions: Seq[Transaction],
                   fee: Long)

  case class Transaction(`type`: Int, id: String, fee: Long, timestamp: Long)

  case class Status(status: String)

}
