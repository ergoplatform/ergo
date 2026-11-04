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
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.settings.NetworkType
import org.slf4j.{Logger, LoggerFactory}
import scorex.util.ScorexLogging

import scala.compat.java8.FutureConverters._
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future, blocking}
import scala.util.control.NonFatal

trait NodeApi {

  import NodeApi._

  implicit def ec: ExecutionContext

  def restAddress: String

  def nodeRestPort: Int

  def blockDelay: FiniteDuration

  protected val client: AsyncHttpClient = new DefaultAsyncHttpClient

  protected val timer: Timer = new HashedWheelTimer()

  protected val log: Logger = LoggerFactory.getLogger(s"${getClass.getName} $restAddress")

  /** How long requests and waits on this node may go on; unbounded unless overridden. */
  protected def waitPolicy: WaitPolicy = WaitPolicy.Unbounded

  /** Names this node in failure messages. */
  def nodeLabel: String = s"$restAddress:$nodeRestPort"

  /** Why the node cannot answer any more, e.g. its container exited; blocking. */
  protected def containerProblem(): Option[String] = None

  /** The clock of stall checks. */
  protected def stallClock: () => Deadline = () => Deadline.now

  def get(path: String, f: RequestBuilder => RequestBuilder = identity): Future[Response] =
    retrying(f(_get(s"http://$restAddress:$nodeRestPort$path")).build())

  def singleGet(path: String, f: RequestBuilder => RequestBuilder = identity): Future[Response] = {
    client.executeRequest(f(_get(s"http://$restAddress:$nodeRestPort$path")).build())
      .toCompletableFuture
      .toScala
  }

  def getWihApiKey(path: String, f: RequestBuilder => RequestBuilder = identity): Future[Response] = retrying {
    _get(s"http://$restAddress:$nodeRestPort$path")
      .setHeader("api_key", "hello")
      .build()
  }

  def post(url: String, port: Int, path: String, f: RequestBuilder => RequestBuilder = identity): Future[Response] =
    retrying(f(
      _post(s"$url:$port$path").setHeader("api_key", "hello")
    ).build())

  def postJson[A: Encoder](path: String, body: A): Future[Response] =
    post(path, body.asJson.toString())

  def post(path: String, body: String): Future[Response] =
    post(s"http://$restAddress", nodeRestPort, path,
      (rb: RequestBuilder) => rb.setHeader("Content-type", "application/json").setBody(body))

  def ergoJsonAnswerAs[A](body: String)(implicit d: Decoder[A]): A = parse(body)
    .flatMap(_.as[A])
    .fold(e => throw e, r => r)

  def blacklist(networkIpAddress: String, hostNetworkPort: Int): Future[Unit] =
    post("/debug/blacklist", s"$networkIpAddress:$hostNetworkPort").map(_ => ())

  def connectedPeers: Future[Seq[Peer]] = get("/peers/connected").map { r =>
    ergoJsonAnswerAs[Seq[Peer]](r.getResponseBody)
  }

  def allPeers: Future[Seq[Peer]] = get("/peers/all").map { r =>
    ergoJsonAnswerAs[Seq[Peer]](r.getResponseBody)
  }

  def blacklistedPeers: Future[Seq[BlacklistedPeer]] = get("/peers/blacklisted").map { r =>
    ergoJsonAnswerAs[Seq[BlacklistedPeer]](r.getResponseBody)
  }

  def connect(addressAndPort: String): Future[Unit] = post("/peers/connect", addressAndPort).map(_ => ())

  def waitForPeers(targetPeersCount: Int): Future[Seq[Peer]] = {
    waitFor[Seq[Peer]](_.connectedPeers, _.length >= targetPeersCount, 1.second)
  }

  def waitForHeight(expectedHeight: Int, retryingInterval: FiniteDuration = 1.second): Future[Int] = {
    waitForProgress[NodeInfo, (Option[Int], Option[String])](
      s"fullHeight >= $expectedHeight",
      _.info,
      _.bestBlockHeightOpt.getOrElse(0) >= expectedHeight,
      retryingInterval
    )(
      // full-block tip only: headers keep following a miner while block download stalls
      info => Some(info.bestBlockHeightOpt -> info.bestBlockIdOpt),
      describeTips
    ).map(_.bestBlockHeightOpt.getOrElse(0))
  }

  def waitForStartup: Future[this.type] = get("/info").map(_ => this)

  def fullHeight: Future[Int] = get("/info") flatMap { r =>
    val response = ergoJsonAnswerAs[Json](r.getResponseBody)
    val eitherHeight = response.hcursor.downField("fullHeight").as[Option[Int]]
    eitherHeight.fold[Future[Int]](
      e => Future.failed(new Exception(s"Error getting `fullHeight` from /info response: $e\n$response", e)),
      maybeHeight => Future.successful(maybeHeight.getOrElse(0))
    )
  }

  def status: Future[Status] = get("/info").map(j => Status(ergoJsonAnswerAs[Json](j.getResponseBody).noSpaces))

  def info: Future[NodeInfo] = get("/info").map(r => ergoJsonAnswerAs[NodeInfo](r.getResponseBody))

  def headerIdsByHeight(h: Int): Future[Seq[String]] = get(s"/blocks/at/$h")
    .map(j => ergoJsonAnswerAs[Seq[String]](j.getResponseBody))

  def headerById(id: String): Future[Header] = get(s"/blocks/$id/header")
    .map(r => ergoJsonAnswerAs[Header](r.getResponseBody))

  def headers(offset: Int, limit: Int): Future[Seq[String]] = get(s"/blocks?offset=$offset&limit=$limit")
    .map(r => ergoJsonAnswerAs[Seq[String]](r.getResponseBody))

  def waitFor[A](f: this.type => Future[A], cond: A => Boolean, retryInterval: FiniteDuration): Future[A] = {
    timer.retryUntil(f(this), cond, retryInterval)
  }

  /** Like `waitFor`, but logs the wait at INFO (start, every 30 s, end) and, if this
    * node's policy has a stall limit, fails once `progress` of the observed value has not
    * changed for that long; a sample without a key never counts as progress. */
  def waitForProgress[A, K](what: String,
                            f: this.type => Future[A],
                            cond: A => Boolean,
                            retryInterval: FiniteDuration)
                           (progress: A => Option[K],
                            describe: A => String): Future[A] = {
    val watch      = new StallWatch[K](stallClock)
    val started    = Deadline.now
    var nextReport = started + ProgressReportInterval
    log.info(s"$nodeLabel: waiting for $what")

    def loop(): Future[A] = f(this).flatMap { value =>
      val quiet = watch.record(progress(value))
      if (cond(value)) {
        log.info(s"$nodeLabel: $what after ${(Deadline.now - started).toSeconds} s, " +
          s"longest quiet period ${watch.longestQuietPeriod.toMillis} ms")
        Future.successful(value)
      } else if (waitPolicy.stallLimit.exists(quiet >= _)) {
        infoSummary.flatMap { info =>
          val error = new NoProgressException(
            s"$nodeLabel: no progress towards $what for ${quiet.toSeconds} s, " +
              s"last seen ${describe(value)}; /info now: $info")
          // Future.traverse reports only the first failure in its list order
          log.warn(error.getMessage)
          Future.failed(error)
        }
      } else {
        if (nextReport.isOverdue()) {
          log.info(s"$nodeLabel: still waiting for $what after " +
            s"${(Deadline.now - started).toSeconds} s, last seen ${describe(value)}")
          nextReport = Deadline.now + ProgressReportInterval
        }
        timer.schedule(loop(), retryInterval)
      }
    }

    loop()
  }

  /** Selected /info fields for failure messages; never fails. */
  def infoSummary: Future[String] =
    Future.unit
      .flatMap(_ => singleGet("/info", _.setRequestTimeout(5000)))
      .map { r =>
        val cursor = parse(r.getResponseBody).getOrElse(Json.Null).hcursor
        InfoSummaryFields
          .map(field => s"$field=${cursor.downField(field).focus.fold("?")(_.noSpaces)}")
          .mkString(" ")
      }
      .recover { case NonFatal(e) => s"unavailable ($e)" }

  private def describeTips(info: NodeInfo): String =
    s"fullHeight=${info.bestBlockHeightOpt.getOrElse("none")} " +
      s"bestFullHeaderId=${info.bestBlockIdOpt.getOrElse("none")} " +
      s"headersHeight=${info.bestHeaderHeightOpt.getOrElse("none")} " +
      s"bestHeaderId=${info.bestHeaderIdOpt.getOrElse("none")}"

  def close(): Unit = {
    timer.stop()
  }

  def retrying(request: Request,
               interval: FiniteDuration = 1.second,
               statusCode: Int = HttpConstants.ResponseStatusCodes.OK_200): Future[Response] = {
    def executeRequest(failingSince: Option[Deadline], attempt: Int): Future[Response] = {
      log.trace(s"Executing request '$request'")
      val startedAt = Deadline.now
      client.executeRequest(request, new AsyncCompletionHandler[Response] {
        override def onCompleted(response: Response): Response = {
          if (response.getStatusCode == statusCode) {
            log.debug(s"Request: ${request.getUrl} \n Response: ${response.getResponseBody}")
            response
          } else {
            log.debug(s"Request:  ${request.getUrl} \n Unexpected status code(${response.getStatusCode}): " +
              s"${response.getResponseBody}")
            throw UnexpectedStatusCodeException(request, response)
          }
        }
      }).toCompletableFuture.toScala
        .recoverWith {
          case e@(_: IOException | _: TimeoutException) =>
            log.debug(s"Failed to execute request '$request' with error: ${e.getMessage}")
            val since = failingSince.getOrElse(startedAt)
            unreachableReason(since, attempt).flatMap {
              case Some(reason) =>
                val url            = request.getUrl
                val unreachableFor = Deadline.now - since
                val error =
                  NodeUnreachableException(nodeLabel, url, unreachableFor, reason, e)
                // Future.traverse reports only the first failure in its list order
                log.warn(error.getMessage)
                Future.failed(error)
              case None =>
                timer.schedule(executeRequest(Some(since), attempt + 1), interval)
            }
        }
    }

    executeRequest(None, attempt = 1)
  }

  /** Why to stop retrying a request failing since `since`, if this node's policy says so;
    * every third attempt also asks whether the container is still running. */
  private def unreachableReason(since: Deadline, attempt: Int): Future[Option[String]] = {
    val policy  = waitPolicy
    val expired = policy.maxUnreachable.filter(Deadline.now - since >= _)
    val problem =
      if (policy.checkContainer && (expired.isDefined || attempt % 3 == 0)) {
        Future(blocking(containerProblem())).recover { case NonFatal(_) => None }
      } else {
        Future.successful(None)
      }
    problem.map(_.orElse(expired.map(limit => s"no response for $limit")))
  }

}

object NodeApi extends ScorexLogging {

  case class UnexpectedStatusCodeException(request: Request, response: Response)
    extends Exception(s"Request: ${request.getUrl}\n Unexpected status code (${response.getStatusCode}): " +
      s"${response.getResponseBody}")

  /** Not an IOException nor a TimeoutException, so that nothing retries it; serializable,
    * so that a forked test JVM can report it. */
  case class NodeUnreachableException(node: String,
                                      url: String,
                                      unreachableFor: FiniteDuration,
                                      reason: String,
                                      cause: Throwable)
    extends Exception(
      s"$node: $url unreachable for ${unreachableFor.toSeconds} s, " +
        s"$reason; last error: $cause",
      cause
    )

  val ProgressReportInterval: FiniteDuration = 30.seconds

  val InfoSummaryFields: Seq[String] = Seq(
    "fullHeight",
    "bestFullHeaderId",
    "headersHeight",
    "bestHeaderId",
    "peersCount",
    "maxPeerHeight",
    "isMining"
  )

  /** Bounds on requests to and waits on a node; None keeps them going as long as the
    * caller waits. */
  case class WaitPolicy(maxUnreachable: Option[FiniteDuration],
                        stallLimit: Option[FiniteDuration],
                        checkContainer: Boolean)

  object WaitPolicy {
    val Unbounded: WaitPolicy = WaitPolicy(None, None, checkContainer = false)

    /** The suites' devnet nodes answer REST within seconds of their container start; a
      * wait on them fails once what it watches stands still for StallWatch's limit. */
    val LocalDevNet: WaitPolicy =
      WaitPolicy(Some(60.seconds), Some(StallWatch.DefaultLimit), checkContainer = true)

    /** it2's mainnet nodes may be busy for minutes (NiPoPoW proof, UTXO snapshot), so
      * they keep the unbounded behaviour. */
    def forNetwork(networkType: NetworkType): WaitPolicy = networkType match {
      case NetworkType.DevNet | NetworkType.DevNet60 => LocalDevNet
      case _                                         => Unbounded
    }
  }

  case class Peer(address: String, name: String)

  case class BlacklistedPeer(hostname: String, timestamp: Long, reason: String)

  case class Block(signature: String, height: Int, timestamp: Long, generator: String, transactions: Seq[Transaction],
                   fee: Long)

  case class Transaction(`type`: Int, id: String, fee: Long, timestamp: Long)

  case class Status(status: String)

  case class NodeInfo(bestHeaderIdOpt: Option[String],
                      bestBlockIdOpt: Option[String],
                      bestHeaderHeightOpt: Option[Int],
                      bestBlockHeightOpt: Option[Int],
                      stateRootOpt: Option[String],
                      isMining: Option[Boolean])

  implicit val nodeInfoDecoder: Decoder[NodeInfo] = { c =>
    for {
      bestHeaderIdOpt <- c.downField("bestHeaderId").as[Option[String]]
      bestBlockIdOpt <- c.downField("bestFullHeaderId").as[Option[String]]
      bestHeaderHeightOpt <- c.downField("headersHeight").as[Option[Int]]
      bestBlockHeightOpt <- c.downField("fullHeight").as[Option[Int]]
      stateRootOpt <- c.downField("stateRoot").as[Option[String]]
      isMining <- c.downField("isMining").as[Option[Boolean]]
    } yield NodeInfo(bestHeaderIdOpt, bestBlockIdOpt, bestHeaderHeightOpt, bestBlockHeightOpt, stateRootOpt, isMining)
  }
}
