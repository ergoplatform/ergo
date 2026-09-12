package org.ergoplatform.it

import java.io.File
import java.util.concurrent.TimeoutException
import akka.japi.Option.Some
import com.typesafe.config.Config
import io.circe.Json
import org.apache.commons.io.FileUtils
import org.ergoplatform.it.api.NodeApi.NodeInfo
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.ergoplatform.it.util.ConvergenceObservations
import org.scalatest.flatspec.AnyFlatSpec

import scala.async.Async
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class StateRecoveryDigestNodeSpec extends AnyFlatSpec with IntegrationSuite {

  val approxMinerTargetHeight = 20
  val approxFollowerTargetHeight: Int = approxMinerTargetHeight + 5

  val minerLocalVolume = s"$localDataDir/state-recovery-spec/miner/data"
  val followerLocalVolume = s"$localDataDir/state-recovery-spec/follower/data"
  val remoteVolume = "/app"

  val dir = new File(minerLocalVolume)
  dir.mkdirs()

  val minerConfig: Config = nodeSeedConfigs.head
    .withFallback(shortInternalMinerPollingInterval)
    .withFallback(specialDataDirConfig(remoteVolume))
    .withFallback(allowLocalConfig)

  val followerConfig: Config = digestStatePeerConfig
    .withFallback(blockIntervalConfig(10000))
    .withFallback(nonGeneratingPeerConfig)
    .withFallback(nodeSeedConfigs(1))
    .withFallback(specialDataDirConfig(remoteVolume))
    .withFallback(allowLocalConfig)

  //  Testing scenario:
  // 1. Start up one node and let it mine {approxMinerTargetHeight} blocks;
  // 2. Shut it down and copy its history to testing node's directory;
  // 3. Start mining node again;
  // 4. Start testing node and wait until it gets synced with the mining node + {approxFollowerTargetHeight}
  //    - it would require testing node to recover state correctly and apply new blocks on top of it;
  it should "Startup with only history available" in {

    val minerNode: Node = docker.startDevNetNode(minerConfig, specialVolumeOpt = Some((minerLocalVolume, remoteVolume))).get

    val deadline = 10.minutes.fromNow
    val observations = new ConvergenceObservations
    val observationLock = new Object
    var phase = "initial-miner-height"
    var recent = Vector.empty[String]
    var closed = false
    val probes = scala.collection.mutable.Map.empty[
      Node, (observations.Probe[NodeInfo], observations.Probe[Int])]

    def timeoutMessage: String = observationLock.synchronized {
      s"Digest startup timed out in phase=$phase; ${recent.mkString("; ")}"
    }

    def enterPhase(next: String): Unit = observationLock.synchronized {
      if (closed || deadline.isOverdue()) throw new TimeoutException(timeoutMessage)
      phase = next
    }

    def remember(label: String, endpoint: String, summary: String): Unit = observationLock.synchronized {
      if (!closed) {
        val entry = s"phase=$phase node=$label endpoint=$endpoint sampledAt=${System.currentTimeMillis()} $summary"
        recent = (recent :+ entry).takeRight(12)
        log.info(entry)
      }
    }

    def request[A](execute: Int => Future[A]): Future[A] = observationLock.synchronized {
      if (closed || deadline.isOverdue()) Future.failed(new TimeoutException("Observation deadline"))
      else execute(math.max(1L, deadline.timeLeft.min(5.seconds).toMillis).toInt)
    }

    def snapshot(label: String, node: Node, budget: FiniteDuration): Future[Option[NodeInfo]] = {
      val (statusProbe, peerProbe) = probes.getOrElseUpdate(node, (
        observations.probe(request { timeout =>
          node.singleGet("/info", _.setRequestTimeout(timeout)).map { response =>
            require(response.getStatusCode == 200, "Unexpected observation status")
            node.ergoJsonAnswerAs[NodeInfo](response.getResponseBody)
          }
        }),
        observations.probe(request { timeout =>
          node.singleGet("/peers/connected", _.setRequestTimeout(timeout)).map { response =>
            require(response.getStatusCode == 200, "Unexpected observation status")
            node.ergoJsonAnswerAs[Json](response.getResponseBody).asArray.getOrElse(
              throw new IllegalArgumentException("Expected peer array")).size
          }
        })
      ))
      val status = statusProbe.sample(budget).map {
        case Right(info) =>
          val summary = Json.obj(
            "headersHeight" -> info.bestHeaderHeightOpt.map(Json.fromInt).getOrElse(Json.Null),
            "fullHeight" -> info.bestBlockHeightOpt.map(Json.fromInt).getOrElse(Json.Null),
            "bestHeaderId" -> info.bestHeaderIdOpt.map(ConvergenceObservations.headerId).map(Json.fromString).getOrElse(Json.Null),
            "bestFullHeaderId" -> info.bestBlockIdOpt.map(ConvergenceObservations.headerId).map(Json.fromString).getOrElse(Json.Null)
          )
          remember(label, "status", summary.noSpaces)
          Option(info)
        case Left(error) =>
          remember(label, "status", s"errorClass=$error")
          None
      }
      val peers = peerProbe.sample(budget).map { result =>
        remember(label, "peers", result.fold(error => s"errorClass=$error", count => s"connectedPeerCount=$count"))
      }
      status.zip(peers).map(_._1)
    }

    try {
      val result = Async.async {
        Async.await(minerNode.waitForHeight(approxMinerTargetHeight))
        enterPhase("stop-initial-miner")
        docker.stopNode(minerNode, secondsToWait = 0)

        enterPhase("copy-history")
        FileUtils.copyDirectoryToDirectory(new File(s"$minerLocalVolume/history"), new File(followerLocalVolume))

        enterPhase("restart-miner")
        val nodeForSyncing: Node = docker
          .startDevNetNode(minerConfig, specialVolumeOpt = Some((minerLocalVolume, remoteVolume))).get
        enterPhase("restarted-miner-height")
        Async.await(nodeForSyncing.waitForHeight(approxMinerTargetHeight + 2))

        enterPhase("start-digest-follower")
        val followerNode: Node = docker
          .startDevNetNode(followerConfig, specialVolumeOpt = Some((followerLocalVolume, remoteVolume))).get
        enterPhase("digest-follower-height")
        Async.await(observations.until(deadline, 1.second, 5.seconds) { budget =>
          snapshot("miner", nodeForSyncing, budget).zip(snapshot("follower", followerNode, budget))
        }(_._2.exists(_.bestBlockHeightOpt.exists(_ >= approxFollowerTargetHeight)))(timeoutMessage))
      }

      Await.result(result, deadline.timeLeft.max(Duration.Zero))
    } catch {
      case _: TimeoutException => throw new TimeoutException(timeoutMessage)
    } finally {
      observationLock.synchronized { closed = true }
      observations.close()
    }
  }

}
