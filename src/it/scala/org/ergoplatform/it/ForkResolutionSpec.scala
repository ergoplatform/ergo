package org.ergoplatform.it

import java.io.File
import java.util.concurrent.atomic.AtomicBoolean
import com.typesafe.config.Config
import io.circe.Json
import org.ergoplatform.it.api.NodeApi.NodeInfo
import org.ergoplatform.it.container.Docker.{ExtraConfig, noExtraConfig}
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.ergoplatform.it.util.ConvergenceObservations
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Try
import scala.util.control.NonFatal

class ForkResolutionSpec extends AnyFlatSpec with Matchers with IntegrationSuite {

  import ForkResolutionDiagnostics._

  val nodesQty: Int = 4

  val commonChainLength: Int = 5
  val forkLength: Int = 5
  val syncLength: Int = 15

  val localVolumes: Seq[String] = (1 to nodesQty).map(localVolume)
  val remoteVolume = "/app"

  val volumesMapping: Seq[(String, String)] = localVolumes.map(_ -> remoteVolume)

  val dirs: Seq[File] = localVolumes.map(vol => new File(vol))
  dirs.foreach(_.mkdirs())

  val miningTimingConfig: Config = shortInternalMinerPollingInterval
    .withFallback(blockIntervalConfig(500))

  val nodeConfigs: List[Config] = nodeSeedConfigs.take(4)
    .map(_.withFallback(allowLocalConfig).withFallback(miningTimingConfig))

  val minerConfig: Config = nodeConfigs.head
  val onlineSyncNodesConfig: List[Config] = nodeConfigs.slice(1, nodesQty)
    .map(_.withFallback(nonGeneratingPeerConfig))
  val offlineMiningNodesConfig: List[Config] = nodeConfigs.slice(1, nodesQty)

  def localVolume(n: Int): String = s"$localDataDir/fork-resolution-spec/node-$n/data"

  private var phase = "initial-start"
  private var recent = Vector.empty[String]
  private val active = new AtomicBoolean(true)

  private def enter(next: String): Unit = synchronized {
    requireActive()
    phase = next
    log.info(s"Fork resolution phase=$phase")
  }

  private def remember(entry: String): Unit = synchronized {
    recent = (recent :+ entry).takeRight(nodesQty * 3)
  }

  private def evidence: String = synchronized { s"phase=$phase; ${recent.mkString("; ")}" }

  def clearPeerDatabases(): Unit = {
    volumesMapping.zipWithIndex.foreach { case ((localVolume, remoteVolume), index) =>
      requireActive()
      remember(s"phase=$phase node=$index operation=clear-peers")
      docker.removeFromMountedVolume(localVolume, remoteVolume, "peers")
    }
  }

  private def startNodesWithBinds(nodeConfigs: List[Config], observations: ConvergenceObservations,
                          deadline: Option[Deadline] = None,
                          configEnrich: ExtraConfig = noExtraConfig): List[Node] = {
    val nodes = nodeConfigs
      .map(_.withFallback(specialDataDirConfig(remoteVolume)))
      .zip(volumesMapping)
      .zipWithIndex.map { case ((cfg, vol), index) =>
        requireActive()
        deadline.foreach(d => requireTime(d))
        remember(s"phase=$phase node=$index operation=start")
        docker.startDevNetNode(cfg, configEnrich, Some(vol)).get
      }
    val startupDeadline = deadline.map(d => d.timeLeft.min(180.seconds).fromNow).getOrElse(180.seconds.fromNow)
    waitFor(nodes, observations, startupDeadline, None, None)(startupReached)
    nodes
  }

  private def requireTime(deadline: Deadline): Unit = {
    requireActive()
    if (deadline.isOverdue()) throw new java.util.concurrent.TimeoutException("Fork resolution deadline")
  }

  private def requireActive(): Unit = {
    if (!active.get()) throw new java.util.concurrent.CancellationException("Fork resolution finished")
  }

  private def waitFor[A](nodes: List[Node], observations: ConvergenceObservations, deadline: Deadline,
                         headerHeight: Option[Int], fixedSample: Option[String])
                        (accept: Snapshot => Option[A]): Vector[A] = {
    val currentPhase = phase
    val probes = nodes.map { node =>
      def json(path: String): Future[Json] = {
        requireTime(deadline)
        node.singleGet(path, _.setRequestTimeout(5000)).map { response =>
          require(response.getStatusCode == 200, "Unexpected observation status")
          node.ergoJsonAnswerAs[Json](response.getResponseBody)
        }
      }
      val status = observations.probe {
        requireTime(deadline)
        node.singleGet("/info", _.setRequestTimeout(5000)).map { response =>
          require(response.getStatusCode == 200, "Unexpected observation status")
          val parsed = Try(node.ergoJsonAnswerAs[Json](response.getResponseBody))
          val info = parsed.flatMap(j => Try(j.as[NodeInfo].fold(throw _, identity)))
            .toEither.left.map(_.getClass.getSimpleName)
          // Height gates retain NodeApi.fullHeight's exact field/default semantics.
          val height = parsed.flatMap(j => Try(j.hcursor.downField("fullHeight").as[Option[Int]]
            .fold(throw _, identity).getOrElse(0))).toEither.left.map(_.getClass.getSimpleName)
          (info, height)
        }
      }
      val peers = observations.probe(json("/peers/connected").map(_.asArray.get.size))
      val headers = headerHeight.map { height =>
        observations.probe(json(s"/blocks/at/$height").map(_.as[Seq[String]].fold(throw _, identity)))
      }
      (status, peers, headers)
    }
    var accepted = Vector.fill[Option[A]](nodes.size)(None)
    val result = observations.until(deadline, 100.millis, 5.seconds) { budget =>
      requireTime(deadline)
      Future.traverse(probes.zipWithIndex) { case ((status, peers, headers), index) =>
        val infoResult = status.sample(budget)
        val peerResult = peers.sample(budget)
        val headerResult = headers.map(_.sample(budget)).getOrElse(Future.successful(Right(Seq.empty[String])))
        infoResult.zip(peerResult).zip(headerResult).map { case ((status, peerCount), ids) =>
          val snapshot = Snapshot(status.flatMap(_._1), peerCount, ids,
            status.flatMap(_._2), status.isRight)
          remember(describe(currentPhase, index, headerHeight, fixedSample, snapshot) +
            s" sampledAt=${System.currentTimeMillis()}")
          accept(snapshot)
        }
      }.map { observed =>
        accepted = latch(accepted, observed.toVector)
        accepted
      }
    }(_.forall(_.isDefined))(s"Fork resolution did not complete; $evidence")
    Await.result(result, deadline.timeLeft.max(Duration.Zero)).map(_.get)
  }

  // Testing scenario:
  // 1. Start up {nodesQty} nodes and let them mine common chain of length {initialCommonChainLength};
  // 2. Kill all nodes when they are done, make them offline generating, clear known peers and restart them;
  // 3. Let them mine another {forkLength} blocks offline in order to create {nodesQty} forks;
  // 4. Kill all nodes again and restart with `knownPeers` filled, wait another {syncLength} blocks;
  // 5. Check that nodes reached consensus on created forks;
  it should "Fork resolution after isolated mining" in {

    val observations = new ConvergenceObservations
    try {
      enter("initial-start")
      val nodes = startNodesWithBinds(minerConfig +: onlineSyncNodesConfig, observations)
      // Match the original budget: initial startup precedes the 15-minute scenario deadline.
      val deadline = 15.minutes.fromNow
      val result = Future {
        enter("initial-height")
        val initMaxHeight = waitFor(nodes, observations, deadline, None, None)(_.fullHeight.toOption).max
        val forkHeight = initMaxHeight + commonChainLength + forkLength
        enter(s"common-height target=${initMaxHeight + commonChainLength}")
        waitFor(nodes, observations, deadline, Some(initMaxHeight + commonChainLength), None)(
          heightReached(initMaxHeight + commonChainLength))
        def stop(current: List[Node]): Unit = current.zipWithIndex.foreach { case (node, index) =>
          requireTime(deadline)
          remember(s"phase=$phase node=$index operation=stop")
          docker.stopNode(node.containerId)
        }
        enter("isolate-stop")
        stop(nodes)
        enter("isolate-clear-peers")
        clearPeerDatabases()
        enter("isolate-start")
        val isolatedNodes = startNodesWithBinds(minerConfig +: offlineMiningNodesConfig,
          observations, Some(deadline), isolatedPeersConfig)
        enter(s"isolated-height target=$forkHeight")
        waitFor(isolatedNodes, observations, deadline, Some(forkHeight), None)(heightReached(forkHeight))
        enter("reconnect-stop")
        stop(isolatedNodes)
        enter("reconnect-clear-peers")
        clearPeerDatabases()
        enter("reconnect-start")
        val regularNodes = startNodesWithBinds(minerConfig +: onlineSyncNodesConfig, observations, Some(deadline))
        enter(s"reconnected-height target=${forkHeight + syncLength}")
        waitFor(regularNodes, observations, deadline, Some(forkHeight), None)(heightReached(forkHeight + syncLength))
        enter("select-anchor")
        val sample = waitFor(regularNodes.take(1), observations, deadline, Some(forkHeight), None)(
          _.headers.toOption).head.headOption.value
        enter("match-anchor")
        val headers = waitFor(regularNodes, observations, deadline, Some(forkHeight), Some(sample))(
          _.headers.toOption.filter(_.headOption.contains(sample)))
        val headerIdsAtSameHeight = headers.map(_.headOption.value)
        headerIdsAtSameHeight should contain only sample
        log.info(s"Fork resolution completed; $evidence")
      }
      Await.result(result, deadline.timeLeft.max(Duration.Zero))
    } catch {
      case NonFatal(error) =>
        log.error(s"Fork resolution failed errorClass=${error.getClass.getSimpleName}; $evidence")
        throw error
    } finally {
      active.set(false)
      observations.close()
    }
  }

}

private[it] object ForkResolutionDiagnostics {
  final case class Snapshot(info: Either[String, NodeInfo], peers: Either[String, Int],
                            headers: Either[String, Seq[String]], fullHeight: Either[String, Int],
                            reachable: Boolean)

  def startupReached(snapshot: Snapshot): Option[Unit] = if (snapshot.reachable) Some(()) else None

  def heightReached(target: Int)(snapshot: Snapshot): Option[Int] =
    snapshot.fullHeight.toOption.filter(_ >= target)

  def latch[A](previous: Vector[Option[A]], observed: Vector[Option[A]]): Vector[Option[A]] =
    previous.zip(observed).map { case (accepted, current) => accepted.orElse(current) }

  def describe(phase: String, index: Int, height: Option[Int], fixedSample: Option[String], snapshot: Snapshot): String = {
    def error(value: String): String = if (value.matches("[A-Za-z0-9_$]+")) value else "ObservationError"
    val info = snapshot.info.fold(e => s"statusErrorClass=${error(e)}", value =>
      s"headerHeight=${value.bestHeaderHeightOpt} fullHeight=${value.bestBlockHeightOpt} mining=${value.isMining}")
    val peers = snapshot.peers.fold(e => s"peerErrorClass=${error(e)}", count => s"peerCount=$count")
    val selected = snapshot.headers.fold(e => s"headerErrorClass=${error(e)}", ids =>
      s"selected=${ids.headOption.map(ConvergenceObservations.headerId).getOrElse("missing")}")
    s"phase=$phase node=$index height=$height fixed=${fixedSample.map(ConvergenceObservations.headerId)} $info $peers $selected"
  }
}
