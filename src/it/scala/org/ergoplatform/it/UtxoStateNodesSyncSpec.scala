package org.ergoplatform.it

import com.typesafe.config.Config
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.ergoplatform.it.util.{ConvergenceObservations, NoProgressException, StallWatch}
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.concurrent.TrieMap
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class UtxoStateNodesSyncSpec extends AnyFlatSpec with IntegrationSuite {

  // Three nodes mine, so their first blocks can fork. The forks heal only if peers get
  // mined blocks quickly (#2414). The check is far above the height at which every node
  // has applied a block, and a few blocks below the tips.
  val blocksQty = 30

  val tipMargin: Int      = 3
  val minerConfig: Config = nodeSeedConfigs.head

  val nonGeneratingConfig: Config =
    nonGeneratingPeerConfig.withFallback(nodeSeedConfigs(1))

  val onlineGeneratingConfigs: List[Config] =
    nodeSeedConfigs.slice(2, 4).map(onlineGeneratingPeerConfig.withFallback)

  val nodeConfigs: List[Config] =
    (minerConfig +: nonGeneratingConfig +: onlineGeneratingConfigs)
      .map(_.withFallback(allowLocalConfig))

  val nodes: List[Node] = docker.startDevNetNodes(nodeConfigs).get

  // For a node, the id of the block at `height` on its best full-block chain, if that
  // chain reaches `height`. The best header can follow another branch for a moment,
  // while the blocks of that branch are still downloading. A block's ancestor at `height`
  // never changes and is the same on every node, so one cache keeps it for every block
  // walked: a later call walks only the blocks added since.
  private def fullChainHeaderIdAt(height: Int): Node => Future[Option[String]] = {
    val walked = TrieMap.empty[String, Option[String]]
    def walk(node: Node, id: String): Future[Option[String]] = walked.get(id) match {
      case Some(known) => Future.successful(known)
      case None =>
        node.headerById(id).flatMap { header =>
          val found =
            if (header.height > height) walk(node, header.parentId)
            else if (header.height == height) Future.successful(Some(id))
            else Future.successful(None) // the full tip is below `height`
          found.map { result =>
            walked.put(id, result)
            result
          }
        }
    }
    node =>
      node.info.flatMap { info =>
        info.bestBlockIdOpt.fold(Future.successful(Option.empty[String]))(walk(node, _))
      }
  }

  it should s"Utxo state nodes synchronisation ($blocksQty blocks)" in {
    val deadline = 15.minutes.fromNow
    val observations = new ConvergenceObservations
    @volatile var recent = Vector.empty[String]
    val stall = new StallWatch[Seq[String]]()
    val result = for {
      // an online miner starts mining after it applies a block, once its headers are less
      // than 6 ahead
      initHeight <- Future.traverse(nodes)(_.waitForHeight(1)).map(_.max)
      _          <- Future.traverse(nodes)(_.waitForHeight(initHeight + blocksQty))
      _ <- {
        val height            = initHeight + blocksQty - tipMargin
        val fullChainHeaderId = fullChainHeaderIdAt(height) // one cache for all samples
        val probes = nodes.map { node =>
          observations.probe(fullChainHeaderId(node).map(_.toSeq))
        }
        observations.until(deadline, 1.second, 5.seconds) { budget =>
          Future.traverse(probes.zipWithIndex) { case (probe, index) =>
            probe.sample(budget).map { result =>
              val selection = result.fold(error => s"errorClass=$error", ids =>
                ids.headOption.map(ConvergenceObservations.headerId).getOrElse("missing"))
              synchronized {
                recent = (recent :+ s"node$index height=$height fullChain=$selection " +
                  s"sampledAt=${System.currentTimeMillis()}")
                  .takeRight(nodes.size * 3)
              }
              result.toOption.getOrElse(Seq.empty)
            }
          }.flatMap { ids =>
            // a failed probe yields no ids, and a sample with a gap is never progress
            val complete = ids.forall(_.nonEmpty)
            val quiet    = stall.record(if (complete) Some(ids.map(_.head)) else None)
            if (!ConvergenceObservations.selectedHeadersAgree(ids) &&
                quiet >= StallWatch.DefaultLimit) {
              docker.describeNodes(nodes).flatMap { described =>
                Future.failed(new NoProgressException(
                  s"Full-chain blocks at height $height do not all agree (a failed " +
                    s"probe counts as no answer) and none changed for " +
                    s"${quiet.toSeconds} s; nodes: $described; " +
                    s"recent observations: ${recent.mkString("; ")}"))
              }
            } else {
              Future.successful(ids)
            }
          }
        }(ConvergenceObservations.selectedHeadersAgree)(
          s"Full-chain blocks did not converge at height $height; " +
            s"recent observations: ${recent.mkString("; ")}")
      }
    } yield {
      // `until` returns only a sample in which the full-chain blocks of all nodes agree
      log.info(s"Full-chain convergence (longest quiet period " +
        s"${stall.longestQuietPeriod.toMillis} ms): ${recent.mkString("; ")}")
    }
    try Await.result(result, deadline.timeLeft.max(Duration.Zero))
    catch {
      case error: java.util.concurrent.TimeoutException =>
        log.error(s"UTXO synchronization timed out; recent observations: ${recent.mkString("; ")}")
        throw error
    } finally observations.close()
  }

}
