package org.ergoplatform.it

import com.typesafe.config.Config
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.ergoplatform.it.util.ConvergenceObservations
import org.scalatest.flatspec.AnyFlatSpec

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class UtxoStateNodesSyncSpec extends AnyFlatSpec with IntegrationSuite {

  val blocksQty = 5

  val forkDepth: Int      = blocksQty
  val minerConfig: Config = nodeSeedConfigs.head

  val nonGeneratingConfig: Config =
    nonGeneratingPeerConfig.withFallback(nodeSeedConfigs(1))

  val onlineGeneratingConfigs: List[Config] =
    nodeSeedConfigs.slice(2, 4).map(onlineGeneratingPeerConfig.withFallback)

  val nodeConfigs: List[Config] =
    (minerConfig +: nonGeneratingConfig +: onlineGeneratingConfigs)
      .map(_.withFallback(allowLocalConfig))

  val nodes: List[Node] = docker.startDevNetNodes(nodeConfigs).get

  it should s"Utxo state nodes synchronisation ($blocksQty blocks)" in {
    val deadline = 15.minutes.fromNow
    val observations = new ConvergenceObservations
    @volatile var recent = Vector.empty[String]
    val result = for {
      initHeight <- Future.traverse(nodes)(_.fullHeight).map(x => math.max(x.max, 1))
      _          <- Future.traverse(nodes)(_.waitForHeight(initHeight + blocksQty))
      headers <- {
        val height = initHeight + blocksQty - forkDepth
        val probes = nodes.map { node =>
          observations.probe(node.singleGet(s"/blocks/at/$height", _.setRequestTimeout(5000))
            .map { r =>
              require(r.getStatusCode == 200, "Unexpected observation status")
              node.ergoJsonAnswerAs[Seq[String]](r.getResponseBody)
            })
        }
        observations.until(deadline, 1.second, 5.seconds) { budget =>
          Future.traverse(probes.zipWithIndex) { case (probe, index) =>
            probe.sample(budget).map { result =>
              val selection = result.fold(error => s"errorClass=$error", ids =>
                ids.headOption.map(ConvergenceObservations.headerId).getOrElse("missing"))
              synchronized {
                recent = (recent :+ s"node$index height=$height selected=$selection sampledAt=${System.currentTimeMillis()}")
                  .takeRight(nodes.size * 3)
              }
              result.toOption.getOrElse(Seq.empty)
            }
          }
        }(ConvergenceObservations.selectedHeadersAgree)(
          s"Selected headers did not converge at height $height; recent observations: ${recent.mkString("; ")}")
      }
    } yield {
      log.info(s"Selected header convergence: ${recent.mkString("; ")}")
      // `/blocks/at/{height}` returns *every* header id known at the given height, with the
      // best-chain one first (see `HeadersProcessor.headerIdsAtHeight`). Nodes are in sync
      // when their best-chain header at that height matches; a node may legitimately also
      // know orphaned headers of a fork that was already resolved, so flattening all the
      // returned ids makes the assertion fail on a perfectly synchronised network.
      // Same convention as ForkResolutionSpec and DeepRollBackSpec, which compare `.head`.
      headers.foreach(_ should not be empty)
      val bestChainHeaderIds = headers.map(_.head)
      val sample             = bestChainHeaderIds.head
      bestChainHeaderIds should contain only sample
    }
    try Await.result(result, deadline.timeLeft.max(Duration.Zero))
    catch {
      case error: java.util.concurrent.TimeoutException =>
        log.error(s"UTXO synchronization timed out; recent observations: ${recent.mkString("; ")}")
        throw error
    } finally observations.close()
  }

}
