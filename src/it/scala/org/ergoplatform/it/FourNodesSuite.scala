package org.ergoplatform.it

import org.scalatest.Suite

import scala.collection.immutable.IndexedSeq

class FourNodesSuite extends IntegrationSuite {

  private val nodesCount = 4

  protected val nodeConfigs = {
    val src = Docker.nodeConfigs.take(nodesCount)
    src.head +:
      nonGeneratingPeerConfig.withFallback(src.tail.head) +:
      src.tail.tail.map(onlineGeneratingPeerConfig.withFallback)
  }

  protected def nodeSuites(nodes: Seq[Node]): IndexedSeq[Suite] = IndexedSeq(
    new NodesSynchronizationSpec(nodes)
  )

}
