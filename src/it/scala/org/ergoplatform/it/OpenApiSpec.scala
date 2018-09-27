package org.ergoplatform.it

import com.typesafe.config.Config
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.{FreeSpec, TryValues}

class OpenApiSpec extends FreeSpec with IntegrationSuite with TryValues {

  val offlineGenerationNode: Config = onlineGeneratingPeerConfig.withFallback(nodeSeedConfigs.head)

  val node: Node = docker.startNode(offlineGenerationNode).success.value

  "OpenApi specification check" in {}
}
