package org.ergoplatform.it

import com.typesafe.config.Config
import io.circe.Json
import io.circe.parser._
import io.circe.syntax._
import org.ergoplatform.it.api.NodeApi.UnexpectedStatusCodeException
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.ergoplatform.utils.ErgoTestHelpers
import org.scalatest.wordspec.AsyncWordSpec

import scala.concurrent.ExecutionContext


class WalletSpec extends AsyncWordSpec with IntegrationSuite {

  override implicit def executionContext: ExecutionContext = ErgoTestHelpers.defaultExecutionContext

  private val nodeConfig: Config = nonGeneratingPeerConfig.withFallback(nodeSeedConfigs.head)
  private val node: Node = docker.startDevNetNode(nodeConfig, sequentialTopologyConfig).get

  "it should be initialized with testMnemonic" in {
    node.waitForStartup.flatMap { node: Node =>
      node.getWihApiKey("/wallet/status")
    }.map { response =>
      val body = parse(response.getResponseBody)
      body.flatMap(_.hcursor.downField("isInitialized").as[Boolean]) shouldBe Right(true)
      body.flatMap(_.hcursor.downField("isUnlocked").as[Boolean]) shouldBe Right(true)
      body.flatMap(_.hcursor.downField("walletHeight").as[Int]) shouldBe Right(0)
    }
  }

  "initializing already initialized wallet should fail" in {
    node.waitForStartup.flatMap { node: Node =>
      recoverToExceptionIf[UnexpectedStatusCodeException] {
        node.postJson("/wallet/init", Json.obj("pass" -> "foo".asJson))
      }.map { ex =>
        ex.response.getStatusCode shouldBe 400
        ex.response.getResponseBody should include("Wallet is already initialized")
      }
    }
  }

  "restoring initialized wallet should fail" in {
    node.waitForStartup.flatMap { node: Node =>
      recoverToExceptionIf[UnexpectedStatusCodeException] {
        node.postJson("/wallet/restore", Json.obj("pass" -> "foo".asJson, "mnemonic" -> "bar".asJson))
      }.map { ex =>
        ex.response.getStatusCode shouldBe 400
        ex.response.getResponseBody should include("Wallet is already initialized")
      }
    }
  }

}
