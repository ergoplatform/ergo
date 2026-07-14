package org.ergoplatform.it

import com.typesafe.config.ConfigFactory
import org.ergoplatform.it.api.NodeApi.IntegrationTestApiKey
import org.ergoplatform.tools.ApiKeyHash
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.core.api.http.ApiDirectives.{DisabledApiKeyHash, LegacyDefaultApiKeyHash}

import java.io.File

class RestApiFixtureSpec extends AnyFlatSpec with Matchers {

  "Integration REST API fixture" should "use a dedicated non-default API key" in {
    val configuredHash = ConfigFactory
      .parseFile(new File("src/it/resources/devnetTemplate.conf"))
      .getString("scorex.restApi.apiKeyHash")

    configuredHash shouldBe ApiKeyHash.hash(IntegrationTestApiKey)
    configuredHash should not be DisabledApiKeyHash
    configuredHash should not be LegacyDefaultApiKeyHash
  }
}
