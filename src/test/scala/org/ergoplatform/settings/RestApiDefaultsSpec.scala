package org.ergoplatform.settings

import com.typesafe.config.ConfigFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

class RestApiDefaultsSpec extends AnyFlatSpec with Matchers {
  private val application = ConfigFactory.parseFile(new File("src/main/resources/application.conf"))

  "Bundled REST profiles" should "use loopback and leave protected routes disabled" in {
    Seq("application" -> 9052, "mainnet" -> 9053, "testnet" -> 9052, "devnet" -> 9052).foreach {
      case (profile, port) =>
        val config = ConfigFactory.parseFile(new File(s"src/main/resources/$profile.conf"))
          .withFallback(application)
        withClue(profile) {
          config.getString("scorex.restApi.bindAddress") shouldBe s"127.0.0.1:$port"
          config.getIsNull("scorex.restApi.apiKeyHash") shouldBe true
        }
    }
  }

  it should "allow explicit operator settings to override the bundled defaults" in {
    val config = ConfigFactory.parseString(
      """scorex.restApi { bindAddress = "0.0.0.0:9053", apiKeyHash = "operator-hash" }"""
    ).withFallback(application)
    config.getString("scorex.restApi.bindAddress") shouldBe "0.0.0.0:9053"
    config.getString("scorex.restApi.apiKeyHash") shouldBe "operator-hash"
  }
}
