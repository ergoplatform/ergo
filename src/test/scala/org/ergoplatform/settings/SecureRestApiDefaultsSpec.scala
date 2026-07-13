package org.ergoplatform.settings

import com.typesafe.config.{Config, ConfigFactory}
import org.ergoplatform.tools.ApiKeyHash
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.core.api.http.ApiDirectives.{DisabledApiKeyHash, LegacyDefaultApiKeyHash}

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

class SecureRestApiDefaultsSpec extends AnyFlatSpec with Matchers {

  private val restApiPrefix = "scorex.restApi"

  private val applicationConfig =
    ConfigFactory.parseFile(new File("src/main/resources/application.conf"))

  private val mainnetConfig =
    ConfigFactory
      .parseFile(new File("src/main/resources/mainnet.conf"))
      .withFallback(applicationConfig)

  private val testnetConfig =
    ConfigFactory
      .parseFile(new File("src/main/resources/testnet.conf"))
      .withFallback(applicationConfig)

  private val devnetConfig =
    ConfigFactory
      .parseFile(new File("src/main/resources/devnet.conf"))
      .withFallback(applicationConfig)

  private val releaseBundleConfig =
    ConfigFactory.parseFile(new File("ci/ergo.conf")).withFallback(mainnetConfig)

  private def assertSafeDefaults(config: Config, expectedPort: Int): Unit = {
    config.getString(s"$restApiPrefix.bindAddress") shouldBe s"127.0.0.1:$expectedPort"
    config.getString(s"$restApiPrefix.apiKeyHash") shouldBe DisabledApiKeyHash
  }

  "REST API defaults" should "bind to loopback and disable protected routes" in {
    assertSafeDefaults(applicationConfig, 9052)
    assertSafeDefaults(mainnetConfig, 9053)
    assertSafeDefaults(testnetConfig, 9052)
    assertSafeDefaults(devnetConfig, 9052)
  }

  it should "keep release-bundle fallback settings fail-closed" in {
    assertSafeDefaults(releaseBundleConfig, 9053)
    releaseBundleConfig.getBoolean("ergo.node.mining") shouldBe false
  }

  it should "allow an operator to explicitly configure a non-default key and external bind" in {
    val operatorHash = "1" * 64
    val operatorConfig = ConfigFactory
      .parseString(
        s"""
           |scorex.restApi {
           |  bindAddress = "0.0.0.0:9053"
           |  apiKeyHash = "$operatorHash"
           |}
           |""".stripMargin
      )
      .withFallback(mainnetConfig)

    operatorConfig.getString(s"$restApiPrefix.bindAddress") shouldBe "0.0.0.0:9053"
    operatorConfig.getString(s"$restApiPrefix.apiKeyHash") shouldBe operatorHash
    operatorConfig.getString(s"$restApiPrefix.apiKeyHash") should not be LegacyDefaultApiKeyHash
  }

  it should "hash the installer API key offline without exposing it as a Java argument" in {
    val installer = new String(
      Files.readAllBytes(Paths.get("ergo-installer.sh")),
      StandardCharsets.UTF_8
    )

    installer should include(
      "printf '%s' \"${API_KEY}\" | java -cp \"${APP_DIR}/ergo.jar\" " +
      "org.ergoplatform.tools.ApiKeyHash"
    )
    installer should include("stty -echo < /dev/tty")
    installer should include("stty \"${TTY_STATE}\" < /dev/tty")
    installer should include("IFS= read -r API_KEY < /dev/tty")
    installer should include("b2sum -l 256")
    installer should include("if [ " + "$" + "{#API_KEY_HASH} -ne 64 ]")
    installer should not include "${APP_KEY}"
    installer should not include "/utils/hash/blake2b"
    installer should not include LegacyDefaultApiKeyHash
  }

  it should "place JVM options before the release-bundle jar argument" in {
    val releaseGenerator = new String(
      Files.readAllBytes(Paths.get("ci/release-binaries.py")),
      StandardCharsets.UTF_8
    )

    releaseGenerator should include(" -Xmx4G -jar ")
    releaseGenerator should not include " -jar -Xmx4G "
  }

  it should "calculate the same API key hash as the REST utility" in {
    ApiKeyHash.hash("hello") shouldBe LegacyDefaultApiKeyHash
  }
}
