package org.ergoplatform.http

import com.typesafe.config.ConfigFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HttpParsingSettingsSpec extends AnyFlatSpec with Matchers {

  "Akka HTTP parser settings" should "log malformed requests as single-line summaries" in {
    val config = ConfigFactory.defaultApplication()
      .withFallback(ConfigFactory.defaultReference())
      .resolve()

    val effectiveServerParsing = config
      .getConfig("akka.http.server.parsing")
      .withFallback(config.getConfig("akka.http.parsing"))
      .resolve()

    effectiveServerParsing.getString("error-logging-verbosity") shouldBe "simple"
  }
}
