package org.ergoplatform.http

import akka.http.scaladsl.settings.{ParserSettings, ServerSettings}
import com.typesafe.config.ConfigFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HttpParsingSettingsSpec extends AnyFlatSpec with Matchers {

  "application.conf" should "configure akka.http.parsing.error-logging-verbosity = simple" in {
    val config = ConfigFactory.load()
    config.getString("akka.http.parsing.error-logging-verbosity") shouldBe "simple"

    val parserSettings = ServerSettings(config).parserSettings
    parserSettings.errorLoggingVerbosity shouldBe ParserSettings.ErrorLoggingVerbosity.Simple
    parserSettings.maxUriLength shouldBe 8192
  }

}
