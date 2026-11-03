package org.ergoplatform.settings

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File
import java.nio.file.{Files, Paths}
import scala.concurrent.duration._
import scala.sys.process.{Process, ProcessLogger}

class ErgoSettingsReaderPrecedenceSpec extends AnyFlatSpec with Matchers {
  "The settings reader" should "resolve bundled mainnet settings without a user file" in {
    val application = new File("src/main/resources/application.conf").getCanonicalFile
    application.isFile shouldBe true
    val javaExecutable = Paths.get(
      System.getProperty("java.home"),
      "bin",
      if (scala.util.Properties.isWin) "java.exe" else "java"
    ).toString

    Seq(("mainnet", 9053, 9030, 4900000)).foreach { case (network, restPort, p2pPort, transactionCost) =>
      // The reader copies each network to a fixed filename in java.io.tmpdir.
      // A private child directory also isolates its shutdown hooks from other tests.
      val temporaryDirectory = Files.createTempDirectory("ergo-settings-reader-").toFile
      temporaryDirectory.deleteOnExit()
      val output = new java.lang.StringBuffer
      val logger = ProcessLogger(line => {
        output.append(line).append(System.lineSeparator())
        ()
      })
      val exitCode = Process(Seq(
        javaExecutable,
        s"-Djava.io.tmpdir=${temporaryDirectory.getAbsolutePath}",
        // Select the bundled application directly: test application.conf shadows it
        // on the normal test classpath and must not supply this regression's defaults.
        s"-Dconfig.file=${application.getAbsolutePath}",
        "-cp",
        System.getProperty("java.class.path"),
        "org.ergoplatform.settings.ErgoSettingsReaderPrecedenceProbe",
        network,
        restPort.toString,
        p2pPort.toString,
        transactionCost.toString
      )).!(logger)

      withClue(s"$network: $output") {
        exitCode shouldBe 0
        output.toString should include(s"SETTINGS_PRECEDENCE_ACCEPTED $network")
      }
    }
  }
}

object ErgoSettingsReaderPrecedenceProbe {
  def main(args: Array[String]): Unit = {
    val network = NetworkType.fromString(args(0)).get
    val networkArgs = Args(None, Some(network))
    val defaults = ErgoSettingsReader.read(networkArgs)
    require(defaults.networkType == network)
    require(defaults.scorexSettings.network.bindAddress.getPort == args(2).toInt)
    require(defaults.nodeSettings.maxTransactionCost == args(3).toInt)
    require(defaults.nodeSettings.maxTransactionSize == 98304)
    require(defaults.scorexSettings.restApi.bindAddress.getHostString == "127.0.0.1")
    require(defaults.scorexSettings.restApi.bindAddress.getPort == args(1).toInt)
    require(defaults.scorexSettings.restApi.apiKeyHash.isEmpty)
    require(defaults.scorexSettings.restApi.timeout == 5.seconds)

    println(s"SETTINGS_PRECEDENCE_ACCEPTED ${network.verboseName}")
  }
}
