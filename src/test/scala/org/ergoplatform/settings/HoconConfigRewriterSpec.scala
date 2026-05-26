package org.ergoplatform.settings

import com.typesafe.config.ConfigFactory
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoNodeTestConstants.settings

import java.nio.charset.StandardCharsets
import java.nio.file.attribute.{PosixFileAttributeView, PosixFilePermissions}
import java.nio.file.{Files, Path}
import scala.concurrent.duration._

class HoconConfigRewriterSpec extends ErgoCorePropertyTest {

  private def writeTempConfig(content: String): Path = {
    val dir: Path = Files.createTempDirectory("ergo-config-test")
    val file = dir.resolve("ergo.conf")
    Files.write(file, content.getBytes(StandardCharsets.UTF_8))
    file
  }

  private def readFile(f: Path): String =
    new String(Files.readAllBytes(f), StandardCharsets.UTF_8)

  private def withMempoolCapacity(s: ErgoSettings, cap: Int): ErgoSettings =
    s.copy(nodeSettings = s.nodeSettings.copy(mempoolCapacity = cap))

  private val baseConfig: String =
    """# Original user config
      |ergo {
      |  node {
      |    mempoolCapacity = 1000
      |    minimalFeeAmount = 1000000
      |    mempoolCleanupDuration = 10s
      |  }
      |  voting {
      |    rulesToDisable = []
      |  }
      |}
      |""".stripMargin

  // Fixtures for the snapshot test. Two literal chunks so a reader can see exactly
  // what the rewriter is contracted to produce. Update both together if the override
  // block format changes intentionally.
  private val snapshotInput: String =
    """# Original user config — keep this comment
      |ergo {
      |  node {
      |    mempoolCapacity = 100
      |    minimalFeeAmount = 1000000
      |    mempoolCleanupDuration = 10s
      |  }
      |  voting {
      |    rulesToDisable = []
      |  }
      |}
      |""".stripMargin

  private val snapshotExpected: String =
    """# Original user config — keep this comment
      |ergo {
      |  node {
      |    mempoolCapacity = 100
      |    minimalFeeAmount = 1000000
      |    mempoolCleanupDuration = 10s
      |  }
      |  voting {
      |    rulesToDisable = []
      |  }
      |}
      |# >>> ergo-runtime-overrides v1 BEGIN <<<
      |# Written by the runtime config API. Hand edits will be overwritten.
      |ergo.node.mempoolCapacity = 5000
      |ergo.node.minimalFeeAmount = 2000000
      |ergo.node.mempoolCleanupDuration = 30000ms
      |ergo.voting."1" = 1250000
      |ergo.voting."4" = 1000000
      |ergo.voting.rulesToDisable = [215, 409]
      |# >>> ergo-runtime-overrides v1 END <<<
      |""".stripMargin

  property("appends override block on first write and is idempotent on second write") {
    val file = writeTempConfig(baseConfig)
    val rewriter = new HoconConfigRewriter(file)

    rewriter.writeOverrides(withMempoolCapacity(settings, 4242)) shouldBe Right(())
    val afterFirst = readFile(file)
    afterFirst should include(HoconConfigRewriter.BlockBegin)
    afterFirst should include("ergo.node.mempoolCapacity = 4242")
    val blockCountFirst = HoconConfigRewriter.BlockBegin.r.findAllIn(afterFirst).size
    blockCountFirst shouldBe 1

    rewriter.writeOverrides(withMempoolCapacity(settings, 4242)) shouldBe Right(())
    val afterSecond = readFile(file)
    val blockCountSecond = HoconConfigRewriter.BlockBegin.r.findAllIn(afterSecond).size
    blockCountSecond shouldBe 1
  }

  property("replaces existing override block on subsequent writes") {
    val file = writeTempConfig(baseConfig)
    val rewriter = new HoconConfigRewriter(file)

    rewriter.writeOverrides(withMempoolCapacity(settings, 1)) shouldBe Right(())
    rewriter.writeOverrides(withMempoolCapacity(settings, 2)) shouldBe Right(())
    val afterSecond = readFile(file)

    afterSecond should not include "ergo.node.mempoolCapacity = 1"
    afterSecond should include("ergo.node.mempoolCapacity = 2")
    HoconConfigRewriter.BlockBegin.r.findAllIn(afterSecond).size shouldBe 1
  }

  property("preserves the original user content above the override block") {
    val file = writeTempConfig(baseConfig)
    val rewriter = new HoconConfigRewriter(file)
    rewriter.writeOverrides(settings) shouldBe Right(())

    val updated = readFile(file)
    updated should include("# Original user config")
    updated should include("ergo {")
    updated.indexOf("# Original user config") should be < updated.indexOf(HoconConfigRewriter.BlockBegin)
  }

  property("rejects files containing include directives") {
    val file = writeTempConfig(
      """include "shared.conf"
        |ergo.node.mempoolCapacity = 1000
        |""".stripMargin
    )
    val rewriter = new HoconConfigRewriter(file)
    rewriter.writeOverrides(settings) match {
      case Left(_: PersistError.ConfigFileUnsupported) => succeed
      case other => fail(s"expected ConfigFileUnsupported, got $other")
    }
  }

  property("creates a .bak file on first write only") {
    val file = writeTempConfig(baseConfig)
    val bak = file.resolveSibling(file.getFileName.toString + ".bak")
    Files.exists(bak) shouldBe false

    val rewriter = new HoconConfigRewriter(file)
    rewriter.writeOverrides(withMempoolCapacity(settings, 1234)) shouldBe Right(())
    Files.exists(bak) shouldBe true
    val bakContent = readFile(bak)
    bakContent shouldEqual baseConfig

    rewriter.writeOverrides(withMempoolCapacity(settings, 5678)) shouldBe Right(())
    readFile(bak) shouldEqual baseConfig
  }

  property("written file parses back to the expected managed values") {
    val file = writeTempConfig(baseConfig)
    val updated = settings.copy(
      nodeSettings = settings.nodeSettings.copy(
        mempoolCapacity = 7777,
        minimalFeeAmount = 555000L,
        mempoolCleanupDuration = 15.seconds
      ),
      votingTargets = VotingTargets(
        targets = Map[Byte, Int]((1.toByte, 1000000), (120.toByte, 1)),
        desiredUpdate = ErgoValidationSettingsUpdate(Seq(215.toShort, 409.toShort), Seq())
      )
    )

    val rewriter = new HoconConfigRewriter(file)
    rewriter.writeOverrides(updated) shouldBe Right(())

    val cfg = ConfigFactory.parseString(readFile(file)).resolve()
    cfg.getInt("ergo.node.mempoolCapacity") shouldBe 7777
    cfg.getLong("ergo.node.minimalFeeAmount") shouldBe 555000L
    cfg.getDuration("ergo.node.mempoolCleanupDuration").toMillis shouldBe 15000L
    cfg.getInt("ergo.voting.\"1\"") shouldBe 1000000
    cfg.getInt("ergo.voting.\"120\"") shouldBe 1
    cfg.getIntList("ergo.voting.rulesToDisable").size() shouldBe 2
  }

  property("stripOverrideBlock removes the marked region but leaves surrounding text") {
    val text =
      s"""ergo.node.mempoolCapacity = 1000
         |${HoconConfigRewriter.BlockBegin}
         |ergo.node.mempoolCapacity = 999
         |${HoconConfigRewriter.BlockEnd}
         |""".stripMargin
    val stripped = HoconConfigRewriter.stripOverrideBlock(text)
    stripped should not include "999"
    stripped should include("mempoolCapacity = 1000")
    HoconConfigRewriter.BlockBegin.r.findAllIn(stripped).size shouldBe 0
  }

  property("containsIncludeDirective detects top-level include lines") {
    HoconConfigRewriter.containsIncludeDirective("ergo.node.mempoolCapacity = 1000") shouldBe false
    HoconConfigRewriter.containsIncludeDirective("include \"a.conf\"\nergo {}") shouldBe true
    HoconConfigRewriter.containsIncludeDirective("  include classpath(\"foo\")\n") shouldBe true
  }

  property("reports ConfigFileUnsupported when the config file does not exist") {
    val absent = Files.createTempDirectory("ergo-config-test").resolve("missing.conf")
    val rewriter = new HoconConfigRewriter(absent)
    rewriter.writeOverrides(settings) match {
      case Left(_: PersistError.ConfigFileUnsupported) => succeed
      case other => fail(s"expected ConfigFileUnsupported, got $other")
    }
  }

  property("produces exactly the expected file contents (snapshot)") {
    val file = writeTempConfig(snapshotInput)
    val patched = settings.copy(
      nodeSettings = settings.nodeSettings.copy(
        mempoolCapacity = 5000,
        minimalFeeAmount = 2000000L,
        mempoolCleanupDuration = 30.seconds
      ),
      votingTargets = VotingTargets(
        targets = Map[Byte, Int]((1.toByte, 1250000), (4.toByte, 1000000)),
        desiredUpdate = ErgoValidationSettingsUpdate(Seq(215.toShort, 409.toShort), Seq())
      )
    )

    val rewriter = new HoconConfigRewriter(file)
    rewriter.writeOverrides(patched) shouldBe Right(())

    readFile(file) shouldEqual snapshotExpected
  }

  property("reports ConfigFileUnsupported when the config file is not writable") {
    val file = writeTempConfig(baseConfig)
    val posixSupported = Files.getFileStore(file)
      .supportsFileAttributeView(classOf[PosixFileAttributeView])
    if (!posixSupported) cancel("requires a POSIX file system")

    Files.setPosixFilePermissions(file, PosixFilePermissions.fromString("r--r--r--"))
    // Some environments (e.g. root in CI containers) bypass POSIX read-only.
    if (Files.isWritable(file)) cancel("running as a user that ignores POSIX read-only")

    val rewriter = new HoconConfigRewriter(file)
    rewriter.writeOverrides(settings) match {
      case Left(_: PersistError.ConfigFileUnsupported) => succeed
      case other => fail(s"expected ConfigFileUnsupported, got $other")
    }
  }
}
