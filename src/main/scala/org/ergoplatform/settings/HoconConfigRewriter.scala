package org.ergoplatform.settings

import com.typesafe.config.ConfigFactory
import scorex.util.ScorexLogging

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import scala.util.Try

/**
  * Rewrites the user's HOCON configuration file by appending (or replacing) a
  * sentinel-delimited "runtime overrides" block at the end of the file. The
  * block is regenerated from a full [[ErgoSettings]] snapshot on every write,
  * so the file is always self-consistent with the in-memory state.
  *
  * v1 limitations (reported as `Left(PersistError.ConfigFileUnsupported)`):
  *   - file does not exist or is not writable
  *   - file contains HOCON `include` directives (scope unclear; not safe to append)
  */
class HoconConfigRewriter(configFile: Path) extends ScorexLogging {
  import HoconConfigRewriter._

  def writeOverrides(settings: ErgoSettings): Either[PersistError, Unit] = {
    val checks: Either[PersistError, Unit] =
      if (!Files.exists(configFile))
        Left(PersistError.ConfigFileUnsupported(s"config file does not exist: $configFile"))
      else if (!Files.isWritable(configFile))
        Left(PersistError.ConfigFileUnsupported(s"config file is not writable: $configFile"))
      else
        Right(())

    checks.flatMap { _ =>
      catchIo(readFile(configFile)).flatMap { originalText =>
        if (containsIncludeDirective(originalText)) {
          Left(PersistError.ConfigFileUnsupported(
            "config file contains `include` directives; runtime overrides are not supported in v1"
          ))
        } else {
          val withoutOldBlock = stripOverrideBlock(originalText)
          val overrideBlock = buildOverrideBlock(settings)
          val newText = ensureTrailingNewline(withoutOldBlock) + overrideBlock + "\n"

          catchIo {
            // Post-flight: parse the override block in isolation and verify managed keys round-trip.
            // verifyOverrideBlock throws IllegalArgumentException via require(); we catch as IoFailure
            // because reaching this point means the file passed all preconditions.
            verifyOverrideBlock(overrideBlock, settings)
            backupOnce(configFile)
            val tmp = configFile.resolveSibling(configFile.getFileName.toString + ".tmp")
            Files.write(tmp, newText.getBytes(StandardCharsets.UTF_8))
            Files.move(
              tmp,
              configFile,
              StandardCopyOption.ATOMIC_MOVE,
              StandardCopyOption.REPLACE_EXISTING
            )
            log.info(s"Wrote runtime-override block to ${configFile.getFileName}")
          }
        }
      }
    }
  }
}

object HoconConfigRewriter {
  val BlockBegin: String = "# >>> ergo-runtime-overrides v1 BEGIN <<<"
  val BlockEnd: String = "# >>> ergo-runtime-overrides v1 END <<<"

  /** Convenience for callers that hold a path as a string. */
  def fromPathString(path: String): HoconConfigRewriter =
    new HoconConfigRewriter(Paths.get(path))

  private val includeRe = """(?m)^\s*include\b""".r

  def containsIncludeDirective(text: String): Boolean =
    includeRe.findFirstIn(text).isDefined

  /** Idempotently removes a previously written override block. */
  def stripOverrideBlock(text: String): String = {
    val beginIdx = text.indexOf(BlockBegin)
    if (beginIdx < 0) {
      text
    } else {
      val endIdx = text.indexOf(BlockEnd, beginIdx)
      val before = text.substring(0, beginIdx).reverse.dropWhile(c => c == ' ' || c == '\t').reverse
      val trimmedBefore = if (before.endsWith("\n")) before.dropRight(1) else before
      if (endIdx < 0) {
        // Malformed: strip from begin marker through end of file.
        trimmedBefore
      } else {
        val after = text.substring(endIdx + BlockEnd.length)
        val afterTrimmed = if (after.startsWith("\n")) after.drop(1) else after
        if (afterTrimmed.isEmpty) trimmedBefore else trimmedBefore + "\n" + afterTrimmed
      }
    }
  }

  def buildOverrideBlock(settings: ErgoSettings): String = {
    val lines = scala.collection.mutable.ArrayBuffer.empty[String]
    lines += BlockBegin
    lines += "# Written by the runtime config API. Hand edits will be overwritten."
    lines += s"ergo.node.mempoolCapacity = ${settings.nodeSettings.mempoolCapacity}"
    lines += s"ergo.node.minimalFeeAmount = ${settings.nodeSettings.minimalFeeAmount}"
    lines += s"ergo.node.mempoolCleanupDuration = ${settings.nodeSettings.mempoolCleanupDuration.toMillis}ms"

    settings.votingTargets.targets.toSeq
      .sortBy { case (id, _) => id.toInt & 0xFF }
      .foreach { case (id, value) =>
        lines += s"""ergo.voting."${id.toInt & 0xFF}" = $value"""
      }

    val rulesToDisable = settings.votingTargets.desiredUpdate.rulesToDisable
    lines += s"ergo.voting.rulesToDisable = [${rulesToDisable.map(_.toInt).mkString(", ")}]"

    lines += BlockEnd
    lines.mkString("\n")
  }

  private def verifyOverrideBlock(blockText: String, expected: ErgoSettings): Unit = {
    val cfg = ConfigFactory.parseString(blockText)
    require(
      cfg.getInt("ergo.node.mempoolCapacity") == expected.nodeSettings.mempoolCapacity,
      "override round-trip mismatch: mempoolCapacity"
    )
    require(
      cfg.getLong("ergo.node.minimalFeeAmount") == expected.nodeSettings.minimalFeeAmount,
      "override round-trip mismatch: minimalFeeAmount"
    )
    require(
      cfg.getDuration("ergo.node.mempoolCleanupDuration").toMillis ==
        expected.nodeSettings.mempoolCleanupDuration.toMillis,
      "override round-trip mismatch: mempoolCleanupDuration"
    )
  }

  private def readFile(path: Path): String =
    new String(Files.readAllBytes(path), StandardCharsets.UTF_8)

  private def ensureTrailingNewline(s: String): String =
    if (s.isEmpty || s.endsWith("\n")) s else s + "\n"

  private def backupOnce(file: Path): Unit = {
    val bak = file.resolveSibling(file.getFileName.toString + ".bak")
    if (!Files.exists(bak)) {
      val _ = Files.copy(file, bak)
    }
  }

  private def catchIo[A](block: => A): Either[PersistError, A] =
    Try(block).toEither.left.map { e =>
      PersistError.IoFailure(Option(e.getMessage).getOrElse(e.toString))
    }
}
