package org.ergoplatform.it.container

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import org.scalatest.flatspec.AnyFlatSpec

class DockerMountedCommandSpec extends AnyFlatSpec with IntegrationSuite {
  "A mounted command" should "complete before its helper returns" in {
    val volume = Files.createDirectories(new File(localDataDir, "mounted-command").toPath)
    val marker = volume.resolve("completed")

    docker.runMountedCommand(volume.toString, "/app",
      Seq("sh", "-c", "sleep 2; printf complete > /app/completed"),
      "Delayed mounted command failed")

    new String(Files.readAllBytes(marker), StandardCharsets.UTF_8) shouldBe "complete"
  }

  it should "remove a peer directory before returning" in {
    val volume = Files.createDirectories(new File(localDataDir, "peer-cleanup").toPath)
    val peers = Files.createDirectories(volume.resolve("peers"))
    Files.write(peers.resolve("sentinel"), Array[Byte](1))

    docker.removeFromMountedVolume(volume.toString, "/app", "peers")

    Files.exists(peers) shouldBe false
  }
}
