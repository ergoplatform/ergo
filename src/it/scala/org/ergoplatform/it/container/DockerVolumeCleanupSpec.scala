package org.ergoplatform.it.container

import java.io.File
import java.nio.file.Files

import org.scalatest.flatspec.AnyFlatSpec

class DockerVolumeCleanupSpec extends AnyFlatSpec with IntegrationSuite {

  "Volume cleanup" should "remove the path" in {
    val volume = Files.createDirectories(new File(localDataDir, "cleanup").toPath)
    val peers  = Files.createDirectories(volume.resolve("peers"))
    Files.write(peers.resolve("sentinel"), Array[Byte](1))
    docker.removeFromMountedVolume(volume.toString, "/app", "peers")
    Files.exists(peers) shouldBe false
  }

  "A command in a mounted volume" should "finish before the call returns" in {
    // a wait that returns early lets the helper container be removed while `sleep` runs
    val volume = Files.createDirectories(new File(localDataDir, "slow").toPath)
    docker.runInMountedVolume(volume.toString, "/app", "sh", "-c",
      "sleep 1; touch /app/done")
    Files.exists(volume.resolve("done")) shouldBe true
  }
}
