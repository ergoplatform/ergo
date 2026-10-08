package org.ergoplatform.nodeView.wallet

import java.io.File
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicReference

import org.ergoplatform.sdk.SecretString
import org.ergoplatform.utils.ErgoCoreTestConstants.parameters
import org.ergoplatform.utils.ErgoNodeTestConstants
import org.ergoplatform.wallet.secrets.JsonSecretStorage
import org.ergoplatform.wallet.settings.SecretStorageSettings
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

import scala.util.{Failure, Success}

/** Test-only cross-JVM probe. Run two worker JVMs with a common WALLET_RACE_ROOT. */
class WalletCrossProcessInitializationSpec extends AnyPropSpec with Matchers {
  private lazy val root = new File(sys.env("WALLET_RACE_ROOT"))
  private lazy val role = sys.env("WALLET_RACE_ROLE")
  private lazy val original = ErgoNodeTestConstants.settings
  private lazy val settings = original.copy(directory = new File(root, "shared-node").getPath,
    walletSettings = original.walletSettings.copy(testMnemonic = None,
      secretStorage = original.walletSettings.secretStorage.copy(
        secretDir = new File(root, "shared-keystore").getPath)))

  private def close(state: ErgoWalletState): Unit = {
    state.registry.close()
    state.storage.close()
  }

  private def password: SecretString = SecretString.create("synthetic-race-password")

  private def createSecret(secretSettings: SecretStorageSettings): JsonSecretStorage =
    JsonSecretStorage.init(Array.fill[Byte](32)(1), password,
      usePre1627KeyDerivation = false)(secretSettings)

  private def awaitFile(name: String): Unit = {
    val path = new File(root, name).toPath
    val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(60)
    while (!Files.exists(path) && System.nanoTime() < deadline) Thread.sleep(20)
    require(Files.exists(path), s"Timed out waiting for $name")
  }

  property("two-process wallet initialization and cold reopen") {
    if (!sys.env.contains("WALLET_RACE_ROOT") || !sys.env.contains("WALLET_RACE_ROLE"))
      cancel("Cross-process fixture requires WALLET_RACE_ROOT and WALLET_RACE_ROLE")
    if (role == "holder") {
      val entered = new CountDownLatch(1)
      val release = new CountDownLatch(1)
      val resultA = new AtomicReference[String]("missing")
      val priorA = ErgoWalletState.initial(settings.copy(directory = new File(root, "prior-holder").getPath),
        parameters).get
      val thread = new Thread(new Runnable {
        override def run(): Unit = {
          try {
            val outcome = new WalletInitialization().initialize(priorA, settings, secretSettings => {
              entered.countDown()
              require(release.await(60, TimeUnit.SECONDS), "Holder was not released")
              createSecret(secretSettings)
            }) match {
              case Success(current) =>
                val id = current.generation.get.id
                close(current)
                s"success:$id"
              case Failure(error) => s"failure:${error.getClass.getSimpleName}:${error.getMessage}"
            }
            resultA.set(outcome)
          } catch {
            case error: Throwable => resultA.set(s"failure:${error.getClass.getSimpleName}:${error.getMessage}")
          } finally close(priorA)
        }
      })
      thread.start()
      try {
        require(entered.await(30, TimeUnit.SECONDS), "Holder did not acquire the selection lock")
        val priorB = ErgoWalletState.initial(settings.copy(directory = new File(root, "prior-contender").getPath),
          parameters).get
        val resultB = try new WalletInitialization().initialize(priorB, settings, createSecret) match {
          case Success(current) =>
            val id = current.generation.get.id
            close(current)
            s"success:$id"
          case Failure(error) => s"failure:${error.getClass.getSimpleName}:${error.getMessage}"
        } finally close(priorB)
        Files.write(new File(root, "result-same-jvm").toPath, resultB.getBytes(UTF_8))
        Files.write(new File(root, "ready-probe").toPath, Array[Byte](1))
        awaitFile("result-probe-before")
        val resultC = new String(Files.readAllBytes(new File(root, "result-probe-before").toPath), UTF_8)
        withClue(s"same-JVM=$resultB, separate-JVM=$resultC") {
          resultB should startWith("failure:IOException:")
          resultC should startWith("failure:IOException:")
        }
      } finally {
        release.countDown()
        thread.join(30000)
        Files.write(new File(root, "result-holder").toPath, resultA.get().getBytes(UTF_8))
        Files.write(new File(root, "released-holder").toPath, Array[Byte](1))
      }
      resultA.get() should startWith("success:")
      awaitFile("result-probe-after")
      new String(Files.readAllBytes(new File(root, "result-probe-after").toPath), UTF_8) should startWith("success:")
    } else if (role == "probe") {
      awaitFile("ready-probe")
      val prior = ErgoWalletState.initial(settings.copy(directory = new File(root, "prior-probe").getPath),
        parameters).get
      val before = try new WalletInitialization().initialize(prior, settings, createSecret) match {
        case Success(current) =>
          val id = current.generation.get.id
          close(current)
          s"success:$id"
        case Failure(error) => s"failure:${error.getClass.getSimpleName}:${error.getMessage}"
      } finally close(prior)
      Files.write(new File(root, "result-probe-before").toPath, before.getBytes(UTF_8))
      awaitFile("released-holder")
      val active = ErgoWalletState.initial(settings, parameters).get
      val initialization = new WalletInitialization
      val candidate = initialization.prepareRescan(active, settings).get
      val selected = initialization.publishRescan(active, candidate, settings).get
      try {
        WalletInitialization.selected(settings) shouldBe selected.generation
        Files.write(new File(root, "result-probe-after").toPath, "success:rescan".getBytes(UTF_8))
      } finally close(selected)
    } else if (role == "verify") {
      val results = Seq("a", "b").map { worker =>
        new String(Files.readAllBytes(new File(root, s"result-$worker").toPath), UTF_8).trim
      }
      val winners = results.filter(_.startsWith("success:"))
      val selected = WalletInitialization.selected(settings).get
      val reopened = ErgoWalletState.initial(settings, parameters).get
      try {
        reopened.generation shouldBe Some(selected)
        val loaded = new ErgoWalletServiceImpl(settings).readWallet(reopened, None, None,
          settings.walletSettings.secretStorage)
        loaded.secretStorageOpt.isDefined shouldBe true
        loaded.secretStorageOpt.get.unlock(password).get
        loaded.secretStorageOpt.get.lock()
      } finally close(reopened)
      withClue(s"worker outcomes: ${results.mkString(", ")}; selected: ${selected.id}") {
        winners.size shouldBe 1
        winners.head shouldBe s"success:${selected.id}"
      }
    } else {
      require(Set("a", "b").contains(role), "Worker role must be a or b")
      val other = if (role == "a") "b" else "a"
      val workerSettings = settings.copy(directory = new File(root, s"prior-$role").getPath)
      val prior = ErgoWalletState.initial(workerSettings, parameters).get
      try {
        Files.write(new File(root, s"ready-$role").toPath, Array[Byte](1))
        val deadline = System.nanoTime() + 60000000000L
        while (!Files.exists(new File(root, s"ready-$other").toPath) &&
          System.nanoTime() < deadline) Thread.sleep(20)
        require(Files.exists(new File(root, s"ready-$other").toPath), "Peer did not reach initialization")
        val outcome = new WalletInitialization().initialize(prior, settings, createSecret) match {
          case Success(current) =>
            val id = current.generation.get.id
            close(current)
            s"success:$id"
          case Failure(error) => s"failure:${error.getClass.getSimpleName}:${error.getMessage}"
        }
        Files.write(new File(root, s"result-$role").toPath, outcome.getBytes(UTF_8))
        info(outcome)
      } finally close(prior)
    }
  }
}
