package org.ergoplatform.nodeView.wallet

import java.io.{File, IOException}
import java.nio.file.{FileAlreadyExistsException, Files}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

import akka.actor.Props
import akka.testkit.TestProbe
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.ChangedState
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages._
import org.ergoplatform.sdk.SecretString
import org.ergoplatform.settings.{ErgoSettings, WalletSettings}
import org.ergoplatform.utils.{ErgoCorePropertyTest, WalletTestOps}
import org.ergoplatform.utils.ErgoCoreTestConstants.parameters
import org.ergoplatform.utils.ErgoNodeTestConstants.settings
import org.ergoplatform.utils.fixtures.WalletFixture
import org.ergoplatform.wallet.boxes.ReplaceCompactCollectBoxSelector
import org.ergoplatform.wallet.secrets.JsonSecretStorage
import org.ergoplatform.wallet.utils.FileUtils
import org.scalatest.concurrent.Eventually

import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

class WalletInitFailureSpec extends ErgoCorePropertyTest with WalletTestOps with Eventually {

  private def assertErased(chars: Array[Char]): Unit = {
    chars.nonEmpty shouldBe true
    chars.zipWithIndex.foreach { case (char, index) =>
      withClue(s"secret character at index $index: ") {
        char shouldBe ' '
      }
    }
  }

  private def withIsolatedFixture(test: WalletFixture => Unit): Unit = {
    val directory = Files.createTempDirectory("wallet-init-failure-").toFile
    val fileUtils = new FileUtils {}
    val isolated = settings.copy(
      directory = new File(directory, "node").getPath,
      nodeSettings = settings.nodeSettings.copy(blocksToKeep = -1),
      walletSettings = settings.walletSettings.copy(
        testMnemonic = None,
        secretStorage = settings.walletSettings.secretStorage.copy(
          secretDir = new File(directory, "keystore").getPath
        )
      )
    )
    try new WalletFixture(isolated, parameters, getCurrentView(_).vault).apply(test)
    finally fileUtils.deleteRecursive(directory)
  }

  property("failed recreateRegistry during /wallet/init leaves no secret file, erases generated mnemonic, keeps actor usable, and allows retry") {
    withIsolatedFixture { implicit w =>
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "registry-fail-case").getAbsolutePath,
        walletSettings = w.settings.walletSettings.copy(
          secretStorage = w.settings.walletSettings.secretStorage.copy(
            secretDir = new File(w.nodeViewDir, "registry-fail-keystore").getAbsolutePath
          )
        )
      )
      val failOnce = new AtomicBoolean(true)
      val capturedMnemonicChars = new AtomicReference[Array[Char]]()

      val service = new ErgoWalletServiceImpl(actorSettings) {
        override protected def generateMnemonic(walletSettings: WalletSettings, entropy: Array[Byte]): Try[SecretString] =
          super.generateMnemonic(walletSettings, entropy).map { m =>
            capturedMnemonicChars.set(m.getData())
            m
          }

        override def recreateRegistry(state: ErgoWalletState, s: ErgoSettings): Try[ErgoWalletState] = {
          if (failOnce.compareAndSet(true, false)) {
            state.registry.close()
            Failure(new IOException("synthetic registry recreate failure"))
          } else {
            super.recreateRegistry(state, s)
          }
        }
      }

      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val actor = w.actorSystem.actorOf(
        Props(classOf[ErgoWalletActor], actorSettings, parameters, service, selector, getHistory)
      )
      val probe = TestProbe()(w.actorSystem)

      val pass1 = SecretString.create("synthetic wallet password 1")
      val mnemonicPass1 = SecretString.create("synthetic mnemonic passphrase 1")
      val pass1Chars = pass1.getData()
      val mnemonicPass1Chars = mnemonicPass1.getData()

      probe.send(actor, InitWallet(pass1, Some(mnemonicPass1)))
      probe.expectMsgPF(10.seconds) {
        case Failure(ex: IOException) =>
          ex.getMessage should include("synthetic registry recreate failure")
      }

      // 1 & 2. No secret file was left behind on disk
      val readAttempt = JsonSecretStorage.readFile(actorSettings.walletSettings.secretStorage)
      readAttempt.isFailure shouldBe true
      readAttempt.failed.get shouldBe a[JsonSecretStorage.SecretFileNotFoundException]

      // 3. Generated mnemonic and supplied passwords were all erased
      val failedMnemonicChars = Option(capturedMnemonicChars.get())
      failedMnemonicChars shouldBe defined
      assertErased(failedMnemonicChars.get)
      assertErased(pass1Chars)
      assertErased(mnemonicPass1Chars)

      // 4. Actor reopened its closed registry handle and remains responsive
      probe.send(actor, GetWalletStatus)
      val statusAfterFailure = probe.expectMsgType[WalletStatus](5.seconds)
      statusAfterFailure.initialized shouldBe false
      statusAfterFailure.unlocked shouldBe false

      // Retry /wallet/init succeeds and unlocks the wallet
      val pass2 = SecretString.create("synthetic wallet password 2")
      probe.send(actor, InitWallet(pass2, None))
      val retryMnemonic = probe.expectMsgPF(10.seconds) {
        case Success(m: SecretString) => m
      }
      try {
        retryMnemonic.getData().exists(_ != ' ') shouldBe true
        JsonSecretStorage.readFile(actorSettings.walletSettings.secretStorage).isSuccess shouldBe true
        eventually(timeout(10.seconds), interval(100.millis)) {
          probe.send(actor, GetWalletStatus)
          val s = probe.expectMsgType[WalletStatus](5.seconds)
          s.initialized shouldBe true
          s.unlocked shouldBe true
        }
      } finally {
        retryMnemonic.erase()
      }

      probe.watch(actor)
      probe.send(actor, CloseWallet)
      probe.expectTerminated(actor, 5.seconds)
    }
  }

  property("failed recreateStorage during /wallet/init leaves no secret file, erases generated mnemonic, recovers handles, and allows retry") {
    withIsolatedFixture { implicit w =>
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "storage-fail-case").getAbsolutePath,
        walletSettings = w.settings.walletSettings.copy(
          secretStorage = w.settings.walletSettings.secretStorage.copy(
            secretDir = new File(w.nodeViewDir, "storage-fail-keystore").getAbsolutePath
          )
        )
      )
      val failOnce = new AtomicBoolean(true)
      val capturedMnemonicChars = new AtomicReference[Array[Char]]()

      val service = new ErgoWalletServiceImpl(actorSettings) {
        override protected def generateMnemonic(walletSettings: WalletSettings, entropy: Array[Byte]): Try[SecretString] =
          super.generateMnemonic(walletSettings, entropy).map { m =>
            capturedMnemonicChars.set(m.getData())
            m
          }

        override def recreateStorage(state: ErgoWalletState, s: ErgoSettings): Try[ErgoWalletState] = {
          if (failOnce.compareAndSet(true, false)) {
            state.storage.close()
            Failure(new IOException("synthetic storage recreate failure"))
          } else {
            super.recreateStorage(state, s)
          }
        }
      }

      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val actor = w.actorSystem.actorOf(
        Props(classOf[ErgoWalletActor], actorSettings, parameters, service, selector, getHistory)
      )
      val probe = TestProbe()(w.actorSystem)

      val pass1 = SecretString.create("synthetic wallet password 1")
      val pass1Chars = pass1.getData()

      probe.send(actor, InitWallet(pass1, None))
      probe.expectMsgPF(10.seconds) {
        case Failure(ex: IOException) =>
          ex.getMessage should include("synthetic storage recreate failure")
      }

      JsonSecretStorage.readFile(actorSettings.walletSettings.secretStorage).failed.get shouldBe
        a[JsonSecretStorage.SecretFileNotFoundException]

      val failedMnemonicChars = Option(capturedMnemonicChars.get())
      failedMnemonicChars shouldBe defined
      assertErased(failedMnemonicChars.get)
      assertErased(pass1Chars)

      // Both registry (GetWalletStatus) and storage (ChangedState) remain usable after recovery
      probe.send(actor, ChangedState(getCurrentState))
      probe.send(actor, GetWalletStatus)
      val statusAfterFailure = probe.expectMsgType[WalletStatus](5.seconds)
      statusAfterFailure.initialized shouldBe false
      statusAfterFailure.error shouldBe None

      val pass2 = SecretString.create("synthetic wallet password 2")
      probe.send(actor, InitWallet(pass2, None))
      val retryMnemonic = probe.expectMsgPF(10.seconds) {
        case Success(m: SecretString) => m
      }
      try {
        retryMnemonic.getData().exists(_ != ' ') shouldBe true
        eventually(timeout(10.seconds), interval(100.millis)) {
          probe.send(actor, GetWalletStatus)
          val s = probe.expectMsgType[WalletStatus](5.seconds)
          s.initialized shouldBe true
          s.unlocked shouldBe true
        }
      } finally {
        retryMnemonic.erase()
      }

      probe.watch(actor)
      probe.send(actor, CloseWallet)
      probe.expectTerminated(actor, 5.seconds)
    }
  }

  property("initWallet fails before recreating registry or storage when a secret file already exists") {
    withIsolatedFixture { implicit w =>
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "existing-secret-case").getAbsolutePath,
        walletSettings = w.settings.walletSettings.copy(
          secretStorage = w.settings.walletSettings.secretStorage.copy(
            secretDir = new File(w.nodeViewDir, "existing-secret-keystore").getAbsolutePath
          )
        )
      )
      JsonSecretStorage.init(
        Array.fill[Byte](32)(7: Byte),
        SecretString.create("existing password"),
        usePre1627KeyDerivation = false
      )(actorSettings.walletSettings.secretStorage)

      val registryTouched = new AtomicBoolean(false)
      val storageTouched = new AtomicBoolean(false)
      val capturedMnemonicChars = new AtomicReference[Array[Char]]()

      val service = new ErgoWalletServiceImpl(actorSettings) {
        override protected def generateMnemonic(walletSettings: WalletSettings, entropy: Array[Byte]): Try[SecretString] =
          super.generateMnemonic(walletSettings, entropy).map { m =>
            capturedMnemonicChars.set(m.getData())
            m
          }

        override def recreateRegistry(state: ErgoWalletState, s: ErgoSettings): Try[ErgoWalletState] = {
          registryTouched.set(true)
          super.recreateRegistry(state, s)
        }

        override def recreateStorage(state: ErgoWalletState, s: ErgoSettings): Try[ErgoWalletState] = {
          storageTouched.set(true)
          super.recreateStorage(state, s)
        }
      }

      val state = ErgoWalletState.initial(actorSettings, parameters).get
      try {
        val mnemonicPass = SecretString.create("unused mnemonic pass")
        val mnemonicPassChars = mnemonicPass.getData()
        val result = service.initWallet(
          state,
          actorSettings,
          SecretString.create("new password"),
          Some(mnemonicPass)
        )
        result.isFailure shouldBe true
        result.failed.get shouldBe a[FileAlreadyExistsException]
        registryTouched.get() shouldBe false
        storageTouched.get() shouldBe false
        assertErased(capturedMnemonicChars.get())
        assertErased(mnemonicPassChars)
      } finally {
        state.storage.close()
        state.registry.close()
      }
    }
  }
}
