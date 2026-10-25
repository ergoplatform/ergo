package org.ergoplatform.nodeView.wallet

import java.io.{File, IOException}
import java.nio.file.Files
import java.util.concurrent.atomic.AtomicBoolean

import akka.actor.Props
import akka.testkit.TestProbe
import org.ergoplatform.sdk.SecretString
import org.ergoplatform.utils.{ErgoCorePropertyTest, WalletTestOps}
import org.ergoplatform.utils.ErgoCoreTestConstants.parameters
import org.ergoplatform.utils.ErgoNodeTestConstants.settings
import org.ergoplatform.utils.fixtures.WalletFixture
import org.ergoplatform.wallet.boxes.ReplaceCompactCollectBoxSelector
import org.ergoplatform.wallet.utils.FileUtils
import org.scalatest.concurrent.Eventually

import scala.concurrent.duration._
import scala.util.Try

class WalletRequestSecretErasureSpec extends ErgoCorePropertyTest with WalletTestOps with Eventually {

  private def withUninitializedWallet(seedStrengthBits: Int = settings.walletSettings.seedStrengthBits,
                                      blocksToKeep: Int = -1)
                                     (test: WalletFixture => Unit): Unit = {
    val directory = Files.createTempDirectory("wallet-request-erasure-").toFile
    val fileUtils = new FileUtils {}
    val isolated = settings.copy(
      nodeSettings = settings.nodeSettings.copy(blocksToKeep = blocksToKeep),
      walletSettings = settings.walletSettings.copy(
        testMnemonic = None,
        seedStrengthBits = seedStrengthBits,
        secretStorage = settings.walletSettings.secretStorage.copy(
          secretDir = new File(directory, "keystore").getPath)))
    try new WalletFixture(isolated, parameters, getCurrentView(_).vault).apply(test)
    finally fileUtils.deleteRecursive(directory)
  }

  property("rejected restore and init requests erase supplied secrets") {
    withFixture { implicit w =>
      // This fixture has a test mnemonic, so both requests are rejected by the actor.
      val mnemonic = SecretString.create("synthetic restore phrase")
      val mnemonicPass = SecretString.create("synthetic restore passphrase")
      val walletPass = SecretString.create("synthetic wallet password")
      val mnemonicChars = mnemonic.getData()
      val mnemonicPassChars = mnemonicPass.getData()
      val walletPassChars = walletPass.getData()

      await(wallet.restoreWallet(walletPass, mnemonic, Some(mnemonicPass),
        usePre1627KeyDerivation = false)).isFailure shouldBe true
      mnemonicChars.forall(_ == ' ') shouldBe true
      mnemonicPassChars.forall(_ == ' ') shouldBe true
      walletPassChars.forall(_ == ' ') shouldBe true

      val initPass = SecretString.create("synthetic init password")
      val initMnemonicPass = SecretString.create("synthetic init passphrase")
      val initPassChars = initPass.getData()
      val initMnemonicPassChars = initMnemonicPass.getData()

      await(wallet.initWallet(initPass, Some(initMnemonicPass))).isFailure shouldBe true
      initPassChars.forall(_ == ' ') shouldBe true
      initMnemonicPassChars.forall(_ == ' ') shouldBe true
    }
  }

  property("accepted restore erases the mnemonic and still unlocks the wallet") {
    withUninitializedWallet() { implicit w =>
      val mnemonic = SecretString.create("synthetic restore phrase")
      val walletPass = SecretString.create("synthetic wallet password")
      val mnemonicChars = mnemonic.getData()
      val walletPassChars = walletPass.getData()

      await(wallet.getWalletStatus).initialized shouldBe false
      await(wallet.restoreWallet(walletPass, mnemonic, usePre1627KeyDerivation = false)).isSuccess shouldBe true
      mnemonicChars.forall(_ == ' ') shouldBe true

      implicit val patienceConfig: PatienceConfig = PatienceConfig(10.seconds, 100.millis)
      eventually {
        await(wallet.getWalletStatus).unlocked shouldBe true
      }
      walletPassChars.forall(_ == ' ') shouldBe true
    }
  }

  property("restore rejected by pruning erases all supplied secrets") {
    withUninitializedWallet(blocksToKeep = 0) { implicit w =>
      val mnemonic = SecretString.create("synthetic restore phrase")
      val mnemonicPass = SecretString.create("synthetic restore passphrase")
      val walletPass = SecretString.create("synthetic wallet password")
      val mnemonicChars = mnemonic.getData()
      val mnemonicPassChars = mnemonicPass.getData()
      val walletPassChars = walletPass.getData()

      await(wallet.getWalletStatus).initialized shouldBe false
      val result = await(wallet.restoreWallet(walletPass, mnemonic, Some(mnemonicPass),
        usePre1627KeyDerivation = false))
      result.failed.get.getMessage should include("pruning")
      mnemonicChars.forall(_ == ' ') shouldBe true
      mnemonicPassChars.forall(_ == ' ') shouldBe true
      walletPassChars.forall(_ == ' ') shouldBe true
    }
  }

  property("accepted restore preserves an aliased unlock password") {
    withUninitializedWallet() { implicit w =>
      val shared = SecretString.create("synthetic shared secret")
      val sharedChars = shared.getData()

      await(wallet.restoreWallet(shared, shared, usePre1627KeyDerivation = false)).isSuccess shouldBe true
      implicit val patienceConfig: PatienceConfig = PatienceConfig(10.seconds, 100.millis)
      eventually {
        await(wallet.getWalletStatus).unlocked shouldBe true
      }
      sharedChars.forall(_ == ' ') shouldBe true
    }
  }

  property("unlock erases its password when the service throws") {
    withFixture { implicit w =>
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "throwing-unlock").getAbsolutePath)
      val called = new AtomicBoolean(false)
      val service = new ErgoWalletServiceImpl(actorSettings) {
        override def unlockWallet(state: ErgoWalletState,
                                  walletPass: SecretString,
                                  usePreEip3Derivation: Boolean): Try[ErgoWalletState] = {
          called.set(true)
          throw new IOException("synthetic unlock failure")
        }
      }
      val walletSettings = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(
        walletSettings.maxInputs, walletSettings.optimalInputs, None)
      val actor = w.actorSystem.actorOf(
        Props(classOf[ErgoWalletActor], actorSettings, parameters, service, selector, getHistory))
      val probe = TestProbe()(w.actorSystem)
      probe.watch(actor)

      val walletPass = SecretString.create("synthetic unlock password")
      val walletPassChars = walletPass.getData()
      walletPassChars.exists(_ != ' ') shouldBe true
      probe.send(actor, ErgoWalletActorMessages.UnlockWallet(walletPass))
      eventually(timeout(10.seconds), interval(100.millis)) {
        called.get shouldBe true
        walletPassChars.forall(_ == ' ') shouldBe true
      }

      probe.send(actor, ErgoWalletActorMessages.CloseWallet)
      probe.expectTerminated(actor, 5.seconds)
    }
  }

  property("accepted init erases the mnemonic password and still unlocks the wallet") {
    withUninitializedWallet() { implicit w =>
      val walletPass = SecretString.create("synthetic wallet password")
      val mnemonicPass = SecretString.create("synthetic mnemonic password")
      val walletPassChars = walletPass.getData()
      val mnemonicPassChars = mnemonicPass.getData()

      await(wallet.getWalletStatus).initialized shouldBe false
      val generatedMnemonic = await(wallet.initWallet(walletPass, Some(mnemonicPass))).get
      try {
        generatedMnemonic.getData().exists(_ != ' ') shouldBe true
        mnemonicPassChars.forall(_ == ' ') shouldBe true
        implicit val patienceConfig: PatienceConfig = PatienceConfig(10.seconds, 100.millis)
        eventually {
          await(wallet.getWalletStatus).unlocked shouldBe true
        }
        walletPassChars.forall(_ == ' ') shouldBe true
      } finally generatedMnemonic.erase()
    }
  }

  property("an exceptional init erases both supplied passwords") {
    withUninitializedWallet(seedStrengthBits = -8) { implicit w =>
      val walletPass = SecretString.create("synthetic wallet password")
      val mnemonicPass = SecretString.create("synthetic mnemonic password")
      val walletPassChars = walletPass.getData()
      val mnemonicPassChars = mnemonicPass.getData()

      await(wallet.getWalletStatus).initialized shouldBe false
      wallet.walletActor ! ErgoWalletActorMessages.InitWallet(walletPass, Some(mnemonicPass))
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.seconds, 100.millis)
      eventually {
        walletPassChars.forall(_ == ' ') shouldBe true
        mnemonicPassChars.forall(_ == ' ') shouldBe true
      }
    }
  }
}
