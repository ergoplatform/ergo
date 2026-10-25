package org.ergoplatform.nodeView.wallet

import java.io.{File, IOException}
import java.nio.file.Files
import java.util.concurrent.atomic.AtomicBoolean

import akka.actor.{ActorRef, Props}
import akka.testkit.TestProbe
import org.ergoplatform.sdk.SecretString
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.{ErgoCorePropertyTest, WalletTestOps}
import org.ergoplatform.utils.ErgoCoreTestConstants.parameters
import org.ergoplatform.utils.ErgoNodeTestConstants.settings
import org.ergoplatform.utils.fixtures.WalletFixture
import org.ergoplatform.wallet.boxes.ReplaceCompactCollectBoxSelector
import org.ergoplatform.wallet.utils.FileUtils
import org.scalatest.concurrent.Eventually

import scala.concurrent.duration._
import scala.util.{Failure, Try}

class WalletRequestSecretErasureSpec extends ErgoCorePropertyTest with WalletTestOps with Eventually {

  private def assertErased(chars: Array[Char]): Unit = {
    chars.nonEmpty shouldBe true
    chars.zipWithIndex.foreach { case (char, index) =>
      withClue(s"secret character at index $index: ") {
        char shouldBe ' '
      }
    }
  }

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

  private def throwingSetupActor(implicit w: WalletFixture): (ActorRef, TestProbe) = {
    val actorSettings = w.settings.copy(
      directory = new File(w.nodeViewDir, "throwing-setup").getAbsolutePath)
    val service = new ErgoWalletServiceImpl(actorSettings) {
      override def initWallet(state: ErgoWalletState,
                              settings: ErgoSettings,
                              walletPass: SecretString,
                              mnemonicPassOpt: Option[SecretString]): Try[(SecretString, ErgoWalletState)] =
        throw new IOException("synthetic init failure")

      override def restoreWallet(state: ErgoWalletState,
                                 settings: ErgoSettings,
                                 mnemonic: SecretString,
                                 mnemonicPassOpt: Option[SecretString],
                                 walletPass: SecretString,
                                 usePre1627KeyDerivation: Boolean): Try[ErgoWalletState] =
        throw new IOException("synthetic restore failure")
    }
    val walletSettings = actorSettings.walletSettings
    val selector = new ReplaceCompactCollectBoxSelector(
      walletSettings.maxInputs, walletSettings.optimalInputs, None)
    val actor = w.actorSystem.actorOf(
      Props(classOf[ErgoWalletActor], actorSettings, parameters, service, selector, getHistory))
    actor -> TestProbe()(w.actorSystem)
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
      assertErased(mnemonicChars)
      assertErased(mnemonicPassChars)
      assertErased(walletPassChars)

      val initPass = SecretString.create("synthetic init password")
      val initMnemonicPass = SecretString.create("synthetic init passphrase")
      val initPassChars = initPass.getData()
      val initMnemonicPassChars = initMnemonicPass.getData()

      await(wallet.initWallet(initPass, Some(initMnemonicPass))).isFailure shouldBe true
      assertErased(initPassChars)
      assertErased(initMnemonicPassChars)
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
      assertErased(mnemonicChars)

      implicit val patienceConfig: PatienceConfig = PatienceConfig(10.seconds, 100.millis)
      eventually {
        await(wallet.getWalletStatus).unlocked shouldBe true
      }
      assertErased(walletPassChars)
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
      assertErased(mnemonicChars)
      assertErased(mnemonicPassChars)
      assertErased(walletPassChars)
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
        assertErased(walletPassChars)
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
        assertErased(mnemonicPassChars)
        implicit val patienceConfig: PatienceConfig = PatienceConfig(10.seconds, 100.millis)
        eventually {
          await(wallet.getWalletStatus).unlocked shouldBe true
        }
        assertErased(walletPassChars)
      } finally generatedMnemonic.erase()
    }
  }

  property("a synchronous init throw replies after erasing request passwords") {
    withUninitializedWallet() { implicit w =>
      val (actor, probe) = throwingSetupActor
      val initPass = SecretString.create("synthetic init password")
      val initMnemonicPass = SecretString.create("synthetic init passphrase")
      val initPassChars = initPass.getData()
      val initMnemonicPassChars = initMnemonicPass.getData()
      probe.send(actor, ErgoWalletActorMessages.InitWallet(initPass, Some(initMnemonicPass)))
      probe.expectMsgPF(5.seconds) {
        case Failure(ex: IOException) if ex.getMessage == "synthetic init failure" =>
          assertErased(initPassChars)
          assertErased(initMnemonicPassChars)
      }

      probe.watch(actor)
      probe.send(actor, ErgoWalletActorMessages.CloseWallet)
      probe.expectTerminated(actor, 5.seconds)
    }
  }

  property("a synchronous restore throw replies after erasing request secrets") {
    withUninitializedWallet() { implicit w =>
      val (actor, probe) = throwingSetupActor
      val mnemonic = SecretString.create("synthetic restore phrase")
      val restorePass = SecretString.create("synthetic restore password")
      val restoreMnemonicPass = SecretString.create("synthetic restore passphrase")
      val mnemonicChars = mnemonic.getData()
      val restorePassChars = restorePass.getData()
      val restoreMnemonicPassChars = restoreMnemonicPass.getData()
      probe.send(actor, ErgoWalletActorMessages.RestoreWallet(
        mnemonic, Some(restoreMnemonicPass), restorePass, usePre1627KeyDerivation = false))
      probe.expectMsgPF(5.seconds) {
        case Failure(ex: IOException) if ex.getMessage == "synthetic restore failure" =>
          assertErased(mnemonicChars)
          assertErased(restorePassChars)
          assertErased(restoreMnemonicPassChars)
      }

      probe.watch(actor)
      probe.send(actor, ErgoWalletActorMessages.CloseWallet)
      probe.expectTerminated(actor, 5.seconds)
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
        assertErased(walletPassChars)
        assertErased(mnemonicPassChars)
      }
    }
  }
}
