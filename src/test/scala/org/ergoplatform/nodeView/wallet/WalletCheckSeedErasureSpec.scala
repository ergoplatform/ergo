package org.ergoplatform.nodeView.wallet

import java.io.File
import java.nio.file.Files

import org.ergoplatform.sdk.SecretString
import org.ergoplatform.utils.{ErgoCorePropertyTest, WalletTestOps}
import org.ergoplatform.utils.ErgoCoreTestConstants.parameters
import org.ergoplatform.utils.ErgoNodeTestConstants.settings
import org.ergoplatform.utils.fixtures.WalletFixture
import org.ergoplatform.wallet.utils.FileUtils
import org.scalatest.concurrent.Eventually

import scala.concurrent.duration._

class WalletCheckSeedErasureSpec extends ErgoCorePropertyTest with WalletTestOps with Eventually {

  property("CheckSeed erases caller-owned inputs before replying with secret storage") {
    val directory = Files.createTempDirectory("wallet-check-seed-").toFile
    val fileUtils = new FileUtils {}
    val isolated = settings.copy(
      nodeSettings = settings.nodeSettings.copy(blocksToKeep = -1),
      walletSettings = settings.walletSettings.copy(
        testMnemonic = None,
        secretStorage = settings.walletSettings.secretStorage.copy(
          secretDir = new File(directory, "keystore").getPath)))

    try new WalletFixture(isolated, parameters, getCurrentView(_).vault).apply { implicit w =>
      val walletPass = SecretString.create("synthetic wallet password")
      val generatedMnemonic = await(wallet.initWallet(walletPass, None)).get
      try {
        implicit val patienceConfig: PatienceConfig = PatienceConfig(10.seconds, 100.millis)
        eventually {
          await(wallet.getWalletStatus).unlocked shouldBe true
        }

        val candidate = SecretString.create("synthetic different phrase")
        val candidatePass = SecretString.create("synthetic passphrase")
        val candidateChars = candidate.getData()
        val passChars = candidatePass.getData()
        candidateChars.exists(_ != ' ') shouldBe true
        passChars.exists(_ != ' ') shouldBe true

        await(wallet.checkSeed(candidate, Some(candidatePass))) shouldBe false
        candidateChars.toSeq shouldBe Seq.fill(candidateChars.length)(' ')
        passChars.toSeq shouldBe Seq.fill(passChars.length)(' ')
      } finally generatedMnemonic.erase()
    } finally fileUtils.deleteRecursive(directory)
  }

  property("CheckSeed erases the optional password when storage throws") {
    val directory = Files.createTempDirectory("wallet-check-seed-throw-").toFile
    val fileUtils = new FileUtils {}
    val isolated = settings.copy(
      nodeSettings = settings.nodeSettings.copy(blocksToKeep = -1),
      walletSettings = settings.walletSettings.copy(
        testMnemonic = None,
        secretStorage = settings.walletSettings.secretStorage.copy(
          secretDir = new File(directory, "keystore").getPath)))

    try new WalletFixture(isolated, parameters, getCurrentView(_).vault).apply { implicit w =>
      val walletPass = SecretString.create("synthetic wallet password")
      val generatedMnemonic = await(wallet.initWallet(walletPass, None)).get
      try {
        implicit val patienceConfig: PatienceConfig = PatienceConfig(10.seconds, 100.millis)
        eventually {
          await(wallet.getWalletStatus).unlocked shouldBe true
        }

        val erasedMnemonic = SecretString.create("synthetic invalid phrase")
        erasedMnemonic.erase()
        val candidatePass = SecretString.create("synthetic passphrase")
        val passChars = candidatePass.getData()
        passChars.exists(_ != ' ') shouldBe true

        wallet.walletActor ! ErgoWalletActorMessages.CheckSeed(erasedMnemonic, Some(candidatePass))
        eventually(timeout(10.seconds), interval(100.millis)) {
          passChars.toSeq shouldBe Seq.fill(passChars.length)(' ')
        }
      } finally generatedMnemonic.erase()
    } finally fileUtils.deleteRecursive(directory)
  }
}
