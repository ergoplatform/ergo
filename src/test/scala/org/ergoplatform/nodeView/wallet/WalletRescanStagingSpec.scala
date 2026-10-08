package org.ergoplatform.nodeView.wallet

import akka.actor.{ActorSystem, Props}
import akka.testkit.TestProbe
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.core.idToVersion
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.ChangedState
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages._
import org.ergoplatform.nodeView.wallet.requests.PaymentRequest
import org.ergoplatform.nodeView.wallet.persistence.WalletDigest
import org.ergoplatform.sdk.SecretString
import org.ergoplatform.sdk.wallet.secrets.DerivationPath
import org.ergoplatform.utils.{ErgoCorePropertyTest, WalletTestOps}
import org.ergoplatform.wallet.Constants.PaymentsScanId
import org.ergoplatform.wallet.boxes.ReplaceCompactCollectBoxSelector
import org.ergoplatform.wallet.boxes.ChainStatus
import org.scalatest.concurrent.Eventually

import java.io.{File, IOException}
import java.nio.file.Files
import java.nio.file.Path
import java.util.concurrent.{CountDownLatch, TimeUnit}
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Try}

class WalletRescanStagingSpec extends ErgoCorePropertyTest with WalletTestOps with Eventually {
  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters
  import org.ergoplatform.utils.ErgoNodeTestConstants.genesisBoxes

  property("selected-generation rescan publishes only a complete cold-reopenable replay") {
    withFixture { implicit w =>
      val sourceAddress = getPublicKeys.head
      val first = makeGenesisBlock(sourceAddress.pubkey)
      applyBlock(first) shouldBe 'success
      val walletBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      val addressRoot = new File(w.nodeViewDir, "restored-address")
      val addressSettings = w.settings.copy(directory = addressRoot.getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(blocksToKeep = -1),
        walletSettings = w.settings.walletSettings.copy(testMnemonic = None,
          secretStorage = w.settings.walletSettings.secretStorage.copy(
            secretDir = new File(addressRoot, "keystore").getAbsolutePath)))
      val addressState = ErgoWalletState.initial(addressSettings, parameters).get
      val addressService = new ErgoWalletServiceImpl(addressSettings)
      val addressMnemonic = SecretString.create(w.settings.walletSettings.testMnemonic.get)
      val addressPassword = SecretString.create("staged-rescan-password")
      val restoredAddress = try {
        val restored = addressService.restoreWallet(addressState, addressSettings, addressMnemonic,
          None, addressPassword, usePre1627KeyDerivation = false).get
        val unlocked = addressService.unlockWallet(restored, addressPassword,
          addressSettings.walletSettings.usePreEip3Derivation).get
        try unlocked.walletVars.publicKeyAddresses.head finally {
          unlocked.registry.close()
          unlocked.storage.close()
        }
      } finally {
        addressMnemonic.erase()
        addressPassword.erase()
      }
      val payment = PaymentRequest(restoredAddress, walletBalance / 2, Array.empty, Map.empty)
      val spendingTx = await(wallet.generateTransaction(Seq(payment))).get
      val alternativePayment = PaymentRequest(sourceAddress, walletBalance / 3, Array.empty, Map.empty)
      val alternativeTx = await(wallet.generateTransaction(Seq(alternativePayment))).get
      val alternativeSecond = makeNextBlock(getUtxoState, Seq(alternativeTx))
      val second = makeNextBlock(getUtxoState, Seq(spendingTx))
      applyBlock(second) shouldBe 'success
      val confirmedBox = spendingTx.outputs.find(_.ergoTree == restoredAddress.script).get
      val pendingTx = makeSpendingTx(Seq(confirmedBox), restoredAddress, confirmedBox.value / 2)
      val pendingChange = pendingTx.outputs.find(_.ergoTree == restoredAddress.script).get

      for (cut <- Seq("unapplied-fork", "restart", "stale-restart", "timeout", "success", "mid-replay", "promotion", "lock", "rollback",
        "pending", "pending-during", "duplicate", "missing-prefix", "fork", "advance",
        "timeout-mismatch", "prestart-mismatch", "descriptor-mismatch")) {
        val root = new File(w.nodeViewDir, s"staged-$cut")
        val settings = w.settings.copy(directory = root.getAbsolutePath,
          nodeSettings = w.settings.nodeSettings.copy(blocksToKeep = -1),
          walletSettings = w.settings.walletSettings.copy(testMnemonic = None,
            secretStorage = w.settings.walletSettings.secretStorage.copy(
              secretDir = new File(root, "keystore").getAbsolutePath)))
        val fault = new IOException(s"$cut rescan fault")
        val scanEntered = new CountDownLatch(1)
        val releaseScan = new CountDownLatch(1)
        val publishFinished = new CountDownLatch(1)
        @volatile var stagedReplay = false
        val service = new ErgoWalletServiceImpl(settings) {
          override private[wallet] val walletInitialization: WalletInitialization = new WalletInitialization {
            override protected def replaceDescriptor(staging: Path, target: Path): Unit = {
              if (cut == "promotion") throw fault
              super.replaceDescriptor(staging, target)
            }
          }

          override def scanBlockUpdate(state: ErgoWalletState, block: ErgoFullBlock,
                                       dustLimit: Option[Long]): Try[ErgoWalletState] = {
            if (stagedReplay && (((cut == "lock" || cut == "duplicate" || cut == "pending-during" ||
              cut == "advance") && block.height == first.height) ||
              ((cut == "rollback" || cut == "fork" || cut == "descriptor-mismatch") &&
                block.height == second.height))) {
              scanEntered.countDown()
              if (!releaseScan.await(5, TimeUnit.SECONDS)) throw fault
            }
            if (cut == "mid-replay" && block.height == second.height) Failure(fault)
            else super.scanBlockUpdate(state, block, dustLimit)
          }

          override def publishRegistryRescan(active: ErgoWalletState, candidate: ErgoWalletState,
                                             settings: org.ergoplatform.settings.ErgoSettings): Try[ErgoWalletState] =
            try super.publishRegistryRescan(active, candidate, settings)
            finally publishFinished.countDown()
        }
        val legacy = ErgoWalletState.initial(settings, parameters).get
        val mnemonic = SecretString.create(w.settings.walletSettings.testMnemonic.get)
        val initialPassword = SecretString.create("staged-rescan-password")
        val selected = try {
          val prepared = service.restoreWallet(legacy, settings, mnemonic, None,
            initialPassword, usePre1627KeyDerivation = false).get
          try {
            prepared.registry.updateScans(Set(PaymentsScanId), genesisBoxes.head).get
            if (cut == "restart") {
              prepared.storage.updateStateContext(getCurrentState.stateContext).get
            }
            if (Set("rollback", "pending", "fork", "advance").contains(cut)) {
              val unlocked = service.unlockWallet(prepared, initialPassword,
                settings.walletSettings.usePreEip3Derivation).get
              val afterFirst = service.scanBlockUpdate(unlocked, first, settings.walletSettings.dustLimit).get
              service.scanBlockUpdate(afterFirst, second, settings.walletSettings.dustLimit).get
            }
            prepared.generation.get
          } finally {
            prepared.registry.close()
            prepared.storage.close()
          }
        } finally {
          mnemonic.erase()
          initialPassword.erase()
        }

        val selector = new ReplaceCompactCollectBoxSelector(
          settings.walletSettings.maxInputs, settings.walletSettings.optimalInputs, None)
        val unappliedForkTip = if (cut == "unapplied-fork") {
          val appliedHeight = getCurrentState.stateContext.currentHeight
          val appliedTip = getHistory.bestFullBlockAt(appliedHeight).get
          appliedTip.id shouldBe org.ergoplatform.core.versionToId(getCurrentState.version)
          val fork = appliedTip.copy(header = appliedTip.header.copy(
            timestamp = appliedTip.header.timestamp + 1))
          fork.id should not equal appliedTip.id
          Some(fork)
        } else None
        val holderRef = if (cut == "timeout" || cut == "timeout-mismatch")
          TestProbe()(w.actorSystem).ref else w.nodeViewHolderRef
        val actorProps = if (cut == "missing-prefix" || cut == "unapplied-fork") {
          Props(new ErgoWalletActor(settings, parameters, service, selector, getHistory,
            Some(holderRef)) {
            override protected[wallet] def selectedBlockAt(height: Int): Option[ErgoFullBlock] =
              if (cut == "missing-prefix" && height == first.height) None
              else if (unappliedForkTip.exists(_.height == height)) unappliedForkTip
              else super.selectedBlockAt(height)
          })
        } else Props(classOf[ErgoWalletActor], settings, parameters, service, selector, getHistory,
          Some(holderRef))
        val descriptorMismatch = Set("timeout-mismatch", "prestart-mismatch", "descriptor-mismatch").contains(cut)
        val actorSystem = if (descriptorMismatch) ActorSystem(s"wallet-rescan-$cut")
          else w.actorSystem
        val actor = actorSystem.actorOf(actorProps)
        val probe = TestProbe()(actorSystem)
        probe.watch(actor)
        var originalDescriptor: Option[Array[Byte]] = None
        try {
          probe.send(actor, UnlockWallet(SecretString.create("staged-rescan-password")))
          probe.expectMsg(scala.util.Success(()))
          probe.send(actor, ReadPublicKeys(0, 1))
          probe.expectMsgType[Seq[org.ergoplatform.P2PKAddress]].head shouldBe restoredAddress
          if (cut != "restart" && cut != "stale-restart") probe.send(actor, ChangedState(getCurrentState))
          if (cut == "unapplied-fork") {
            getCurrentState.stateContext.currentHeight shouldBe unappliedForkTip.get.height
          }
          if (cut == "pending") {
            probe.send(actor, ReadBalances(ChainStatus.OffChain))
            val priorBalance = probe.expectMsgType[WalletDigest].walletBalance
            probe.send(actor, ScanOffChain(pendingTx))
            probe.send(actor, ReadBalances(ChainStatus.OffChain))
            probe.expectMsgType[WalletDigest].walletBalance shouldBe priorBalance - confirmedBox.value / 2
          }
          if (cut == "prestart-mismatch") {
            val descriptor = WalletInitialization.descriptor(settings)
            originalDescriptor = Some(Files.readAllBytes(descriptor))
            Files.write(descriptor, Array[Byte](1, 2, 3))
          }
          stagedReplay = true
          probe.send(actor, RescanWallet(if (cut == "rollback" || cut == "missing-prefix" || cut == "fork")
            second.height else first.height))
          if (cut == "prestart-mismatch") {
            probe.expectMsgType[Failure[_]].exception.isInstanceOf[WalletInitialization.OutcomeUnknown] shouldBe true
          } else probe.expectMsg(scala.util.Success(()))

          if (cut == "timeout-mismatch") {
            val descriptor = WalletInitialization.descriptor(settings)
            originalDescriptor = Some(Files.readAllBytes(descriptor))
            Files.write(descriptor, Array[Byte](1, 2, 3))
          }

          if (cut == "lock") {
            scanEntered.await(5, TimeUnit.SECONDS) shouldBe true
            val staleUnlock = SecretString.create("staged-rescan-password")
            probe.send(actor, UnlockWallet(staleUnlock))
            probe.send(actor, LockWallet)
            probe.send(actor, GetPrivateKeyFromPath(DerivationPath.fromEncoded("m/44/1/1/0/0").get))
            probe.send(actor, GetFirstSecret)
            releaseScan.countDown()
            probe.expectMsgType[Failure[_]].exception.getMessage should include("rescan in progress")
            Try(staleUnlock.getData()).isFailure shouldBe true
            probe.expectMsgType[Failure[_]].exception.getMessage should include("locked")
            probe.expectMsgType[FirstSecretResponse].secret.failed.get.getMessage should include("locked")
          }
          if (cut == "rollback" || cut == "fork") {
            scanEntered.await(5, TimeUnit.SECONDS) shouldBe true
            probe.send(actor, Rollback(idToVersion(first.id)))
            if (cut == "fork") probe.send(actor, ScanOnChain(alternativeSecond))
            releaseScan.countDown()
          }
          if (cut == "advance") {
            scanEntered.await(5, TimeUnit.SECONDS) shouldBe true
            val thirdPayment = PaymentRequest(sourceAddress, confirmedBox.value / 3, Array.empty, Map.empty)
            val thirdTx = await(wallet.generateTransaction(Seq(thirdPayment))).get
            val third = makeNextBlock(getUtxoState, Seq(thirdTx))
            applyBlock(third) shouldBe 'success
            probe.send(actor, ScanOnChain(third))
            releaseScan.countDown()
          }
          if (cut == "pending-during") {
            scanEntered.await(5, TimeUnit.SECONDS) shouldBe true
            probe.send(actor, ScanOffChain(pendingTx))
            releaseScan.countDown()
          }
          if (cut == "duplicate") {
            scanEntered.await(5, TimeUnit.SECONDS) shouldBe true
            probe.send(actor, ChangedState(getCurrentState))
            probe.send(actor, ScanOnChain(second))
            releaseScan.countDown()
          }
          if (cut == "descriptor-mismatch") {
            scanEntered.await(5, TimeUnit.SECONDS) shouldBe true
            val descriptor = WalletInitialization.descriptor(settings)
            originalDescriptor = Some(Files.readAllBytes(descriptor))
            Files.write(descriptor, Array[Byte](1, 2, 3))
            releaseScan.countDown()
          }

          if (Set("restart", "stale-restart", "success", "lock", "pending", "pending-during", "duplicate").contains(cut)) {
            publishFinished.await(10, TimeUnit.SECONDS) shouldBe true
            WalletInitialization.selected(settings).get.registryId.isDefined shouldBe true
            if (cut == "lock") {
              probe.send(actor, GetWalletStatus)
              probe.expectMsgType[WalletStatus].unlocked shouldBe false
              probe.send(actor, GetFirstSecret)
              probe.expectMsgType[FirstSecretResponse].secret.isFailure shouldBe true
            }
            if (cut == "pending" || cut == "pending-during") {
              probe.send(actor, ReadBalances(ChainStatus.OnChain))
              val confirmedBalance = probe.expectMsgType[WalletDigest].walletBalance
              probe.send(actor, ReadBalances(ChainStatus.OffChain))
              probe.expectMsgType[WalletDigest].walletBalance shouldBe confirmedBalance - confirmedBox.value / 2
              probe.send(actor, GetWalletBoxes(unspentOnly = true, considerUnconfirmed = true))
              val boxes = probe.expectMsgType[Seq[WalletBox]]
              boxes.exists(_.trackedBox.box.id.sameElements(confirmedBox.id)) shouldBe false
              boxes.exists(_.trackedBox.box.id.sameElements(pendingChange.id)) shouldBe true
            }
          } else if (descriptorMismatch) {
            probe.expectTerminated(actor, 10.seconds)
          } else {
            eventually(timeout(10.seconds), interval(100.millis)) {
              probe.send(actor, GetWalletStatus)
              probe.expectMsgType[WalletStatus].error.value should include(
                if (Set("rollback", "fork", "advance").contains(cut)) "selected chain changed"
                else if (cut == "missing-prefix") "Required rescan block"
                else if (cut == "unapplied-fork") "applied state"
                else if (cut == "timeout") "snapshot timed out"
                else fault.getMessage)
            }
            WalletInitialization.selected(settings) shouldBe Some(selected)
          }
        } finally {
          if (descriptorMismatch) {
            releaseScan.countDown()
            Await.result(actorSystem.terminate(), 10.seconds)
            originalDescriptor.foreach(bytes => Files.write(WalletInitialization.descriptor(settings), bytes))
          } else {
            probe.send(actor, CloseWallet)
            probe.expectTerminated(actor, 5.seconds)
          }
        }

        val reopened = ErgoWalletState.initial(settings, parameters).get
        try {
          if (Set("restart", "stale-restart", "success", "lock", "pending", "pending-during", "duplicate").contains(cut)) {
            reopened.generation.get.registryId.isDefined shouldBe true
            reopened.registry.fetchDigest().height shouldBe second.height
            reopened.registry.allWalletTxs().exists(_.tx.id == spendingTx.id) shouldBe true
            reopened.registry.getBox(genesisBoxes.head.id) shouldBe None
          } else {
            reopened.generation shouldBe Some(selected)
            reopened.registry.fetchDigest().height shouldBe (cut match {
              case "rollback" => first.height
              case "fork" => alternativeSecond.height
              case "advance" => second.height + 1
              case _ => 0
            })
            if (cut == "fork") {
              reopened.registry.allWalletTxs().exists(_.tx.id == spendingTx.id) shouldBe false
            }
            if (!Set("rollback", "fork", "advance").contains(cut)) {
              reopened.registry.getBox(genesisBoxes.head.id).isDefined shouldBe true
            }
          }
        } finally {
          reopened.registry.close()
          reopened.storage.close()
        }
      }
    }
  }
}
