package org.ergoplatform.nodeView.wallet

import akka.actor.{ActorSystem, Props}
import akka.testkit.TestProbe
import com.typesafe.config.ConfigFactory
import org.ergoplatform.Pay2SAddress
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{Rollback => HolderRollback}
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages._
import org.ergoplatform.nodeView.wallet.persistence.WalletStorage
import org.ergoplatform.nodeView.wallet.requests.PaymentRequest
import org.ergoplatform.sdk.SecretString
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.utils.{ErgoCorePropertyTest, WalletTestOps}
import org.ergoplatform.utils.generators.{ErgoNodeTransactionGenerators, ValidBlocksGenerators}
import org.ergoplatform.wallet.Constants.PaymentsScanId
import org.ergoplatform.wallet.boxes.ReplaceCompactCollectBoxSelector
import org.ergoplatform.wallet.interpreter.TransactionHintsBag
import org.ergoplatform.wallet.secrets.JsonSecretStorage
import org.scalatest.concurrent.Eventually
import scorex.db.LDBKVStore

import java.io.{File, IOException}
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

class WalletDeepForkQuarantineSpec extends ErgoCorePropertyTest with WalletTestOps with Eventually {
  import org.ergoplatform.core.idToVersion
  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters

  property("a real holder deep fork quarantines a wallet whose common version was pruned") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val first = makeGenesisBlock(address.pubkey)
      applyBlock(first) shouldBe 'success
      val initialBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      def payment(amount: Long): PaymentRequest = PaymentRequest(
        Pay2SAddress(TrueTree)(w.settings.addressEncoder), amount, Array.empty, Map.empty
      )

      // Build two valid branches which spend the same A1 wallet output.
      val secondATx = await(wallet.generateTransaction(Seq(payment(initialBalance / 2)))).get
      val secondA = makeNextBlock(getUtxoState, Seq(secondATx))
      val secondBTx = await(wallet.generateTransaction(Seq(payment(initialBalance / 3)))).get
      val (forkUtxo, boxHolder) = ValidBlocksGenerators.createUtxoState(w.settings)
      val forkAtFirst = WrappedUtxoState(forkUtxo, boxHolder, w.settings)
        .applyModifier(first)(_ => ()).get
      val secondB = ValidBlocksGenerators.validFullBlock(Some(first), forkAtFirst, Seq(secondBTx))
      val forkAtSecondB = forkAtFirst.applyModifier(secondB)(_ => ()).get
      secondA.id should not equal secondB.id

      applyBlock(secondA) shouldBe 'success
      val externalA = secondA.blockTransactions.txs.flatMap(_.outputs)
        .find(_.ergoTree == TrueTree).get
      val thirdATx = ErgoNodeTransactionGenerators.validTransactionFromBoxes(
        IndexedSeq(externalA), stateCtxOpt = Some(getUtxoState.stateContext)
      )
      val thirdA = makeNextBlock(getUtxoState, Seq(thirdATx))
      applyBlock(thirdA) shouldBe 'success

      def nextForkBlock(parent: ErgoFullBlock, forkState: WrappedUtxoState): ErgoFullBlock = {
        val external = parent.blockTransactions.txs.flatMap(_.outputs)
          .find(_.ergoTree == TrueTree).get
        val tx = ErgoNodeTransactionGenerators.validTransactionFromBoxes(
          IndexedSeq(external), stateCtxOpt = Some(forkState.stateContext)
        )
        ValidBlocksGenerators.validFullBlock(Some(parent), forkState, Seq(tx))
      }
      val thirdB = nextForkBlock(secondB, forkAtSecondB)
      val forkAtThirdB = forkAtSecondB.applyModifier(thirdB)(_ => ()).get
      val fourthB = nextForkBlock(thirdB, forkAtThirdB)
      val expectedB = boxesAvailable(secondB, address.pubkey).map(_.value).sum

      // The holder retains enough state to select B4. Only the observed wallet
      // keeps one version, so its A1 common version is unavailable at rollback.
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "deep-fork-wallet").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 1, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, getHistory
      )))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val actorProbe = TestProbe()(w.actorSystem)
      actorProbe.watch(actor)
      val retainedSettings = actorSettings.copy(
        directory = new File(w.nodeViewDir, "retained-fork-wallet").getAbsolutePath,
        nodeSettings = actorSettings.nodeSettings.copy(keepVersions = 10)
      )
      val retainedActor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        retainedSettings, parameters, new ErgoWalletServiceImpl(retainedSettings), selector, getHistory
      )))
      val retainedReader = new ErgoWalletReader { override val walletActor = retainedActor }
      val retainedProbe = TestProbe()(w.actorSystem)
      retainedProbe.watch(retainedActor)
      val unmarkedSettings = actorSettings.copy(
        directory = new File(w.nodeViewDir, "unmarked-old-branch-wallet").getAbsolutePath
      )
      val unmarkedActor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        unmarkedSettings, parameters, new ErgoWalletServiceImpl(unmarkedSettings), selector, getHistory
      )))
      val unmarkedReader = new ErgoWalletReader { override val walletActor = unmarkedActor }
      val unmarkedProbe = TestProbe()(w.actorSystem)
      unmarkedProbe.watch(unmarkedActor)
      val secretlessSeedSettings = actorSettings.copy(
        directory = new File(w.nodeViewDir, "secretless-rollback-wallet").getAbsolutePath
      )
      val secretlessSeedActor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        secretlessSeedSettings, parameters, new ErgoWalletServiceImpl(secretlessSeedSettings), selector, getHistory
      )))
      val secretlessSeedReader = new ErgoWalletReader { override val walletActor = secretlessSeedActor }
      val secretlessSeedProbe = TestProbe()(w.actorSystem)
      secretlessSeedProbe.watch(secretlessSeedActor)
      val secretlessProbe = TestProbe()(w.actorSystem)
      var secretlessActor: Option[akka.actor.ActorRef] = None
      val rollbackProbe = TestProbe()(w.actorSystem)
      w.actorSystem.eventStream.subscribe(rollbackProbe.ref, classOf[HolderRollback])
      var actorClosed = false
      var retainedClosed = false
      var unmarkedClosed = false
      var secretlessSeedClosed = false
      var secretlessClosed = false

      try {
        Seq(first, secondA, thirdA).foreach(block => actorProbe.send(actor, ScanOnChain(block)))
        Seq(first, secondA, thirdA).foreach(block => retainedProbe.send(retainedActor, ScanOnChain(block)))
        Seq(first, secondA, thirdA).foreach(block => unmarkedProbe.send(unmarkedActor, ScanOnChain(block)))
        Seq(first, secondA, thirdA).foreach(block => secretlessSeedProbe.send(secretlessSeedActor, ScanOnChain(block)))
        val oldBalance = eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe thirdA.height
          status.error shouldBe None
          status.unlocked shouldBe true
          await(reader.confirmedBalances).walletBalance
        }
        oldBalance should not equal expectedB
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(retainedReader.getWalletStatus)
          status.height shouldBe thirdA.height
          status.error shouldBe None
        }
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(unmarkedReader.getWalletStatus)
          status.height shouldBe thirdA.height
          status.error shouldBe None
        }
        unmarkedProbe.send(unmarkedActor, CloseWallet)
        unmarkedProbe.expectTerminated(unmarkedActor, 5.seconds)
        unmarkedClosed = true
        eventually(timeout(10.seconds), interval(100.millis)) {
          await(secretlessSeedReader.getWalletStatus).height shouldBe thirdA.height
        }
        secretlessSeedProbe.send(secretlessSeedActor, CloseWallet)
        secretlessSeedProbe.expectTerminated(secretlessSeedActor, 5.seconds)
        secretlessSeedClosed = true
        getHistory.appliedFullChainProbe(thirdA.id, thirdA.height) shouldBe
          org.ergoplatform.nodeView.history.ErgoHistoryReader.FullChainSelected(thirdA.id)
        val secretlessSettings = secretlessSeedSettings.copy(
          walletSettings = secretlessSeedSettings.walletSettings.copy(
            testMnemonic = None,
            secretStorage = secretlessSeedSettings.walletSettings.secretStorage.copy(
              secretDir = new File(w.nodeViewDir, "secretless-rollback-empty-keystore").getAbsolutePath
            )
          )
        )
        val noSecretActor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
          secretlessSettings, parameters, new ErgoWalletServiceImpl(secretlessSettings), selector, getHistory
        )))
        secretlessActor = Some(noSecretActor)
        val secretlessReader = new ErgoWalletReader { override val walletActor = noSecretActor }
        secretlessProbe.watch(noSecretActor)
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(secretlessReader.getWalletStatus)
          status.height shouldBe thirdA.height
          status.error shouldBe None
          status.initialized shouldBe false
          await(secretlessReader.confirmedBalances).walletBalance shouldBe oldBalance
        }
        val unsignedBeforeFork = await(wallet.generateUnsignedTransaction(Seq(payment(initialBalance / 4)))).get

        applyBlock(secondB) shouldBe 'success
        applyBlock(thirdB) shouldBe 'success
        applyBlock(fourthB) shouldBe 'success
        eventually(timeout(10.seconds), interval(100.millis)) {
          getCurrentState.version shouldBe idToVersion(fourthB.id)
          getHistory.bestFullBlockOpt.map(_.id) shouldBe Some(fourthB.id)
        }
        val holderRollback = rollbackProbe.expectMsgType[HolderRollback](5.seconds)
        holderRollback.branchPoint shouldBe first.id
        secretlessProbe.send(noSecretActor, Rollback(idToVersion(holderRollback.branchPoint)))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(secretlessReader.getWalletStatus)
          status.height shouldBe thirdA.height
          status.error.value.toLowerCase should include("quarantine")
          Try(await(secretlessReader.confirmedBalances)).isFailure shouldBe true
        }
        secretlessProbe.send(noSecretActor, CloseWallet)
        secretlessProbe.expectTerminated(noSecretActor, 5.seconds)
        secretlessClosed = true
        val secretlessPreserved = ErgoWalletState.initial(secretlessSettings, parameters).get
        try {
          secretlessPreserved.registry.fetchDigest().height shouldBe thirdA.height
          secretlessPreserved.storage.deepForkQuarantine.get shouldBe true
        } finally {
          secretlessPreserved.registry.close()
          secretlessPreserved.storage.close()
        }
        val unmarkedBeforeRestart = ErgoWalletState.initial(unmarkedSettings, parameters).get
        try {
          unmarkedBeforeRestart.registry.fetchDigest().height shouldBe thirdA.height
          unmarkedBeforeRestart.storage.deepForkQuarantine.get shouldBe false
        } finally {
          unmarkedBeforeRestart.registry.close()
          unmarkedBeforeRestart.storage.close()
        }
        val unmarkedReopened = w.actorSystem.actorOf(Props(new ErgoWalletActor(
          unmarkedSettings, parameters, new ErgoWalletServiceImpl(unmarkedSettings), selector, getHistory
        )))
        val unmarkedReopenedReader = new ErgoWalletReader { override val walletActor = unmarkedReopened }
        val unmarkedReopenedProbe = TestProbe()(w.actorSystem)
        unmarkedReopenedProbe.watch(unmarkedReopened)
        try {
          eventually(timeout(10.seconds), interval(100.millis)) {
            val status = await(unmarkedReopenedReader.getWalletStatus)
            status.height shouldBe thirdA.height
            status.unlocked shouldBe false
            status.error.value.toLowerCase should include("quarantine")
          }
          Try(await(unmarkedReopenedReader.confirmedBalances)).isFailure shouldBe true
          await(unmarkedReopenedReader.rescanWallet(1)).isFailure shouldBe true
        } finally {
          unmarkedReopenedProbe.send(unmarkedReopened, CloseWallet)
          unmarkedReopenedProbe.expectTerminated(unmarkedReopened, 5.seconds)
        }
        val unmarkedAfterRestart = ErgoWalletState.initial(unmarkedSettings, parameters).get
        try {
          unmarkedAfterRestart.registry.fetchDigest().height shouldBe thirdA.height
          unmarkedAfterRestart.storage.deepForkQuarantine.get shouldBe true
        } finally {
          unmarkedAfterRestart.registry.close()
          unmarkedAfterRestart.storage.close()
        }
        actorProbe.send(actor, Rollback(idToVersion(holderRollback.branchPoint)))
        Seq(secondB, thirdB, fourthB).foreach(block => actorProbe.send(actor, ScanOnChain(block)))
        retainedProbe.send(retainedActor, Rollback(idToVersion(holderRollback.branchPoint)))
        Seq(secondB, thirdB, fourthB).foreach(block => retainedProbe.send(retainedActor, ScanOnChain(block)))

        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(retainedReader.getWalletStatus)
          status.height shouldBe fourthB.height
          status.error shouldBe None
          status.unlocked shouldBe true
          await(retainedReader.confirmedBalances).walletBalance shouldBe expectedB
        }
        retainedProbe.send(retainedActor, CloseWallet)
        retainedProbe.expectTerminated(retainedActor, 5.seconds)
        retainedClosed = true
        val retainedReopened = ErgoWalletState.initial(retainedSettings, parameters).get
        try {
          retainedReopened.registry.fetchDigest().height shouldBe fourthB.height
          retainedReopened.storage.deepForkQuarantine.get shouldBe false
        } finally {
          retainedReopened.registry.close()
          retainedReopened.storage.close()
        }

        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe thirdA.height
          status.unlocked shouldBe false
          status.error.value.toLowerCase should include("quarantine")
        }
        Try(await(reader.confirmedBalances)).isFailure shouldBe true
        Try(await(reader.walletBoxes(unspentOnly = true, considerUnconfirmed = false))).isFailure shouldBe true
        Try(await(reader.transactions)).isFailure shouldBe true
        Try(await(reader.generateUnsignedTransaction(Seq(payment(oldBalance / 2))))).isFailure shouldBe true
        Try(await(reader.signTransaction(unsignedBeforeFork, Seq.empty, TransactionHintsBag.empty, None, None)))
          .isFailure shouldBe true
        await(reader.rescanWallet(1)).isFailure shouldBe true

        actorProbe.send(actor, CloseWallet)
        actorProbe.expectTerminated(actor, 5.seconds)
        actorClosed = true
        val reopened = ErgoWalletState.initial(actorSettings, parameters).get
        try {
          reopened.registry.fetchDigest().height shouldBe thirdA.height
          reopened.registry.fetchDigest().walletBalance shouldBe oldBalance
          reopened.storage.deepForkQuarantine.get shouldBe true
        } finally {
          reopened.registry.close()
          reopened.storage.close()
        }

        val restarted = w.actorSystem.actorOf(Props(new ErgoWalletActor(
          actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, getHistory
        )))
        val restartedReader = new ErgoWalletReader { override val walletActor = restarted }
        val restartProbe = TestProbe()(w.actorSystem)
        restartProbe.watch(restarted)
        try {
          eventually(timeout(10.seconds), interval(100.millis)) {
            val status = await(restartedReader.getWalletStatus)
            status.height shouldBe thirdA.height
            status.unlocked shouldBe false
            status.error.value.toLowerCase should include("quarantine")
          }
          Try(await(restartedReader.confirmedBalances)).isFailure shouldBe true
          Try(await(restartedReader.generateUnsignedTransaction(Seq(payment(oldBalance / 2))))).isFailure shouldBe true
          await(restartedReader.rescanWallet(1)).isFailure shouldBe true
        } finally {
          restartProbe.send(restarted, CloseWallet)
          restartProbe.expectTerminated(restarted, 5.seconds)
        }

        val noSecretSettings = actorSettings.copy(
          walletSettings = actorSettings.walletSettings.copy(
            testMnemonic = None,
            secretStorage = actorSettings.walletSettings.secretStorage.copy(
              secretDir = new File(w.nodeViewDir, "guarded-empty-keystore").getAbsolutePath
            )
          )
        )
        val guarded = w.actorSystem.actorOf(Props(new ErgoWalletActor(
          noSecretSettings, parameters, new ErgoWalletServiceImpl(noSecretSettings), selector, getHistory
        )))
        val guardedReader = new ErgoWalletReader { override val walletActor = guarded }
        val guardedProbe = TestProbe()(w.actorSystem)
        guardedProbe.watch(guarded)
        try {
          eventually(timeout(10.seconds), interval(100.millis)) {
            val status = await(guardedReader.getWalletStatus)
            status.initialized shouldBe false
            status.unlocked shouldBe false
            status.error.value.toLowerCase should include("quarantine")
          }
          await(guardedReader.initWallet(SecretString.create("local-test-pass"), None)).isFailure shouldBe true
          await(guardedReader.restoreWallet(
            SecretString.create("local-test-pass"), SecretString.create("local test mnemonic"),
            None, usePre1627KeyDerivation = false
          )).isFailure shouldBe true
          await(guardedReader.rescanWallet(1)).isFailure shouldBe true
        } finally {
          guardedProbe.send(guarded, CloseWallet)
          guardedProbe.expectTerminated(guarded, 5.seconds)
        }
        val stillPreserved = ErgoWalletState.initial(actorSettings, parameters).get
        try {
          stillPreserved.registry.fetchDigest().height shouldBe thirdA.height
          stillPreserved.registry.fetchDigest().walletBalance shouldBe oldBalance
          stillPreserved.storage.deepForkQuarantine.get shouldBe true
        } finally {
          stillPreserved.registry.close()
          stillPreserved.storage.close()
        }
      } finally {
        w.actorSystem.eventStream.unsubscribe(rollbackProbe.ref)
        if (!retainedClosed) {
          retainedProbe.send(retainedActor, CloseWallet)
          retainedProbe.expectTerminated(retainedActor, 5.seconds)
        }
        if (!unmarkedClosed) {
          unmarkedProbe.send(unmarkedActor, CloseWallet)
          unmarkedProbe.expectTerminated(unmarkedActor, 5.seconds)
        }
        if (!secretlessSeedClosed) {
          secretlessSeedProbe.send(secretlessSeedActor, CloseWallet)
          secretlessSeedProbe.expectTerminated(secretlessSeedActor, 5.seconds)
        }
        if (!secretlessClosed) secretlessActor.foreach { ref =>
          secretlessProbe.send(ref, CloseWallet)
          secretlessProbe.expectTerminated(ref, 5.seconds)
        }
        if (!actorClosed) {
          actorProbe.send(actor, CloseWallet)
          actorProbe.expectTerminated(actor, 5.seconds)
        }
      }
    }
  }

  property("a selected-chain wallet behind the fork point catches up without quarantine") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val initialBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      val secondATx = await(wallet.generateTransaction(Seq(PaymentRequest(
        Pay2SAddress(TrueTree)(w.settings.addressEncoder), initialBalance / 2, Array.empty, Map.empty
      )))).get
      val secondA = makeNextBlock(getUtxoState, Seq(secondATx))
      applyBlock(secondA) shouldBe 'success

      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "lagging-selected-wallet").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 1, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, getHistory
      )))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val probe = TestProbe()(w.actorSystem)
      probe.watch(actor)
      val rollbackProbe = TestProbe()(w.actorSystem)
      w.actorSystem.eventStream.subscribe(rollbackProbe.ref, classOf[HolderRollback])
      var actorClosed = false
      try {
        probe.send(actor, ScanOnChain(first))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe first.height
          status.error shouldBe None
        }

        val (forkUtxo, boxHolder) = ValidBlocksGenerators.createUtxoState(w.settings)
        val forkAtSecondA = WrappedUtxoState(forkUtxo, boxHolder, w.settings)
          .applyModifier(first)(_ => ()).get
          .applyModifier(secondA)(_ => ()).get
        def nextForkBlock(parent: ErgoFullBlock, forkState: WrappedUtxoState): ErgoFullBlock = {
          val external = parent.blockTransactions.txs.flatMap(_.outputs)
            .find(_.ergoTree == TrueTree).get
          val tx = ErgoNodeTransactionGenerators.validTransactionFromBoxes(
            IndexedSeq(external), stateCtxOpt = Some(forkState.stateContext)
          )
          ValidBlocksGenerators.validFullBlock(Some(parent), forkState, Seq(tx))
        }
        val secondAExternal = secondA.blockTransactions.txs.flatMap(_.outputs)
          .find(_.ergoTree == TrueTree).get
        val thirdBTx = ErgoNodeTransactionGenerators.validTransactionFromBoxes(
          IndexedSeq(secondAExternal), stateCtxOpt = Some(forkAtSecondA.stateContext)
        )
        val thirdB = ValidBlocksGenerators.validFullBlock(
          Some(secondA), forkAtSecondA, Seq(thirdBTx), Some(secondA.header.timestamp + 101)
        )
        val forkAtThirdB = forkAtSecondA.applyModifier(thirdB)(_ => ()).get
        val fourthB = nextForkBlock(thirdB, forkAtThirdB)
        val forkAtFourthB = forkAtThirdB.applyModifier(fourthB)(_ => ()).get
        val fifthB = nextForkBlock(fourthB, forkAtFourthB)

        val thirdATx = ErgoNodeTransactionGenerators.validTransactionFromBoxes(
          IndexedSeq(secondAExternal), stateCtxOpt = Some(getUtxoState.stateContext)
        )
        val thirdA = makeNextBlock(getUtxoState, Seq(thirdATx))
        thirdA.id should not equal thirdB.id
        applyBlock(thirdA) shouldBe 'success
        val thirdAExternal = thirdA.blockTransactions.txs.flatMap(_.outputs)
          .find(_.ergoTree == TrueTree).get
        val fourthATx = ErgoNodeTransactionGenerators.validTransactionFromBoxes(
          IndexedSeq(thirdAExternal), stateCtxOpt = Some(getUtxoState.stateContext)
        )
        val fourthA = makeNextBlock(getUtxoState, Seq(fourthATx))
        applyBlock(fourthA) shouldBe 'success
        Seq(thirdB, fourthB, fifthB).foreach(block => applyBlock(block) shouldBe 'success)
        eventually(timeout(10.seconds), interval(100.millis)) {
          getCurrentState.version shouldBe idToVersion(fifthB.id)
          getHistory.bestFullBlockOpt.map(_.id) shouldBe Some(fifthB.id)
        }
        val holderRollback = rollbackProbe.expectMsgType[HolderRollback](5.seconds)
        holderRollback.branchPoint shouldBe secondA.id

        // A new, genuinely empty registry remains usable while it catches up.
        val emptySettings = actorSettings.copy(
          directory = new File(w.nodeViewDir, "empty-selected-wallet").getAbsolutePath
        )
        val emptyActor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
          emptySettings, parameters, new ErgoWalletServiceImpl(emptySettings), selector, getHistory
        )))
        val emptyReader = new ErgoWalletReader { override val walletActor = emptyActor }
        val emptyProbe = TestProbe()(w.actorSystem)
        emptyProbe.watch(emptyActor)
        try {
          eventually(timeout(10.seconds), interval(100.millis)) {
            val status = await(emptyReader.getWalletStatus)
            status.height shouldBe 0
            status.error shouldBe None
            status.unlocked shouldBe true
          }
          val manualBox = boxesAvailable(secondA, getPublicKeys.head.pubkey).head
          await(emptyReader.addBox(manualBox, Set(PaymentsScanId))).status.isSuccess shouldBe true
          eventually(timeout(10.seconds), interval(100.millis)) {
            val digest = await(emptyReader.confirmedBalances)
            digest.height shouldBe 0
            digest.walletBalance shouldBe manualBox.value
          }
        } finally {
          emptyProbe.send(emptyActor, CloseWallet)
          emptyProbe.expectTerminated(emptyActor, 5.seconds)
        }
        val emptyReopened = w.actorSystem.actorOf(Props(new ErgoWalletActor(
          emptySettings, parameters, new ErgoWalletServiceImpl(emptySettings), selector, getHistory
        )))
        val emptyReopenedReader = new ErgoWalletReader { override val walletActor = emptyReopened }
        val emptyReopenedProbe = TestProbe()(w.actorSystem)
        emptyReopenedProbe.watch(emptyReopened)
        try {
          eventually(timeout(10.seconds), interval(100.millis)) {
            val status = await(emptyReopenedReader.getWalletStatus)
            status.height shouldBe 0
            status.error shouldBe None
            status.unlocked shouldBe true
            await(emptyReopenedReader.confirmedBalances).walletBalance should be > 0L
          }
        } finally {
          emptyReopenedProbe.send(emptyReopened, CloseWallet)
          emptyReopenedProbe.expectTerminated(emptyReopened, 5.seconds)
        }

        probe.send(actor, Rollback(idToVersion(holderRollback.branchPoint)))
        Seq(thirdB, fourthB, fifthB).foreach(block => probe.send(actor, ScanOnChain(block)))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe fifthB.height
          status.error shouldBe None
          status.unlocked shouldBe true
          await(reader.confirmedBalances).height shouldBe fifthB.height
        }
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
        actorClosed = true
        val reopened = ErgoWalletState.initial(actorSettings, parameters).get
        try {
          reopened.storage.deepForkQuarantine.get shouldBe false
        } finally {
          reopened.registry.close()
          reopened.storage.close()
        }
      } finally {
        w.actorSystem.eventStream.unsubscribe(rollbackProbe.ref)
        if (!actorClosed) {
          probe.send(actor, CloseWallet)
          probe.expectTerminated(actor, 5.seconds)
        }
      }
    }
  }

  property("quarantine storage rejects write, readback, and malformed-marker faults") {
    val settings = org.ergoplatform.utils.ErgoNodeTestConstants.settings
    val writeFailure = new LDBKVStore(null) {
      override def get(key: Array[Byte]): Option[Array[Byte]] = None
      override def insertSync(key: Array[Byte], value: Array[Byte]): Try[Unit] =
        Failure(new IOException("injected marker write failure"))
    }
    new WalletStorage(writeFailure, settings).quarantineDeepFork().isFailure shouldBe true

    var reads = 0
    val readbackFailure = new LDBKVStore(null) {
      override def get(key: Array[Byte]): Option[Array[Byte]] = {
        reads += 1
        if (reads == 1) None else throw new IOException("injected marker readback failure")
      }
      override def insertSync(key: Array[Byte], value: Array[Byte]): Try[Unit] = Success(())
    }
    new WalletStorage(readbackFailure, settings).quarantineDeepFork().isFailure shouldBe true

    val clearWriteFailure = new LDBKVStore(null) {
      override def get(key: Array[Byte]): Option[Array[Byte]] = Some(Array(1: Byte))
      override def insertSync(key: Array[Byte], value: Array[Byte]): Try[Unit] =
        Failure(new IOException("injected marker clear failure"))
    }
    val clearStore = new WalletStorage(clearWriteFailure, settings)
    clearStore.clearDeepForkQuarantine().isFailure shouldBe true
    clearStore.deepForkQuarantine.get shouldBe true

    val clearReadbackFailure = new LDBKVStore(null) {
      override def get(key: Array[Byte]): Option[Array[Byte]] = Some(Array(1: Byte))
      override def insertSync(key: Array[Byte], value: Array[Byte]): Try[Unit] = Success(())
    }
    new WalletStorage(clearReadbackFailure, settings).clearDeepForkQuarantine().isFailure shouldBe true

    val malformed = new LDBKVStore(null) {
      override def get(key: Array[Byte]): Option[Array[Byte]] = Some(Array(2: Byte))
    }
    new WalletStorage(malformed, settings).deepForkQuarantine.isFailure shouldBe true
  }

  property("quarantine status sees encrypted secret-file metadata without loading signing material") {
    withFixture { implicit w =>
      val secretDir = new File(w.nodeViewDir, "fenced-secret-wallet/keystore")
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "fenced-secret-wallet").getAbsolutePath,
        walletSettings = w.settings.walletSettings.copy(
          testMnemonic = None,
          secretStorage = w.settings.walletSettings.secretStorage.copy(secretDir = secretDir.getAbsolutePath)
        )
      )
      val pass = SecretString.create("local-test-pass")
      val secret = JsonSecretStorage.init(Array.fill(32)(7: Byte), pass,
        usePre1627KeyDerivation = false)(actorSettings.walletSettings.secretStorage)
      pass.erase()
      secret.secretFile.exists() shouldBe true
      JsonSecretStorage.readFile(actorSettings.walletSettings.secretStorage).isSuccess shouldBe true
      val storage = WalletStorage.readOrCreate(actorSettings)
      try storage.quarantineDeepFork().get finally storage.close()
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, getHistory
      )))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val probe = TestProbe()(w.actorSystem)
      probe.watch(actor)
      try {
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.initialized shouldBe true
          status.unlocked shouldBe false
          status.error.value.toLowerCase should include("quarantine")
        }
        Try(await(reader.confirmedBalances)).isFailure shouldBe true
      } finally {
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }
    }
  }

  property("an inconsistent checkpoint and marker write fault fail the wallet actor system closed") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val initialBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      val secondTx = await(wallet.generateTransaction(Seq(PaymentRequest(
        Pay2SAddress(TrueTree)(w.settings.addressEncoder), initialBalance / 2, Array.empty, Map.empty
      )))).get
      val second = makeNextBlock(getUtxoState, Seq(secondTx))
      applyBlock(second) shouldBe 'success
      val external = second.blockTransactions.txs.flatMap(_.outputs)
        .find(_.ergoTree == TrueTree).get
      val thirdTx = ErgoNodeTransactionGenerators.validTransactionFromBoxes(
        IndexedSeq(external), stateCtxOpt = Some(getUtxoState.stateContext)
      )
      val third = makeNextBlock(getUtxoState, Seq(thirdTx))
      applyBlock(third) shouldBe 'success
      getHistory.bestFullBlockOpt.map(_.id) shouldBe Some(third.id)

      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "marker-write-fault").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 1, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      // The shared test config disables system termination during coordinated
      // shutdown; use a dedicated system to check the production stop path.
      val stopOnShutdown = ConfigFactory.parseString(
        "akka.coordinated-shutdown.terminate-actor-system = on"
      ).withFallback(ConfigFactory.load())
      val failStopSystem = ActorSystem("wallet-deep-fork-fail-stop", stopOnShutdown)
      failStopSystem.settings.config.getBoolean(
        "akka.coordinated-shutdown.terminate-actor-system"
      ) shouldBe true
      val failCheckpoint = new AtomicBoolean(false)
      val markerAttempted = new AtomicBoolean(false)
      val actor = failStopSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, getHistory
      ) {
        override protected[wallet] def registryCheckpoint(
          state: ErgoWalletState
        ): Try[(scorex.util.ModifierId, Int)] =
          if (failCheckpoint.get()) Failure(new IOException("injected checkpoint inconsistency"))
          else super.registryCheckpoint(state)

        override protected def persistDeepForkQuarantine(state: ErgoWalletState): Try[Unit] = {
          markerAttempted.set(true)
          Failure(new IOException("injected marker write failure"))
        }
      }))
      val probe = TestProbe()(failStopSystem)
      probe.send(actor, ScanOnChain(first))
      probe.send(actor, ScanOnChain(second))
      probe.send(actor, ScanOnChain(third))
      probe.send(actor, GetWalletStatus)
      probe.expectMsgType[WalletStatus](5.seconds).height shouldBe third.height
      // A stale rollback alone is ignored while the wallet remains selected.
      // Inject an inconsistent checkpoint to exercise marker-failure shutdown.
      failCheckpoint.set(true)
      probe.send(actor, Rollback(idToVersion(first.id)))
      Await.result(failStopSystem.whenTerminated, 20.seconds)
      markerAttempted.get() shouldBe true
    }
  }
}
