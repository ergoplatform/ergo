package org.ergoplatform.nodeView.history

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.gracefulStop
import org.ergoplatform.{ErgoAddress, ErgoBox, Pay2SAddress}
import org.ergoplatform.mining.groupElemToBytes
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoNodeViewHolder.CurrentView
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import org.ergoplatform.nodeView.history.UtxoSetScanner.{InitializeUtxoSetScannerWithHistory, StartUtxoSetScan}
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages.ScanBoxesFromUtxoSnapshot
import org.ergoplatform.nodeView.wallet.requests.PaymentRequest
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.settings.{ErgoSettings, Parameters}
import org.ergoplatform.utils.fixtures.WalletFixture
import org.ergoplatform.utils.{ErgoCorePropertyTest, WalletTestOps}
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer
import org.ergoplatform.wallet.utils.FileUtils
import org.scalatest.OptionValues
import org.scalatest.concurrent.Eventually
import scorex.crypto.authds.avltree.batch.VersionedLDBAVLStorage
import scorex.util.encode.Base16
import scorex.util.{ModifierId, bytesToId}

import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._

class UtxoSetScannerFailureSpec extends ErgoCorePropertyTest with WalletTestOps with Eventually with FileUtils
  with OptionValues {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._

  /** Wallet whose snapshot scan fails once, at the first subtree holding a box that `failOn` matches. */
  private class FailOnceWallet(h: ErgoHistoryReader, s: ErgoSettings, p: Parameters, failOn: ErgoBox => Boolean)
                              (implicit system: ActorSystem) extends ErgoWallet(h, s, p) {
    @volatile var failed = false

    override def scanUtxoSnapshot(msg: ScanBoxesFromUtxoSnapshot): Int = {
      val k = msg.subtrees.indexWhere(_._2.exists(failOn))
      if (!failed && k >= 0) {
        failed = true
        val first = msg.current - msg.subtrees.size + 1
        // the chunks before the failing one are scanned; the reply is the last chunk scanned
        if (k == 0) first - 1 else super.scanUtxoSnapshot(msg.copy(subtrees = msg.subtrees.take(k), current = first + k - 1))
      } else {
        super.scanUtxoSnapshot(msg)
      }
    }
  }

  /** Node view proxy: hands the scanner `wallet` instead of the node's own wallet. */
  private class SwapWalletProxy(nodeView: ActorRef, wallet: ErgoWallet) extends Actor {
    override def receive: Receive = {
      case GetDataFromCurrentView(f) =>
        val g = f.asInstanceOf[CurrentView[Any] => Any]
        nodeView.forward(GetDataFromCurrentView[Any, Any](cv => g(cv.copy(vault = wallet))))
      case m => nodeView.forward(m)
    }
  }

  private def withUtxoBootstrapFixture[T](test: WalletFixture => T): T = {
    val utxoBootstrapSettings = settings.copy(
      nodeSettings = settings.nodeSettings.copy(
        utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)
      )
    )
    new WalletFixture(utxoBootstrapSettings, parameters, getCurrentView(_).vault).apply(test)
  }

  property("a failed UTXO snapshot chunk is scanned again when the scan resumes") {
    withUtxoBootstrapFixture { implicit w =>
      implicit val system: ActorSystem = w.actorSystem
      val address = getPublicKeys.head
      val pkBytes = groupElemToBytes(address.pubkey.value)
      val testWallet = new FailOnceWallet(getHistory, w.settings.copy(directory = createTempDir.getAbsolutePath),
        parameters, _.propositionBytes.containsSlice(pkBytes))
      val proxy = system.actorOf(Props(new SwapWalletProxy(w.nodeViewHolderRef, testWallet)))
      val scannerRef = UtxoSetScanner(proxy, depth = 3)
      try {
        val genesisBlock = makeGenesisBlock(address.pubkey, randomNewAsset)
        val expectedGenesisBalance = balanceAmount(boxesAvailable(genesisBlock, address.pubkey))
        expectedGenesisBalance should be > 0L

        applyBlock(genesisBlock) shouldBe 'success

        implicit val patienceConfig: PatienceConfig = PatienceConfig(10.seconds, 100.millis)
        eventually {
          testWallet.failed shouldBe true
        }
        val afterFailure = await(testWallet.confirmedBalances)
        info(s"after failure: height=${afterFailure.height} balance=${afterFailure.walletBalance}")
        afterFailure.height shouldBe 0
        afterFailure.walletBalance shouldBe 0L

        system.eventStream.publish(StartUtxoSetScan(rescan = false))

        eventually {
          val resumed = await(testWallet.confirmedBalances)
          resumed.height shouldBe genesisBlock.height
          resumed.walletBalance shouldBe expectedGenesisBalance
        }
      } finally {
        system.stop(scannerRef)
        system.stop(proxy)
        system.stop(testWallet.walletActor)
      }
    }
  }

  // The tests below run the scanner against the node's own wallet. `DisturbedWallet` stands between the two and
  // can disturb one batch. The oracle is the UTXO set: the wallet must hold exactly its boxes in it.

  private type Disturbance = PartialFunction[ScanBoxesFromUtxoSnapshot, Int]

  /** Passes every batch to `real`, the node's wallet, except the first batch `disturbance` is defined at. */
  private class DisturbedWallet(h: ErgoHistoryReader, s: ErgoSettings, p: Parameters, real: ErgoWallet)
                               (implicit system: ActorSystem) extends ErgoWallet(h, s, p) {
    @volatile var disturbance: Option[Disturbance] = None
    @volatile var disturbed = false
    val scansCompleted = new AtomicInteger(0)

    override def scanUtxoSnapshot(msg: ScanBoxesFromUtxoSnapshot): Int =
      disturbance.filter(_.isDefinedAt(msg)) match {
        case Some(disturb) =>
          disturbance = None
          try disturb(msg) finally disturbed = true
        case None =>
          val reply = real.scanUtxoSnapshot(msg)
          if (reply == msg.total) scansCompleted.incrementAndGet()
          reply
      }

    // a block applied during a scan is sent by the scanner after it; it goes to the node's wallet as well
    override def scanPersistent(modifier: BlockSection): ErgoWallet = {
      real.scanPersistent(modifier)
      this
    }
  }

  /** What `withScannedWallet` hands to a test. */
  private case class ScanSetup(disturbedWallet: DisturbedWallet, proxy: ActorRef, scanner: ActorRef)

  /** Scanner depth at which the small UTXO set of these tests is a shallow tree: every box is a subtree. */
  private val shallowDepth = 10

  implicit override val patienceConfig: PatienceConfig = PatienceConfig(30.seconds, 100.millis)

  private def boxId(box: ErgoBox): ModifierId = bytesToId(box.id)

  /** All boxes of the UTXO set, in the order the scanner visits them. */
  private def utxoSetBoxes(implicit w: WalletFixture): Vector[ErgoBox] = {
    val boxes = Vector.newBuilder[ErgoBox]
    getUtxoState.persistentProver.storage.asInstanceOf[VersionedLDBAVLStorage].iterateAVLTree(0, shallowDepth) { subtree =>
      boxes ++= subtree.leafValues.flatMap(ErgoBoxSerializer.parseBytesTry(_).toOption)
    }
    boxes.result()
  }

  private def isWalletBox(box: ErgoBox)(implicit w: WalletFixture): Boolean =
    getPublicKeys.exists(pk => box.propositionBytes.containsSlice(groupElemToBytes(pk.pubkey.value)))

  private def unspentWalletBoxIds(implicit w: WalletFixture): Set[ModifierId] =
    await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = false)).map(b => boxId(b.trackedBox.box)).toSet

  /** The wallet holds exactly its boxes of the UTXO set, each counted once in the balance. */
  private def walletShouldEqualUtxoSet()(implicit w: WalletFixture): Unit = {
    val expected = utxoSetBoxes.filter(isWalletBox)
    val expectedIds = expected.map(boxId).toSet
    val unspentIds = unspentWalletBoxIds
    withClue("boxes in the wallet that are not in the UTXO set: ")(unspentIds -- expectedIds shouldBe empty)
    withClue("wallet boxes of the UTXO set that the wallet does not have: ")(expectedIds -- unspentIds shouldBe empty)
    withClue("wallet balance: ")(getConfirmedBalances.walletBalance shouldBe expected.map(_.value).sum)
  }

  /**
    * Runs `test` on a node with two blocks applied, whose wallet holds 80 boxes of its own in a UTXO set of more
    * than 64 subtrees, so a scan takes three batches. The scanner gets a `DisturbedWallet` as its wallet.
    */
  private def withScannedWallet[T](test: WalletFixture => ScanSetup => T): T =
    withUtxoBootstrapFixture { implicit w =>
      implicit val system: ActorSystem = w.actorSystem
      val disturbedWallet = new DisturbedWallet(getHistory, w.settings.copy(directory = createTempDir.getAbsolutePath),
        parameters, wallet)
      val proxy = system.actorOf(Props(new SwapWalletProxy(w.nodeViewHolderRef, disturbedWallet)))
      val scannerRef = UtxoSetScanner(proxy, shallowDepth)
      try {
        val address = getPublicKeys.head
        val genesisBlock = makeGenesisBlock(address.pubkey, randomNewAsset)
        applyBlock(genesisBlock) shouldBe 'success
        eventually(getConfirmedBalances.height shouldBe genesisBlock.height)

        val payments = Seq.fill(80)(PaymentRequest(address, 1000000L, Array.empty, Map.empty))
        val block2 = makeNextBlock(getUtxoState, Seq(await(wallet.generateTransaction(payments)).get))
        applyBlock(block2) shouldBe 'success
        eventually(getConfirmedBalances.height shouldBe block2.height)
        utxoSetBoxes.size should be > 64
        walletShouldEqualUtxoSet()

        test(w)(ScanSetup(disturbedWallet, proxy, scannerRef))
      } finally {
        system.stop(scannerRef)
        system.stop(proxy)
        system.stop(disturbedWallet.walletActor)
      }
    }

  /** Asks the wallet for a rescan with `disturbance` armed and waits until the disturbed batch has been handled. */
  private def startDisturbedScan(disturbedWallet: DisturbedWallet, disturbance: Disturbance)
                                (implicit w: WalletFixture): Unit = {
    disturbedWallet.disturbance = Some(disturbance)
    await(wallet.rescanWallet(0)) shouldBe 'success
    eventually(disturbedWallet.disturbed shouldBe true)
  }

  /** The batch fails at the subtree holding `box`: the subtrees before it are scanned, the reply is the last one scanned. */
  private def failAt(box: ErgoBox)(implicit w: WalletFixture): Disturbance = {
    def position(msg: ScanBoxesFromUtxoSnapshot): Int =
      msg.subtrees.indexWhere { case (_, boxes) => boxes.exists(boxId(_) == boxId(box)) }

    {
      case msg if position(msg) >= 0 =>
        val k = position(msg)
        val first = msg.current - msg.subtrees.size + 1
        if (k == 0) first - 1
        else wallet.scanUtxoSnapshot(msg.copy(subtrees = msg.subtrees.take(k), current = first + k - 1))
    }
  }

  /** Transaction made by the wallet that spends its `box`, which holds no tokens, to `address`. */
  private def spendTo(address: ErgoAddress, box: ErgoBox)(implicit w: WalletFixture): ErgoTransaction =
    await(wallet.generateTransaction(
      Seq(PaymentRequest(address, box.value, Array.empty, Map.empty)),
      inputsRaw = Seq(Base16.encode(ErgoBoxSerializer.toBytes(box)))
    )).get

  private def spendToAnyone(box: ErgoBox)(implicit w: WalletFixture): ErgoTransaction =
    spendTo(Pay2SAddress(TrueTree)(w.settings.addressEncoder), box)

  /** Applies a block holding `tx` and waits until the wallet is at the block's height. */
  private def applyBlockAndAwaitWallet(tx: ErgoTransaction)(implicit w: WalletFixture): Unit = {
    val block = makeNextBlock(getUtxoState, Seq(tx))
    applyBlock(block) shouldBe 'success
    eventually(getConfirmedBalances.height shouldBe block.height)
  }

  property("a scan resumed after a block spent a wallet box it had scanned does not keep that box") {
    withScannedWallet { implicit w => setup =>
      val walletBoxes = utxoSetBoxes.filter(isWalletBox)
      val scannedBox = walletBoxes.find(_.additionalTokens.isEmpty).value
      val spendScannedBox = spendToAnyone(scannedBox)

      startDisturbedScan(setup.disturbedWallet, failAt(walletBoxes.last))
      getConfirmedBalances.height shouldBe 0
      unspentWalletBoxIds should contain(boxId(scannedBox)) // scanned before the failure

      // the wallet is at height 0, so the node view holder asks for the scan to go on after the block
      applyBlockAndAwaitWallet(spendScannedBox)
      walletShouldEqualUtxoSet()
    }
  }

  property("a scan resumed after a block created a wallet box among the subtrees it had scanned finds that box") {
    withScannedWallet { implicit w => setup =>
      val boxes = utxoSetBoxes
      // The block spends the wallet box the scan fails at, which is not scanned yet, to the wallet. The failure
      // is put where the new wallet box sorts before the box scanned last, so the resumed scan does not pass it.
      val (failedBox, payWallet) = boxes.zipWithIndex.reverseIterator
        .collect { case (box, i) if i > 0 && isWalletBox(box) && box.additionalTokens.isEmpty =>
          (box, boxes(i - 1), spendTo(getPublicKeys.head, box))
        }
        .collectFirst { case (box, scannedLast, tx) if tx.outputs.exists(o => boxId(o) < boxId(scannedLast)) => (box, tx) }
        .value

      startDisturbedScan(setup.disturbedWallet, failAt(failedBox))
      getConfirmedBalances.height shouldBe 0

      applyBlockAndAwaitWallet(payWallet)
      walletShouldEqualUtxoSet()
    }
  }

  property("a scan resumed by a restarted node after a block spent a wallet box it had scanned does not keep that box") {
    withScannedWallet { implicit w => setup =>
      implicit val system: ActorSystem = w.actorSystem
      val walletBoxes = utxoSetBoxes.filter(isWalletBox)
      val scannedBox = walletBoxes.find(_.additionalTokens.isEmpty).value
      val spendScannedBox = spendToAnyone(scannedBox)

      startDisturbedScan(setup.disturbedWallet, failAt(walletBoxes.last))
      unspentWalletBoxIds should contain(boxId(scannedBox))

      // the node stops with the scan unfinished, a block is applied, and the scanner of the restarted node goes on
      await(gracefulStop(setup.scanner, 10.seconds)) shouldBe true
      val block = makeNextBlock(getUtxoState, Seq(spendScannedBox))
      applyBlock(block) shouldBe 'success
      val restartedScanner = UtxoSetScanner(setup.proxy, shallowDepth)
      try {
        restartedScanner ! InitializeUtxoSetScannerWithHistory(getHistory)
        eventually(getConfirmedBalances.height shouldBe block.height)
        walletShouldEqualUtxoSet()
      } finally {
        system.stop(restartedScanner)
      }
    }
  }

  /**
    * The wallet scans the batch `isLate` picks, but its reply comes after the scanner has stopped waiting for it.
    * After the next scan request the wallet must equal the UTXO set.
    */
  private def scanWithLateReply(isLate: ScanBoxesFromUtxoSnapshot => Boolean): Unit =
    withScannedWallet { implicit w => setup =>
      val scansBefore = setup.disturbedWallet.scansCompleted.get
      val lateReply: Disturbance = {
        case msg if isLate(msg) =>
          wallet.scanUtxoSnapshot(msg)
          throw new TimeoutException("wallet reply timed out")
      }
      startDisturbedScan(setup.disturbedWallet, lateReply)

      w.actorSystem.eventStream.publish(StartUtxoSetScan(rescan = false))
      eventually(setup.disturbedWallet.scansCompleted.get shouldBe scansBefore + 1)
      walletShouldEqualUtxoSet()
    }

  property("a scan is not complete when the wallet's reply to a batch does not arrive in time") {
    scanWithLateReply(_.current == 64)
  }

  property("a scan does not count its last batch twice when the wallet's reply to it does not arrive in time") {
    scanWithLateReply(msg => msg.current == msg.total)
  }

  property("a wallet rescan requested while a scan is running does not count boxes twice") {
    withScannedWallet { implicit w => setup =>
      val scansBefore = setup.disturbedWallet.scansCompleted.get
      // the rescan request reaches the wallet between the second and the last batch of a running scan
      val rescanRequest: Disturbance = {
        case msg if msg.current == msg.total =>
          await(wallet.rescanWallet(0)) shouldBe 'success
          wallet.scanUtxoSnapshot(msg)
      }
      startDisturbedScan(setup.disturbedWallet, rescanRequest)

      // the scan that was running ends with the disturbed batch, then the requested scan runs
      eventually(setup.disturbedWallet.scansCompleted.get shouldBe scansBefore + 1)
      walletShouldEqualUtxoSet()
    }
  }
}
