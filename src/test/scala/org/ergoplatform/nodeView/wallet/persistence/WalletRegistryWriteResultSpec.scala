package org.ergoplatform.nodeView.wallet.persistence

import akka.testkit.{TestActorRef, TestKit, TestProbe}
import org.ergoplatform.ErgoBoxCandidate
import org.ergoplatform.db.DBSpec
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages.{AddBox, AddBoxResponse, ReadWallet}
import org.ergoplatform.nodeView.wallet.WalletScanLogic.{ScanResults, SpentInputData}
import org.ergoplatform.nodeView.wallet.{ErgoWalletActor, ErgoWalletServiceImpl, ErgoWalletState, WalletVars}
import org.ergoplatform.sdk.SecretString
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.utils.ErgoNodeTestConstants.{extendedParameters, settings}
import org.ergoplatform.wallet.Constants.{MiningScanId, PaymentsScanId, ScanId}
import org.ergoplatform.wallet.boxes.TrackedBox
import org.ergoplatform.wallet.settings.SecretStorageSettings
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.db.LDBVersionedStore
import scorex.testkit.utils.AkkaFixture
import scorex.util.bytesToId

import scala.util.{Failure, Success, Try}

class WalletRegistryWriteResultSpec extends AnyFlatSpec with Matchers with DBSpec {
  info(s"Registry persistence backend: ${scorex.db.LDBFactory.factory.asInstanceOf[scorex.db.StoreRegistry].factory.getClass.getName}")
  private val appScan = ScanId @@ 11.toShort
  private val nextScan = ScanId @@ 12.toShort
  private val box = new ErgoBoxCandidate(1000000L, TrueTree, 1)
    .toBox(bytesToId(Array.fill(32)(1.toByte)), 0)
  private val tracked = TrackedBox(box, 1, Set(appScan))
  private val failure = new IllegalStateException("registry write sentinel")

  private class ControlledStore extends LDBVersionedStore(createTempDir, 10) {
    var rejectWrites = false
    override def update(versionID: Array[Byte],
                        toRemove: TraversableOnce[Array[Byte]],
                        toUpdate: TraversableOnce[(Array[Byte], Array[Byte])]): Try[Unit] =
      if (rejectWrites) Failure(failure) else super.update(versionID, toRemove, toUpdate)
  }

  private def withRegistry(keepSpent: Boolean = true)(body: (ControlledStore, WalletRegistry) => Unit): Unit = {
    val store = new ControlledStore
    val registry = new WalletRegistry(store)(settings.walletSettings.copy(keepSpentBoxes = keepSpent, testMnemonic = None))
    try body(store, registry) finally registry.close()
  }

  for ((label, original, updated) <- Seq(
    ("insert", Set.empty[ScanId], Set(appScan)),
    ("replace", Set(appScan), Set(nextScan)),
    ("remove", Set(appScan), Set.empty[ScanId])
  )) {
    def prepare(store: ControlledStore, registry: WalletRegistry): Unit = {
      if (original.nonEmpty) registry.updateScans(original, box).get
      registry.getBox(box.id)
      store.rejectWrites = true
    }

    it should s"return the persistence failure for scan $label" in withRegistry() { (store, registry) =>
      prepare(store, registry)
      registry.updateScans(updated, box) shouldBe Failure(failure)
    }

    it should s"keep cached and persisted metadata aligned after failed scan $label" in withRegistry() { (store, registry) =>
      prepare(store, registry)
      registry.updateScans(updated, box)
      val persisted = new WalletRegistry(store)(settings.walletSettings).getBox(box.id)
      registry.getBox(box.id).map(_.scans) shouldBe persisted.map(_.scans)
      persisted.map(_.scans) shouldBe (if (original.isEmpty) None else Some(original))
      registry.unspentBoxes(appScan).map(_.scans) shouldBe persisted.toSeq.map(_.scans)
    }
  }

  it should "leave new outputs invisible when a block write fails" in withRegistry() { (store, registry) =>
    store.rejectWrites = true
    registry.updateOnBlock(ScanResults(Seq(tracked), Seq.empty, Seq.empty), bytesToId(versionId("block")), 1) shouldBe Failure(failure)
    registry.getBox(box.id) shouldBe None
    registry.allUnspentBoxes() shouldBe empty
    registry.fetchDigest() shouldBe WalletDigest.empty
  }

  for (keepSpent <- Seq(true, false)) {
    it should s"keep committed spending metadata after a failed block write with history $keepSpent" in withRegistry(keepSpent) { (store, registry) =>
      registry.updateScans(Set(appScan), box).get
      registry.getBox(box.id).get.spendingHeightOpt shouldBe None
      store.rejectWrites = true
      val spent = SpentInputData(bytesToId(versionId("spend")), tracked)
      registry.updateOnBlock(ScanResults(Seq.empty, Seq(spent), Seq.empty), bytesToId(versionId("block")), 2) shouldBe Failure(failure)
      val cached = registry.getBox(box.id).get
      cached.spendingHeightOpt shouldBe None
      cached.spendingTxIdOpt shouldBe None
      registry.unspentBoxes(appScan).map(_.spendingHeightOpt) shouldBe Seq(None)
    }
  }

  it should "leave outputs invisible when block digest validation fails" in withRegistry() { (_, registry) =>
    val spent = SpentInputData(bytesToId(versionId("spend")), tracked.copy(scans = Set(PaymentsScanId)))
    registry.updateOnBlock(ScanResults(Seq(tracked), Seq(spent), Seq.empty), bytesToId(versionId("block")), 1).isFailure shouldBe true
    registry.getBox(box.id) shouldBe None
    registry.fetchDigest() shouldBe WalletDigest.empty
  }

  it should "persist payment assets only when a failed scan write is retried successfully" in withRegistry() { (store, registry) =>
    registry.updateScans(Set(appScan), box).get
    store.rejectWrites = true
    registry.updateScans(Set(PaymentsScanId), box) shouldBe Failure(failure)
    registry.fetchDigest() shouldBe WalletDigest.empty
    registry.getBox(box.id).get.scans shouldBe Set(appScan)
    store.rejectWrites = false
    registry.updateScans(Set(PaymentsScanId), box) shouldBe Success(())
    registry.fetchDigest().walletBalance shouldBe box.value
    registry.getBox(box.id).get.scans shouldBe Set(PaymentsScanId)
    registry.unspentBoxes(appScan) shouldBe empty
    registry.walletUnspentBoxes().map(_.boxId) shouldBe Seq(tracked.boxId)
  }

  for (keepSpent <- Seq(true, false); createdInBlock <- Seq(true, false)) {
    it should s"reload committed block metadata with history $keepSpent and new output $createdInBlock" in withRegistry(keepSpent) { (_, registry) =>
      if (!createdInBlock) {
        registry.updateScans(Set(appScan), box).get
        registry.getBox(box.id).get.spendingHeightOpt shouldBe None
      }
      val spendingTx = bytesToId(versionId("spend"))
      val spent = SpentInputData(spendingTx, tracked)
      val outputs = if (createdInBlock) Seq(tracked) else Seq.empty
      registry.updateOnBlock(ScanResults(outputs, Seq(spent), Seq.empty), bytesToId(versionId("block")), 2) shouldBe Success(())
      if (keepSpent) {
        val updated = registry.getBox(box.id).get
        updated.scans shouldBe Set(appScan)
        updated.inclusionHeightOpt shouldBe Some(1)
        updated.spendingHeightOpt shouldBe Some(2)
        updated.spendingTxIdOpt shouldBe Some(spendingTx)
        registry.spentBoxes(appScan).map(_.spendingHeightOpt) shouldBe Seq(Some(2))
      } else {
        registry.getBox(box.id) shouldBe None
      }
      registry.unspentBoxes(appScan) shouldBe empty
    }
  }

  for (keepSpent <- Seq(true, false); changedField <- Seq("height", "scans")) {
    it should s"use current output $changedField for an overlapping stored spent box with history $keepSpent" in withRegistry(keepSpent) { (_, registry) =>
      registry.updateScans(Set(appScan), box).get
      val previous = registry.getBox(box.id).get
      val current = if (changedField == "height") previous.copy(inclusionHeightOpt = Some(2))
        else previous.copy(scans = Set(nextScan))
      val spendingTx = bytesToId(versionId("overlap-spend"))
      val spent = SpentInputData(spendingTx, previous)
      registry.updateOnBlock(ScanResults(Seq(current), Seq(spent), Seq.empty), bytesToId(versionId("overlap-block")), 2) shouldBe Success(())
      registry.unspentBoxes(appScan) shouldBe empty
      registry.unspentBoxes(nextScan) shouldBe empty
      if (keepSpent) {
        val updated = registry.getBox(box.id).get
        updated.inclusionHeightOpt shouldBe current.inclusionHeightOpt
        updated.scans shouldBe current.scans
        updated.spendingHeightOpt shouldBe Some(2)
        updated.spendingTxIdOpt shouldBe Some(spendingTx)
        val currentScan = current.scans.head
        registry.spentBoxes(currentScan).map(_.boxId) shouldBe Seq(previous.boxId)
        registry.boxesByInclusionHeight(currentScan, current.inclusionHeightOpt.get, current.inclusionHeightOpt.get)
          .map(_.inclusionHeightOpt) shouldBe Seq(current.inclusionHeightOpt)
        if (changedField == "height") registry.boxesByInclusionHeight(appScan, 1, 1) shouldBe empty
        else registry.confirmedBoxes(appScan) shouldBe empty
      } else {
        registry.getBox(box.id) shouldBe None
        registry.confirmedBoxes(appScan) shouldBe empty
        registry.confirmedBoxes(nextScan) shouldBe empty
        registry.boxesByInclusionHeight(appScan, 1, 2) shouldBe empty
        registry.boxesByInclusionHeight(nextScan, 1, 2) shouldBe empty
      }
    }
  }

  for (keepSpent <- Seq(true, false)) {
    it should s"commit maturity and output cleanup after a failed write with history $keepSpent" in withRegistry(keepSpent) { (store, registry) =>
      registry.updateScans(Set(appScan), box).get
      val previousOutput = registry.getBox(box.id).get
      val rewardBox = new ErgoBoxCandidate(2000000L, TrueTree, 1)
        .toBox(bytesToId(versionId("reward")), 0)
      registry.updateScans(Set(MiningScanId, PaymentsScanId), rewardBox).get
      val previousReward = registry.getBox(rewardBox.id).get
      val previousDigest = registry.fetchDigest()
      val maturedReward = previousReward.copy(scans = Set(PaymentsScanId))
      val currentOutput = previousOutput.copy(scans = Set(nextScan), inclusionHeightOpt = Some(2))
      val spendingTx = bytesToId(versionId("composition-spend"))
      val scan = ScanResults(Seq(maturedReward, currentOutput),
        Seq(SpentInputData(spendingTx, previousOutput)), Seq.empty)
      val blockId = bytesToId(versionId("composition-block"))

      store.rejectWrites = true
      registry.updateOnBlock(scan, blockId, 2, Seq(previousReward)) shouldBe Failure(failure)
      registry.getBox(rewardBox.id).get.scans shouldBe previousReward.scans
      registry.getBox(box.id).get.scans shouldBe previousOutput.scans
      registry.getBox(box.id).get.inclusionHeightOpt shouldBe previousOutput.inclusionHeightOpt
      registry.getBox(box.id).get.spendingHeightOpt shouldBe None
      registry.unspentBoxes(MiningScanId).map(_.boxId) shouldBe Seq(previousReward.boxId)
      registry.unspentBoxes(appScan).map(_.boxId) shouldBe Seq(previousOutput.boxId)
      registry.fetchDigest() shouldBe previousDigest

      store.rejectWrites = false
      registry.updateOnBlock(scan, blockId, 2, Seq(previousReward)) shouldBe Success(())
      registry.getBox(rewardBox.id).get.scans shouldBe Set(PaymentsScanId)
      registry.unspentBoxes(MiningScanId) shouldBe empty
      registry.boxesByInclusionHeight(MiningScanId, 1, 2) shouldBe empty
      registry.confirmedBoxes(appScan) shouldBe empty
      registry.boxesByInclusionHeight(appScan, 1, 2) shouldBe empty
      registry.unspentBoxes(nextScan) shouldBe empty
      registry.walletUnspentBoxes().map(_.boxId) shouldBe Seq(previousReward.boxId)
      registry.fetchDigest() shouldBe previousDigest.copy(height = 2)
      if (keepSpent) {
        val persisted = registry.getBox(box.id).get
        persisted.scans shouldBe currentOutput.scans
        persisted.inclusionHeightOpt shouldBe currentOutput.inclusionHeightOpt
        persisted.spendingHeightOpt shouldBe Some(2)
        persisted.spendingTxIdOpt shouldBe Some(spendingTx)
        registry.spentBoxes(nextScan).map(_.boxId) shouldBe Seq(previousOutput.boxId)
      } else {
        registry.getBox(box.id) shouldBe None
        registry.confirmedBoxes(nextScan) shouldBe empty
      }
    }
  }

  for (outcome <- Seq("success", "write failure", "validation failure")) {
    it should s"reply to AddBox with the registry $outcome" in withRegistry() { (store, registry) =>
      val fixture = new AkkaFixture
      implicit val system = fixture.system
      val state = ErgoWalletState(null, None, registry, OffChainRegistry.empty, None,
        WalletVars(None, Seq.empty), None, None, None, extendedParameters, 10, rescanInProgress = false)
      val service = new ErgoWalletServiceImpl(settings) {
        override def readWallet(state: ErgoWalletState, mnemonic: Option[SecretString],
                                keys: Option[Int], storage: SecretStorageSettings): ErgoWalletState = state
      }
      val actor = TestActorRef(new ErgoWalletActor(settings, extendedParameters, service, null, null) {
        override def preStart(): Unit = ()
      })
      try {
        val probe = TestProbe()
        actor ! ReadWallet(state)
        store.rejectWrites = outcome == "write failure"
        val scans = if (outcome == "validation failure") Set.empty[ScanId] else Set(appScan)
        actor.tell(AddBox(box, scans), probe.ref)
        val reply = probe.expectMsgType[AddBoxResponse].status
        outcome match {
          case "success" =>
            reply shouldBe Success(())
            registry.getBox(box.id).get.scans shouldBe scans
          case "write failure" => reply shouldBe Failure(failure)
          case _ =>
            reply.isFailure shouldBe true
            reply.failed.get.getMessage shouldBe "Can't remove a box which does not exist"
        }
      } finally {
        system.stop(actor)
        TestKit.shutdownActorSystem(system)
      }
    }
  }
}
