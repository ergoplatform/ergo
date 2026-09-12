package org.ergoplatform.nodeView.viewholder

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestProbe
import com.typesafe.config.{ConfigFactory, ConfigValueFactory}
import java.io.File
import java.nio.file.Files
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.ergoplatform.core.idToVersion
import org.ergoplatform.mining.DefaultFakePowScheme
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{InitStateFromSnapshot, UtxoSnapshotAppliedToState}
import org.ergoplatform.nodeView.ErgoNodeViewRef
import org.ergoplatform.nodeView.state.{BoxHolder, DigestState, ErgoState, StateType, UtxoState}
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.nodeView.wallet.persistence.{UtxoSnapshotWalletOrigin, WalletStorage}
import org.ergoplatform.serialization.ManifestSerializer
import org.ergoplatform.settings.{Algos, Constants, ErgoSettings, ErgoSettingsReader}
import org.ergoplatform.utils.{ErgoCorePropertyTest, NodeViewTestContext, NodeViewTestOps}
import org.ergoplatform.utils.ErgoCoreTestConstants.{emptyProverResult, parameters}
import org.ergoplatform.utils.generators.ValidBlocksGenerators.{validFullBlock, validTransactionsFromBoxHolder}
import org.ergoplatform.wallet.Constants.PaymentsScanId
import org.ergoplatform.wallet.utils.FileUtils
import scorex.db.{LDBFactory, StoreRegistry}
import scorex.util.ModifierId

import scala.concurrent.Await
import scala.concurrent.duration._

/** The node imports real snapshot bytes; the ordinary wallet discovers an owned payment from them. */
class WalletSnapshotProducerSpecification extends ErgoCorePropertyTest with NodeViewTestOps with FileUtils {

  private def parsedSettings(directory: File, mode: StateType): ErgoSettings = {
    Files.createDirectories(directory.toPath.resolve("wallet/keystore"))
    val config = ConfigFactory.parseString(
      s"""ergo.node.stateType = "${mode.stateTypeName}"
         |ergo.node.utxo.utxoBootstrap = true
         |ergo.node.nipopow.nipopowBootstrap = false
         |ergo.node.verifyTransactions = true
         |ergo.node.mining = false
         |ergo.node.extraIndex = false
         |ergo.chain.voting.votingLength = 20
         |ergo.wallet.testMnemonic = "ozone drill grab fiber curtain grace pudding thank cruise elder eight picnic"
         |ergo.wallet.testKeysQty = 5
         |""".stripMargin)
      .withValue("ergo.directory", ConfigValueFactory.fromAnyRef(directory.getAbsolutePath))
      .withValue("ergo.wallet.secretStorage.secretDir",
        ConfigValueFactory.fromAnyRef(new File(directory, "wallet/keystore").getAbsolutePath))
      .withFallback(ConfigFactory.load()).resolve()
    val parsed = ErgoSettingsReader.fromConfig(config)
    parsed.nodeSettings.stateType shouldBe mode
    parsed.copy(chainSettings = parsed.chainSettings.copy(
      powScheme = new DefaultFakePowScheme(parsed.chainSettings.powScheme.k, parsed.chainSettings.powScheme.n)))
  }

  private def closeOwnedStores(directory: File): Unit = {
    val root = directory.getCanonicalFile.toPath
    val registry = LDBFactory.factory.asInstanceOf[StoreRegistry]
    registry.lock.writeLock().lock()
    try {
      registry.map.toVector.collect {
        case (path, db) if path.getCanonicalFile.toPath.startsWith(root) => db
      }.foreach(_.close())
      registry.map.keys.exists(_.getCanonicalFile.toPath.startsWith(root)) shouldBe false
    } finally registry.lock.writeLock().unlock()
  }

  private class Session(override val settings: ErgoSettings) extends NodeViewTestContext {
    override val actorSystem: ActorSystem = ActorSystem()
    override val testProbe: TestProbe = TestProbe()(actorSystem)
    override val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(settings)(actorSystem)

    def stop(): Unit = {
      Await.result(actorSystem.terminate(), 30.seconds)
      closeOwnedStores(new File(settings.directory))
    }
  }

  private def awaitOwnedPayment(session: Session, payment: ErgoBox, height: Int): Unit = {
    val wallet = getCurrentView(session).vault
    session.testProbe.awaitAssert({
      val status = Await.result(wallet.getWalletStatus, 10.seconds)
      status.initialized shouldBe true
      status.height shouldBe height
      status.error shouldBe None
      val owned = Await.result(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = false), 10.seconds)
        .filter(box => java.util.Arrays.equals(box.trackedBox.box.id, payment.id))
      owned.size shouldBe 1
      owned.head.trackedBox.box shouldBe payment
      owned.head.trackedBox.scans should contain(PaymentsScanId)
      owned.head.trackedBox.spendingHeightOpt shouldBe None
    }, 30.seconds, 100.millis)
  }

  // Only read production storage after actor termination and actual handle closure.
  private def readCompletedOrigin(settings: ErgoSettings, anchor: ModifierId): UtxoSnapshotWalletOrigin = {
    val storage = WalletStorage.readOrCreate(settings)
    try {
      storage.readUtxoSnapshotScanStatusTry().get shouldBe None
      val origin = storage.readUtxoSnapshotWalletOriginTry().get.get
      origin.snapshotHeight shouldBe 19
      origin.snapshotBlockId shouldBe anchor
      origin
    } finally storage.close()
  }

  Seq(StateType.Utxo, StateType.Digest).foreach { mode =>
    property(s"real snapshot producer restores owned wallet payment across restarts in ${mode.stateTypeName} mode") {
      val root = Files.createTempDirectory("wallet-snapshot-producer-").toFile
      val nodeSettings = parsedSettings(new File(root, "node"), mode)
      val sourceSettings = parsedSettings(new File(root, "source"), StateType.Utxo)
      var session: Option[Session] = None
      try {
        val first = new Session(nodeSettings)
        session = Some(first)
        val wallet = getCurrentView(first).vault
        first.testProbe.awaitAssert({
          Await.result(wallet.publicKeys(0, Int.MaxValue), 10.seconds) should not be empty
        }, 20.seconds, 100.millis)
        val address = Await.result(wallet.publicKeys(0, Int.MaxValue), 10.seconds).head
        Await.result(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = false), 10.seconds) shouldBe empty

        val sourceDir = new File(sourceSettings.directory, "state")
        Files.createDirectories(sourceDir.toPath)
        val (genesis, boxes) = ErgoState.generateGenesisUtxoState(sourceDir, sourceSettings, Some(parameters))
        var source = WrappedUtxoState(genesis, boxes, sourceSettings)
        var parent: Option[ErgoFullBlock] = None
        val startTime = System.currentTimeMillis() - 20000L
        val preceding = (1 to 18).map { height =>
          val block = validFullBlock(parent, source, startTime + height * 1000L)
          source = source.applyModifier(block)(_ => ()).get
          parent = Some(block)
          block
        }
        val funding = source.versionedBoxHolder.boxes.values
          .filter(_.ergoTree == Constants.TrueTree).maxBy(_.value)
        val paymentTx = ErgoTransaction(
          IndexedSeq(Input(funding.id, emptyProverResult)), IndexedSeq.empty,
          IndexedSeq(new ErgoBoxCandidate(funding.value, address.script, 19, funding.additionalTokens)))
        val payment = paymentTx.outputs.head
        val withoutFunding = BoxHolder(source.versionedBoxHolder.boxes.values
          .filterNot(box => java.util.Arrays.equals(box.id, funding.id)).toSeq)
        val snapshotBlock = validFullBlock(parent, source,
          validTransactionsFromBoxHolder(withoutFunding)._1 :+ paymentTx, Some(startTime + 19000L))
        source = source.applyModifier(snapshotBlock)(_ => ()).get
        source.boxById(payment.id).get shouldBe payment
        val snapshotHeader = snapshotBlock.header
        snapshotHeader.height shouldBe 19
        val withoutPayment = BoxHolder(source.versionedBoxHolder.boxes.values
          .filterNot(box => java.util.Arrays.equals(box.id, payment.id)).toSeq)
        val successor = validFullBlock(Some(snapshotBlock), source,
          validTransactionsFromBoxHolder(withoutPayment)._1, Some(startTime + 20000L))
        successor.transactions.flatMap(_.inputs).exists(input =>
          java.util.Arrays.equals(input.boxId, payment.id)) shouldBe false
        successor.transactions.flatMap(_.outputs).exists(_.ergoTree == address.script) shouldBe false

        source.dumpSnapshot(snapshotHeader.height, source.rootDigest.dropRight(1)).get
        ManifestSerializer.MainnetManifestDepth shouldBe 14
        val manifestId = source.snapshotsDb.readSnapshotsInfo.availableManifests(snapshotHeader.height)
        val manifestBytes = source.snapshotsDb.readManifestBytes(manifestId).get
        val manifest = ManifestSerializer.defaultSerializer.parseBytes(manifestBytes)
        info(s"${mode.stateTypeName} snapshot subtree count: ${manifest.subtreesIds.size}")
        (preceding :+ snapshotBlock).foreach(block => applyHeader(block.header)(first).get)
        val history = getHistory(first)
        history.bestFullBlockOpt shouldBe None
        history.registerManifestToDownload(manifest, manifestBytes, snapshotHeader.height, Seq.empty)
        history.getChunkIdsToDownload(manifest.subtreesIds.size).foreach { id =>
          history.registerDownloadedChunk(id, source.snapshotsDb.readSubtreeBytes(id).get).get
        }
        val events = TestProbe()(first.actorSystem)
        first.actorSystem.eventStream.subscribe(events.ref, classOf[UtxoSnapshotAppliedToState]) shouldBe true
        send(InitStateFromSnapshot(snapshotHeader.height, snapshotHeader.id))(first)
        val applied = events.expectMsgType[UtxoSnapshotAppliedToState](20.seconds)
        applied.blockId shouldBe snapshotHeader.id
        applied.stateReader.version shouldBe idToVersion(snapshotHeader.id)
        applied.stateReader.getClass shouldBe (if (mode == StateType.Digest) classOf[DigestState] else classOf[UtxoState])
        Algos.encode(applied.stateReader.rootDigest) shouldBe Algos.encode(snapshotHeader.stateRoot)
        getHistory(first).isUtxoSnapshotApplied shouldBe true
        awaitOwnedPayment(first, payment, 19)
        // Successful finalization removes the retained scan source only after writing wallet origin.
        first.testProbe.awaitAssert({
          getHistory(first).readUtxoSnapshotScanSource(snapshotHeader.id).failed.get.getMessage shouldBe
            "No persisted UTXO snapshot scan source"
        }, 20.seconds, 100.millis)
        first.stop()
        session = None
        val origin = readCompletedOrigin(nodeSettings, snapshotHeader.id)

        val reopened = new Session(nodeSettings)
        session = Some(reopened)
        getHistory(reopened).bestFullBlockOpt shouldBe None
        awaitOwnedPayment(reopened, payment, 19)
        applyHeader(successor.header)(reopened).get
        applyPayload(successor)(reopened).get
        reopened.testProbe.awaitAssert({
          getCurrentState(reopened).version shouldBe idToVersion(successor.id)
        }, 20.seconds, 100.millis)
        awaitOwnedPayment(reopened, payment, 20)
        reopened.stop()
        session = None
        readCompletedOrigin(nodeSettings, snapshotHeader.id) shouldBe origin

        val afterSuccessorRestart = new Session(nodeSettings)
        session = Some(afterSuccessorRestart)
        getCurrentState(afterSuccessorRestart).version shouldBe idToVersion(successor.id)
        awaitOwnedPayment(afterSuccessorRestart, payment, 20)
        afterSuccessorRestart.stop()
        session = None
        readCompletedOrigin(nodeSettings, snapshotHeader.id) shouldBe origin
      } finally {
        try session.foreach(_.stop())
        finally {
          closeOwnedStores(root)
          deleteRecursive(root)
        }
      }
    }
  }
}
