package org.ergoplatform.nodeView.history

import akka.testkit.TestProbe
import org.ergoplatform.core.{idToVersion, versionToBytes}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.state.{ErgoState, ErgoStateContext, ErgoStateContextSerializer,
  ErgoStateReader, StateType, UtxoState}
import org.ergoplatform.settings.Parameters
import org.ergoplatform.utils.{ErgoCorePropertyTest, NodeViewTestConfig, NodeViewTestOps}
import org.ergoplatform.utils.fixtures.NodeViewFixture
import org.ergoplatform.utils.generators.ValidBlocksGenerators.{createUtxoState, validFullBlock}
import scorex.db.ByteArrayWrapper
import scorex.db.LDBVersionedStore
import scorex.util.{ModifierId, bytesToId}

import java.nio.file.Files
import scala.concurrent.duration._

class BestFullBlockStartupRecoverySpec extends ErgoCorePropertyTest with NodeViewTestOps {
  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters

  private def moveFixtureHistoryAside(fixture: NodeViewFixture): Unit = {
    val fixtureRoot = fixture.nodeViewDir.toPath.toRealPath()
    val historyDir = ErgoHistory.historyDir(fixture.settings).toPath.toRealPath()
    historyDir.getParent shouldBe fixtureRoot
    val withheldHistory = fixtureRoot.resolve("history-withheld")
    Files.notExists(withheldHistory) shouldBe true
    Files.move(historyDir, withheldHistory)
    Files.notExists(historyDir) shouldBe true
    Files.isDirectory(withheldHistory) shouldBe true
  }

  property("holder refuses a missing best full-block pointer without recreating persisted UTXO state") {
    val settings = NodeViewTestConfig(StateType.Utxo, verifyTransactions = true,
      popowBootstrap = false).toSettings

    new NodeViewFixture(settings, parameters).apply { fixture =>
      import fixture._

      val (sourceState, boxHolder) = createUtxoState(fixture.settings)
      try {
        val block = validFullBlock(None, sourceState, boxHolder)
        applyBlock(block).get
        org.ergoplatform.utils.untilTimeout(10.seconds, 50.millis) {
          getCurrentState.version shouldBe idToVersion(block.id)
        }
        getHistory.bestFullBlockIdOpt shouldBe Some(block.id)
        getHistory.bestFullBlockOpt.map(_.id) shouldBe Some(block.id)
        val savedVersion = getCurrentState.version
        val savedRoot = getCurrentState.rootDigest.toVector

        val oldHolder = nodeViewHolderRef
        val oldHolderWatcher = TestProbe()
        oldHolderWatcher.watch(oldHolder)
        actorSystem.stop(oldHolder)
        oldHolderWatcher.within(10.seconds) {
          oldHolderWatcher.expectTerminated(oldHolder)
        }

        val storage = HistoryStorage(fixture.settings)
        try {
          val bestFullBlockKey = ByteArrayWrapper(Array.fill[Byte](32)(-1))
          storage.getIndex(bestFullBlockKey).map(bytesToId) shouldBe Some(block.id)
          val sectionIds = Seq(block.header.id, block.header.transactionsId, block.header.extensionId)
          val sectionBytesBefore = sectionIds.map(id => storage.modifierTypeAndBytesById(id).map(_._2.toVector).get)

          storage.remove(Array(bestFullBlockKey), Array.empty[ModifierId]).get
          storage.getIndex(bestFullBlockKey) shouldBe None
          sectionIds.map(id => storage.modifierTypeAndBytesById(id).map(_._2.toVector).get) shouldBe sectionBytesBefore
        } finally storage.close()

        startNodeViewHolder()
        val watcher = TestProbe()
        watcher.watch(nodeViewHolderRef)
        watcher.within(10.seconds) {
          watcher.expectTerminated(nodeViewHolderRef)
        }

        val persistedState = UtxoState.create(ErgoState.stateDir(fixture.settings), fixture.settings)
        try {
          persistedState.version shouldBe savedVersion
          persistedState.rootDigest.toVector shouldBe savedRoot
        } finally persistedState.closeStorage()
      } finally sourceState.closeStorage()
    }
  }

  property("holder refuses a missing best full-block pointer when the persisted UTXO version key is also missing") {
    val settings = NodeViewTestConfig(StateType.Utxo, verifyTransactions = true,
      popowBootstrap = false).toSettings

    new NodeViewFixture(settings, parameters).apply { fixture =>
      import fixture._

      val (sourceState, boxHolder) = createUtxoState(fixture.settings)
      try {
        val block = validFullBlock(None, sourceState, boxHolder)
        applyBlock(block).get
        org.ergoplatform.utils.untilTimeout(10.seconds, 50.millis) {
          getCurrentState.version shouldBe idToVersion(block.id)
        }
        val savedRoot = getCurrentState.rootDigest.toVector
        savedRoot should not be fixture.settings.chainSettings.genesisStateDigest.toVector

        val oldHolder = nodeViewHolderRef
        val oldHolderWatcher = TestProbe()
        oldHolderWatcher.watch(oldHolder)
        actorSystem.stop(oldHolder)
        oldHolderWatcher.within(10.seconds) {
          oldHolderWatcher.expectTerminated(oldHolder)
        }

        val storage = HistoryStorage(fixture.settings)
        try {
          val bestFullBlockKey = ByteArrayWrapper(Array.fill[Byte](32)(-1))
          storage.getIndex(bestFullBlockKey).map(bytesToId) shouldBe Some(block.id)
          storage.remove(Array(bestFullBlockKey), Array.empty[ModifierId]).get
          storage.getIndex(bestFullBlockKey) shouldBe None
        } finally storage.close()

        val store = new LDBVersionedStore(ErgoState.stateDir(fixture.settings),
          initialKeepVersions = fixture.settings.nodeSettings.keepVersions)
        try {
          store.get(UtxoState.bestVersionKey).exists(_.sameElements(versionToBytes(idToVersion(block.id)))) shouldBe true
          store.update(savedRoot.toArray, Seq(UtxoState.bestVersionKey), Seq.empty).get
          store.get(UtxoState.bestVersionKey) shouldBe None
        } finally store.close()

        startNodeViewHolder()
        val watcher = TestProbe()
        watcher.watch(nodeViewHolderRef)
        watcher.within(10.seconds) {
          watcher.expectTerminated(nodeViewHolderRef)
        }

        val persistedState = UtxoState.create(ErgoState.stateDir(fixture.settings), fixture.settings)
        try {
          persistedState.version shouldBe ErgoState.genesisStateVersion
          persistedState.rootDigest.toVector shouldBe savedRoot
        } finally persistedState.closeStorage()
      } finally sourceState.closeStorage()
    }
  }

  property("holder refuses missing history beside an applied UTXO state without recreating that state") {
    val settings = NodeViewTestConfig(StateType.Utxo, verifyTransactions = true,
      popowBootstrap = false).toSettings

    new NodeViewFixture(settings, parameters).apply { fixture =>
      import fixture._

      val (sourceState, boxHolder) = createUtxoState(fixture.settings)
      try {
        val block = validFullBlock(None, sourceState, boxHolder)
        applyBlock(block).get
        org.ergoplatform.utils.untilTimeout(10.seconds, 50.millis) {
          getCurrentState.version shouldBe idToVersion(block.id)
        }
        getHistory.bestFullBlockIdOpt shouldBe Some(block.id)
        val savedVersion = getCurrentState.version
        val savedRoot = getCurrentState.rootDigest.toVector

        val oldHolder = nodeViewHolderRef
        val oldHolderWatcher = TestProbe()
        oldHolderWatcher.watch(oldHolder)
        actorSystem.stop(oldHolder)
        oldHolderWatcher.within(10.seconds) {
          oldHolderWatcher.expectTerminated(oldHolder)
        }
        moveFixtureHistoryAside(fixture)

        startNodeViewHolder()
        val watcher = TestProbe()
        watcher.watch(nodeViewHolderRef)
        watcher.within(10.seconds) {
          watcher.expectTerminated(nodeViewHolderRef)
        }

        val persistedState = UtxoState.create(ErgoState.stateDir(fixture.settings), fixture.settings)
        try {
          persistedState.version shouldBe savedVersion
          persistedState.rootDigest.toVector shouldBe savedRoot
        } finally persistedState.closeStorage()
      } finally sourceState.closeStorage()
    }
  }

  property("holder accepts missing history when the persisted UTXO state is still genesis") {
    val settings = NodeViewTestConfig(StateType.Utxo, verifyTransactions = true,
      popowBootstrap = false).toSettings

    new NodeViewFixture(settings, parameters).apply { fixture =>
      import fixture._

      getCurrentState.version shouldBe ErgoState.genesisStateVersion
      val genesisRoot = getCurrentState.rootDigest.toVector
      val oldHolder = nodeViewHolderRef
      val oldHolderWatcher = TestProbe()
      oldHolderWatcher.watch(oldHolder)
      actorSystem.stop(oldHolder)
      oldHolderWatcher.within(10.seconds) {
        oldHolderWatcher.expectTerminated(oldHolder)
      }
      moveFixtureHistoryAside(fixture)

      startNodeViewHolder()
      getCurrentState.version shouldBe ErgoState.genesisStateVersion
      getCurrentState.rootDigest.toVector shouldBe genesisRoot
    }
  }

  property("holder accepts missing history when the persisted Digest state is still genesis") {
    val settings = NodeViewTestConfig(StateType.Digest, verifyTransactions = false,
      popowBootstrap = false).toSettings

    new NodeViewFixture(settings, parameters).apply { fixture =>
      import fixture._

      getHistory.bestFullBlockOpt shouldBe None
      getCurrentState.version shouldBe ErgoState.genesisStateVersion
      val genesisRoot = getCurrentState.rootDigest.toVector
      val oldHolder = nodeViewHolderRef
      val oldHolderWatcher = TestProbe()
      oldHolderWatcher.watch(oldHolder)
      actorSystem.stop(oldHolder)
      oldHolderWatcher.within(10.seconds) {
        oldHolderWatcher.expectTerminated(oldHolder)
      }
      moveFixtureHistoryAside(fixture)

      startNodeViewHolder()
      getHistory.bestFullBlockOpt shouldBe None
      getCurrentState.version shouldBe ErgoState.genesisStateVersion
      getCurrentState.rootDigest.toVector shouldBe genesisRoot
    }
  }

  property("holder keeps canonical Digest genesis state when history has no full block") {
    val settings = NodeViewTestConfig(StateType.Digest, verifyTransactions = false,
      popowBootstrap = false).toSettings

    new NodeViewFixture(settings, parameters).apply { fixture =>
      import fixture._

      ErgoHistory.historyDir(fixture.settings).listFiles().nonEmpty shouldBe true
      getHistory.bestFullBlockOpt shouldBe None
      getCurrentState.version shouldBe ErgoState.genesisStateVersion
      val genesisRoot = getCurrentState.rootDigest.toVector

      val oldHolder = nodeViewHolderRef
      val oldHolderWatcher = TestProbe()
      oldHolderWatcher.watch(oldHolder)
      actorSystem.stop(oldHolder)
      oldHolderWatcher.within(10.seconds) {
        oldHolderWatcher.expectTerminated(oldHolder)
      }

      startNodeViewHolder()
      getCurrentState.version shouldBe ErgoState.genesisStateVersion
      getCurrentState.rootDigest.toVector shouldBe genesisRoot
    }
  }

  property("holder refuses a missing genesis context when history has no best full block") {
    val settings = NodeViewTestConfig(StateType.Utxo, verifyTransactions = true,
      popowBootstrap = false).toSettings

    new NodeViewFixture(settings, parameters).apply { fixture =>
      import fixture._

      getHistory.bestFullBlockOpt shouldBe None
      getCurrentState.version shouldBe ErgoState.genesisStateVersion
      val savedRoot = getCurrentState.rootDigest.toVector

      val oldHolder = nodeViewHolderRef
      val oldHolderWatcher = TestProbe()
      oldHolderWatcher.watch(oldHolder)
      actorSystem.stop(oldHolder)
      oldHolderWatcher.within(10.seconds) {
        oldHolderWatcher.expectTerminated(oldHolder)
      }

      val historyStore = HistoryStorage(fixture.settings)
      historyStore.close()
      ErgoHistory.historyDir(fixture.settings).listFiles().nonEmpty shouldBe true

      val store = new LDBVersionedStore(ErgoState.stateDir(fixture.settings),
        initialKeepVersions = fixture.settings.nodeSettings.keepVersions)
      try {
        store.get(ErgoStateReader.ContextKey).isDefined shouldBe true
        store.update(savedRoot.toArray, Seq(ErgoStateReader.ContextKey), Seq.empty).get
        store.get(ErgoStateReader.ContextKey) shouldBe None
      } finally store.close()

      startNodeViewHolder()
      val watcher = TestProbe()
      watcher.watch(nodeViewHolderRef)
      watcher.within(10.seconds) {
        watcher.expectTerminated(nodeViewHolderRef)
      }

      val persistedState = UtxoState.create(ErgoState.stateDir(fixture.settings), fixture.settings)
      try {
        persistedState.version shouldBe ErgoState.genesisStateVersion
        persistedState.rootDigest.toVector shouldBe savedRoot
      } finally persistedState.closeStorage()
    }
  }

  property("holder refuses missing genesis context beside an empty history without recreating UTXO state") {
    val settings = NodeViewTestConfig(StateType.Utxo, verifyTransactions = true,
      popowBootstrap = false).toSettings

    new NodeViewFixture(settings, parameters).apply { fixture =>
      import fixture._

      val savedVersion = getCurrentState.version
      savedVersion shouldBe ErgoState.genesisStateVersion
      val savedRoot = getCurrentState.rootDigest.toVector
      val genesisVersionBytes = versionToBytes(savedVersion)

      val oldHolder = nodeViewHolderRef
      val oldHolderWatcher = TestProbe()
      oldHolderWatcher.watch(oldHolder)
      actorSystem.stop(oldHolder)
      oldHolderWatcher.within(10.seconds) {
        oldHolderWatcher.expectTerminated(oldHolder)
      }
      moveFixtureHistoryAside(fixture)

      val store = new LDBVersionedStore(ErgoState.stateDir(fixture.settings),
        initialKeepVersions = fixture.settings.nodeSettings.keepVersions)
      try {
        store.get(ErgoStateReader.ContextKey).isDefined shouldBe true
        store.get(UtxoState.bestVersionKey).exists(_.sameElements(genesisVersionBytes)) shouldBe true
        store.get(genesisVersionBytes).exists(_.sameElements(savedRoot)) shouldBe true
        store.lastVersionID.exists(_.sameElements(savedRoot)) shouldBe true

        store.update(savedRoot.toArray, Seq(ErgoStateReader.ContextKey), Seq.empty).get
        store.get(ErgoStateReader.ContextKey) shouldBe None
        store.get(UtxoState.bestVersionKey).exists(_.sameElements(genesisVersionBytes)) shouldBe true
        store.get(genesisVersionBytes).exists(_.sameElements(savedRoot)) shouldBe true
        store.lastVersionID.exists(_.sameElements(savedRoot)) shouldBe true
      } finally store.close()

      startNodeViewHolder()
      val watcher = TestProbe()
      watcher.watch(nodeViewHolderRef)
      watcher.within(10.seconds) {
        watcher.expectTerminated(nodeViewHolderRef)
      }

      val persistedState = UtxoState.create(ErgoState.stateDir(fixture.settings), fixture.settings)
      try {
        persistedState.version shouldBe savedVersion
        persistedState.rootDigest.toVector shouldBe savedRoot
      } finally persistedState.closeStorage()
    }
  }

  property("holder retains UTXO state when empty history has altered genesis parameters") {
    val settings = NodeViewTestConfig(StateType.Utxo, verifyTransactions = true,
      popowBootstrap = false).toSettings

    new NodeViewFixture(settings, parameters).apply { fixture =>
      import fixture._

      getCurrentState.version shouldBe ErgoState.genesisStateVersion
      val savedRoot = getCurrentState.rootDigest.toVector
      val oldHolder = nodeViewHolderRef
      val oldHolderWatcher = TestProbe()
      oldHolderWatcher.watch(oldHolder)
      actorSystem.stop(oldHolder)
      oldHolderWatcher.within(10.seconds) {
        oldHolderWatcher.expectTerminated(oldHolder)
      }
      moveFixtureHistoryAside(fixture)

      val stateDirectory = ErgoState.stateDir(fixture.settings)
      val store = new LDBVersionedStore(stateDirectory,
        initialKeepVersions = fixture.settings.nodeSettings.keepVersions)
      val alteredBytes = try {
        val context = ErgoStateContextSerializer(fixture.settings.chainSettings)
          .parseBytesTry(store.get(ErgoStateReader.ContextKey).get).get
        val original = context.currentParameters
        val changed = Parameters(original.height, original.parametersTable.updated(
          Parameters.MaxBlockCostIncrease, original.maxBlockCost + 1), original.proposedUpdate)
        val altered = new ErgoStateContext(context.lastHeaders, context.lastExtensionOpt,
          context.genesisStateDigest, changed, context.validationSettings,
          context.votingData)(fixture.settings.chainSettings)
        altered.genesisStateDigest.toVector shouldBe savedRoot
        altered.lastHeaders shouldBe empty
        altered.lastExtensionOpt shouldBe None
        val bytes = altered.bytes
        ErgoStateContextSerializer(fixture.settings.chainSettings).parseBytesTry(bytes).isSuccess shouldBe true
        store.update(savedRoot.toArray, Seq.empty, Seq(ErgoStateReader.ContextKey -> bytes)).get
        bytes.toVector
      } finally store.close()
      val marker = Files.write(stateDirectory.toPath.resolve("preserved-state"), Array[Byte](42))

      startNodeViewHolder()
      val watcher = TestProbe()
      watcher.watch(nodeViewHolderRef)
      watcher.within(10.seconds) {
        watcher.expectTerminated(nodeViewHolderRef)
      }
      Files.readAllBytes(marker).toSeq shouldBe Seq[Byte](42)
      val reopened = new LDBVersionedStore(stateDirectory,
        initialKeepVersions = fixture.settings.nodeSettings.keepVersions)
      try {
        reopened.get(ErgoStateReader.ContextKey).map(_.toVector) shouldBe Some(alteredBytes)
        reopened.get(UtxoState.bestVersionKey).isDefined shouldBe true
      } finally reopened.close()
    }
  }
}
