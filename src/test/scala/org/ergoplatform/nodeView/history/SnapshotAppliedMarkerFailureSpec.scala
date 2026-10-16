package org.ergoplatform.nodeView.history

import org.ergoplatform.CriticalSystemException
import org.ergoplatform.nodeView.history.ErgoHistoryUtils.GenesisHeight
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.history.storage.modifierprocessors.FullBlockSectionProcessor
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.generators.ChainGenerator.genHeaderChain
import org.iq80.leveldb.Options
import scorex.db.{LDBFactory, LDBKVStore}

import java.io.IOException
import java.nio.file.Files
import scala.util.{Failure, Success, Try}

class SnapshotAppliedMarkerFailureSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoNodeTestConstants.settings

  property("snapshot completion must surface a failed retained-height marker write") {
    val root = Files.createTempDirectory("snapshot-applied-marker")
    val historyDir = Files.createDirectories(root.resolve("history"))
    val snapshotSettings = settings.copy(
      directory = root.toString,
      nodeSettings = settings.nodeSettings.copy(
        stateType = StateType.Utxo,
        utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true),
        extraIndex = false))
    val writeError = new IOException("injected snapshot marker write failure")
    var markerKey = Array.empty[Byte]
    var markerWriteAttempts = 0
    var failNextMarkerWrite = true

    val rawIndexDb = LDBFactory.factory.open(historyDir.resolve("index").toFile,
      new Options().createIfMissing(true))
    val indexStore = new LDBKVStore(rawIndexDb) {
      override def update(keys: Array[Array[Byte]], values: Array[Array[Byte]],
                          remove: Array[Array[Byte]]): Try[Unit] = {
        if (keys.length == 1 && java.util.Arrays.equals(keys.head, markerKey)) {
          markerWriteAttempts += 1
          if (failNextMarkerWrite) {
            failNextMarkerWrite = false
            Failure(writeError)
          } else super.update(keys, values, remove)
        } else super.update(keys, values, remove)
      }
    }
    val storage = new HistoryStorage(indexStore,
      LDBFactory.createKvDb(historyDir.resolve("objects").toString),
      LDBFactory.createKvDb(historyDir.resolve("extra").toString),
      snapshotSettings.cacheSettings)

    try {
      val history = new ErgoHistory with FullBlockSectionProcessor {
        override protected val settings = snapshotSettings
        override protected[history] val historyStorage: HistoryStorage = storage
        override val powScheme = snapshotSettings.chainSettings.powScheme
        markerKey = MinFullBlockHeightKey.data
      }
      val snapshotHeight = 20

      val failedCompletion = Try(history.onUtxoSnapshotApplied(snapshotHeight))
      val floorAfterFailure = history.minimalFullBlockHeight
      val appliedAfterFailure = history.isUtxoSnapshotApplied
      val successfulRetry = Try(history.onUtxoSnapshotApplied(snapshotHeight))

      markerWriteAttempts shouldBe 2
      floorAfterFailure shouldBe GenesisHeight
      appliedAfterFailure shouldBe false
      successfulRetry shouldBe Success(())
      history.minimalFullBlockHeight shouldBe snapshotHeight + 1
      history.isUtxoSnapshotApplied shouldBe true
      val propagated = failedCompletion.failed.get
      propagated.isInstanceOf[CriticalSystemException] shouldBe true
      propagated.getCause shouldBe writeError
    } finally storage.close()
  }

  property("ordinary pruning must not mark headers synced when the retained-height write fails") {
    val root = Files.createTempDirectory("pruning-applied-marker")
    val historyDir = Files.createDirectories(root.resolve("history"))
    val pruningSettings = settings.copy(
      directory = root.toString,
      nodeSettings = settings.nodeSettings.copy(
        stateType = StateType.Utxo,
        blocksToKeep = 1,
        utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = false),
        extraIndex = false))
    val writeError = new IOException("injected pruning marker write failure")
    var markerKey = Array.empty[Byte]
    var markerWriteAttempts = 0
    var failNextMarkerWrite = true

    val rawIndexDb = LDBFactory.factory.open(historyDir.resolve("index").toFile,
      new Options().createIfMissing(true))
    val indexStore = new LDBKVStore(rawIndexDb) {
      override def update(keys: Array[Array[Byte]], values: Array[Array[Byte]],
                          remove: Array[Array[Byte]]): Try[Unit] = {
        if (keys.length == 1 && java.util.Arrays.equals(keys.head, markerKey)) {
          markerWriteAttempts += 1
          if (failNextMarkerWrite) {
            failNextMarkerWrite = false
            Failure(writeError)
          } else super.update(keys, values, remove)
        } else super.update(keys, values, remove)
      }
    }
    val storage = new HistoryStorage(indexStore,
      LDBFactory.createKvDb(historyDir.resolve("objects").toString),
      LDBFactory.createKvDb(historyDir.resolve("extra").toString),
      pruningSettings.cacheSettings)

    try {
      val history = new ErgoHistory with FullBlockSectionProcessor {
        override protected val settings = pruningSettings
        override protected[history] val historyStorage: HistoryStorage = storage
        override val powScheme = pruningSettings.chainSettings.powScheme
        markerKey = MinFullBlockHeightKey.data
      }
      val header = genHeaderChain(3, history, diffBitsOpt = None, useRealTs = false).last
      header.height shouldBe 3

      val failedPruning = Try(history.updateBestFullBlock(header))
      markerWriteAttempts shouldBe 1
      val propagated = failedPruning.failed.get
      propagated.isInstanceOf[CriticalSystemException] shouldBe true
      propagated.getCause shouldBe writeError
      indexStore.get(markerKey) shouldBe None
      history.minimalFullBlockHeight shouldBe GenesisHeight
      history.isHeadersChainSynced shouldBe false

      Try(history.updateBestFullBlock(header)) shouldBe Success(3)
      markerWriteAttempts shouldBe 2
      history.minimalFullBlockHeight shouldBe 3
      history.isHeadersChainSynced shouldBe true
    } finally storage.close()
  }
}
