package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.extra.ExtraIndexer._
import org.ergoplatform.nodeView.history.extra.IndexedErgoAddressSerializer.hashErgoTree
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.wallet.utils.TestFileUtils
import scorex.util.bytesToId
import scorex.db.LDBFactory
import scorex.util.ModifierId

import java.nio.ByteBuffer
import scala.util.{Failure, Try}

class GenesisIndexSpec extends ErgoCorePropertyTest with TestFileUtils {
  import org.ergoplatform.utils.ErgoNodeTestConstants.{settings => baseSettings}

  private def freshSettings(rent: Boolean): ErgoSettings = baseSettings.copy(
    directory = createTempDir.getAbsolutePath,
    nodeSettings = baseSettings.nodeSettings.copy(extraIndex = true, storageRentCollection = rent))

  private val probeKey = Algos.hash("genesis checkpoint preservation probe")

  property("a failed atomic bootstrap leaves no checkpoint and retries without duplicate balances") {
    val settings = freshSettings(rent = true)
    val empty = ErgoHistory.readOrGenerate(settings.copy(nodeSettings = settings.nodeSettings.copy(extraIndex = false)))(null)
    empty.closeStorage()
    var failWrite = true
    val storage = new HistoryStorage(
      LDBFactory.createKvDb(s"${settings.directory}/history/index"),
      LDBFactory.createKvDb(s"${settings.directory}/history/objects"),
      LDBFactory.createKvDb(s"${settings.directory}/history/extra"), settings.cacheSettings) {
      override def insertExtraTry(entries: Array[(Array[Byte], Array[Byte])], objects: Array[ExtraIndex],
                                  removals: Array[ModifierId]): Try[Unit] =
        if (failWrite) Failure(new IllegalStateException("injected bootstrap write failure"))
        else super.insertExtraTry(entries, objects, removals)
    }
    try {
      GenesisIndex.initialize(storage, settings.chainSettings, rentEnabled = true).isFailure shouldBe true
      Seq(SchemaVersionKey, IndexedHeightKey, GlobalBoxIndexKey, GlobalTxIndexKey).foreach { key =>
        storage.modifierBytesById(bytesToId(key)) shouldBe None
      }
      GenesisIndex.boxes(settings.chainSettings).foreach(box => storage.getExtraIndex(box.id) shouldBe None)
      failWrite = false
      GenesisIndex.initialize(storage, settings.chainSettings, rentEnabled = true).get
      GenesisIndex.identitiesMatch(storage, settings.chainSettings) shouldBe true
      GenesisIndex.initialRowsMatch(storage, settings.chainSettings, rentEnabled = true) shouldBe true
      // A retry constructs the same seed instead of adding to balances in the previous batch.
      GenesisIndex.initialize(storage, settings.chainSettings, rentEnabled = true).get
      GenesisIndex.initialRowsMatch(storage, settings.chainSettings, rentEnabled = true) shouldBe true
    } finally storage.close()
    val reopened = ErgoHistory.readOrGenerate(settings)(null)
    try IndexerState.fromHistory(reopened).globalBoxIndex shouldBe 4L
    finally reopened.closeStorage()
  }

  Seq(false, true).foreach { rent =>
    property(s"configured genesis checkpoint reopens with rent collection $rent and no invented transaction") {
      val settings = freshSettings(rent)
      val expected = ErgoState.genesisBoxes(settings.chainSettings)
      val history = ErgoHistory.readOrGenerate(settings)(null)
      try {
        val checkpoint = IndexerState.fromHistory(history)
        checkpoint.indexedHeight shouldBe 0
        checkpoint.globalTxIndex shouldBe 0L
        checkpoint.globalBoxIndex shouldBe expected.size + 1L
        history.historyStorage.getExtraIndex(bytesToId(NumericTxIndex.indexToBytes(0L))) shouldBe None
        NumericBoxIndex.getBoxByNumber(history, 0L) shouldBe None
        NumericBoxIndex.getBoxByNumber(history, Long.MinValue) shouldBe None
        expected.zipWithIndex.foreach { case (box, i) =>
          NumericBoxIndex.getBoxByNumber(history, i + 1L).get.id shouldBe bytesToId(box.id)
          val address = history.typedExtraIndexById[IndexedErgoAddress](hashErgoTree(box.ergoTree)).get
          address.txs shouldBe empty
          address.balanceInfo.get.nanoErgs shouldBe expected.filter(_.ergoTree == box.ergoTree).map(_.value).sum
        }
        history.storageRentBoxesAtOrBefore(Int.MaxValue, Int.MaxValue).map(_.globalIndex).sorted.toSeq shouldBe
          (if (rent) expected.indices.map(_ + 1L) else Seq.empty)
        history.historyStorage.insertExtraTry(Array(probeKey -> Array[Byte](1)), Array.empty).get
      } finally history.closeStorage()
      val reopened = ErgoHistory.readOrGenerate(settings)(null)
      try {
        reopened.historyStorage.modifierBytesById(bytesToId(probeKey)) shouldBe defined
        IndexerState.fromHistory(reopened).globalBoxIndex shouldBe expected.size + 1L
      } finally reopened.closeStorage()
    }
  }

  private def rebuildAfter(change: (HistoryStorage, ErgoSettings) => Unit): Unit = {
    val settings = freshSettings(rent = true)
    val history = ErgoHistory.readOrGenerate(settings)(null)
    try {
      history.historyStorage.insertExtraTry(Array(probeKey -> Array[Byte](1)), Array.empty).get
      change(history.historyStorage, settings)
    } finally history.closeStorage()
    val reopened = ErgoHistory.readOrGenerate(settings)(null)
    try {
      reopened.historyStorage.modifierBytesById(bytesToId(probeKey)) shouldBe None
      IndexerState.fromHistory(reopened).globalBoxIndex shouldBe 4L
      GenesisIndex.identitiesMatch(reopened.historyStorage, settings.chainSettings) shouldBe true
      GenesisIndex.initialRowsMatch(reopened.historyStorage, settings.chainSettings, rentEnabled = true) shouldBe true
    } finally reopened.closeStorage()
  }

  Seq(6, 7).foreach { schema =>
    property(s"legacy schema $schema rebuilds instead of reusing numeric references") {
      rebuildAfter { (storage, _) =>
        storage.insertExtraTry(Array(SchemaVersionKey -> ByteBuffer.allocate(4).putInt(schema).array), Array.empty).get
      }
    }
  }

  property("a same-version checkpoint missing all genesis boxes rebuilds") {
    rebuildAfter { (storage, settings) =>
      storage.removeExtraTry(ErgoState.genesisBoxes(settings.chainSettings).map(box => bytesToId(box.id)).toArray).get
    }
  }

  property("a single wrong genesis numeric mapping invalidates the checkpoint") {
    rebuildAfter { (storage, settings) =>
      val genesis = ErgoState.genesisBoxes(settings.chainSettings)
      storage.insertExtraTry(Array.empty, Array[ExtraIndex](NumericBoxIndex(1L, bytesToId(genesis.last.id)))).get
    }
  }

  property("a missing zero-valued genesis counter invalidates the checkpoint") {
    rebuildAfter { (storage, _) =>
      storage.removeExtraTry(Array(bytesToId(GlobalTxIndexKey))).get
    }
  }

  property("a single missing genesis box invalidates the checkpoint") {
    rebuildAfter { (storage, settings) =>
      storage.removeExtraTry(Array(bytesToId(ErgoState.genesisBoxes(settings.chainSettings).head.id))).get
    }
  }

  property("a reserved zero numeric row invalidates the checkpoint") {
    rebuildAfter { (storage, settings) =>
      storage.insertExtraTry(Array.empty, Array[ExtraIndex](
        NumericBoxIndex(0L, bytesToId(ErgoState.genesisBoxes(settings.chainSettings).head.id)))).get
    }
  }

  property("a genesis balance change at height zero invalidates the checkpoint") {
    rebuildAfter { (storage, settings) =>
      val address = hashErgoTree(ErgoState.genesisBoxes(settings.chainSettings).head.ergoTree)
      storage.insertExtraTry(Array.empty, Array[ExtraIndex](IndexedErgoAddress(address).initBalance)).get
    }
  }

  property("a missing genesis rent row at height zero invalidates the checkpoint") {
    rebuildAfter { (storage, settings) =>
      storage.removeExtraTry(Array(StorageRentBox(GenesisIndex.boxes(settings.chainSettings).head).id)).get
    }
  }
}
