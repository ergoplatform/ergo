package org.ergoplatform.nodeView.history

import com.google.common.primitives.Ints
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.history.storage.modifierprocessors.FullBlockSectionProcessor
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.iq80.leveldb.Options
import scorex.db.{LDBFactory, LDBKVStore}
import scorex.util.encode.Base16

import java.io.IOException
import java.nio.file.Files
import scala.util.{Failure, Try}

class HistoryPruningFloorWriteBoundarySpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoNodeTestConstants.initSettings
  import org.ergoplatform.utils.generators.ChainGenerator.{applyChain, genChain}

  property("a failed floor write cannot advance cleanup past the persisted floor") {
    val root = Files.createTempDirectory("history-pruning-floor-write-failure")
    val testSettings = initSettings.copy(
      directory = root.toString,
      chainSettings = initSettings.chainSettings.copy(
        voting = initSettings.chainSettings.voting.copy(votingLength = 4)
      ),
      nodeSettings = initSettings.nodeSettings.copy(
        stateType = StateType.Utxo,
        verifyTransactions = true,
        blocksToKeep = 2,
        extraIndex = false,
        utxoSettings = initSettings.nodeSettings.utxoSettings.copy(utxoBootstrap = false)
      )
    )

    val indexPath = root.resolve("history").resolve("index")
    Files.createDirectories(indexPath)
    val floorKey = Base16.decode("4987eb6a8fecbed88a6f733f456cdf4e334b944f4436be4cab50cacb442e15e6").get
    var failNextFloorWrite = false
    var failedFloorWrites = 0
    val indexStore = new LDBKVStore(
      LDBFactory.factory.open(indexPath.toFile, new Options().createIfMissing(true))) {
      override def update(toInsertKeys: Array[Array[Byte]], toInsertValues: Array[Array[Byte]],
                          toRemove: Array[Array[Byte]]): Try[Unit] = {
        val advancingFloor = toInsertKeys.zip(toInsertValues).exists { case (key, value) =>
          key.sameElements(floorKey) && Ints.fromByteArray(value) == 8
        }
        if (failNextFloorWrite && advancingFloor) {
          failNextFloorWrite = false
          failedFloorWrites += 1
          Failure(new IOException("injected retention-floor write failure"))
        } else super.update(toInsertKeys, toInsertValues, toRemove)
      }
    }
    val storage = new HistoryStorage(
      indexStore,
      LDBFactory.createKvDb(root.resolve("history").resolve("objects").toString),
      LDBFactory.createKvDb(root.resolve("history").resolve("extra").toString),
      testSettings.cacheSettings
    )
    val history = new ErgoHistory with FullBlockSectionProcessor {
      override protected val settings: ErgoSettings = testSettings
      override protected[history] val historyStorage: HistoryStorage = storage
      override val powScheme = testSettings.chainSettings.powScheme
    }

    val blocks = try {
      history.writeMinimalFullBlockHeight(1)
      history.isHeadersChainSyncedVar = true
      val generated = genChain(10, history)
      generated.map(_.blockTransactions.id).distinct.size shouldBe 10
      applyChain(history, generated.take(8))
      history.minimalFullBlockHeight shouldBe 4
      history.readPrunedHeight() shouldBe 4
      history.contains(generated(6).blockTransactions.id) shouldBe true

      history.append(generated(8).header).get
      history.append(generated(8).blockTransactions).get
      failNextFloorWrite = true
      history.append(generated(8).extension).get
      failedFloorWrites shouldBe 1
      generated
    } finally history.closeStorage()

    val reopened = ErgoHistory.readOrGenerate(testSettings)(null)
    try {
      reopened.bestFullBlockIdOpt shouldBe Some(blocks(8).id)
      reopened.minimalFullBlockHeight shouldBe 4
      reopened.contains(blocks(6).blockTransactions.id) shouldBe true
      reopened.contains(blocks(6).extension.id) shouldBe true
      reopened.readPrunedHeight() shouldBe 4
      reopened.isHeadersChainSyncedVar = true
      applyChain(reopened, blocks.drop(9))
    } finally reopened.closeStorage()

    val recovered = ErgoHistory.readOrGenerate(testSettings)(null)
    try {
      recovered.bestFullBlockIdOpt shouldBe Some(blocks.last.id)
      recovered.minimalFullBlockHeight shouldBe 8
      recovered.readPrunedHeight() shouldBe 8
      blocks.slice(3, 7).foreach { block =>
        recovered.contains(block.blockTransactions.id) shouldBe false
        recovered.contains(block.extension.id) shouldBe false
      }
      blocks.drop(7).foreach { block =>
        recovered.contains(block.blockTransactions.id) shouldBe true
        recovered.contains(block.extension.id) shouldBe true
      }
    } finally recovered.closeStorage()
  }
}
