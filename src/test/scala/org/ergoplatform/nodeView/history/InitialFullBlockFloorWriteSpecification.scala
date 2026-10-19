package org.ergoplatform.nodeView.history

import com.google.common.primitives.Ints
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.nodeView.history.ErgoHistoryUtils.GenesisHeight
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.history.storage.modifierprocessors.FullBlockSectionProcessor
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.HistoryTestHelpers.BlocksInChain
import org.ergoplatform.utils.generators.ChainGenerator.{applyHeaderChain, genHeaderChain, nextHeader}
import org.iq80.leveldb.Options
import scorex.db.{LDBFactory, LDBKVStore}
import scorex.util.ModifierId
import scorex.util.encode.Base16

import java.io.IOException
import java.nio.file.Files
import scala.util.{Failure, Try}

class InitialFullBlockFloorWriteSpecification extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoNodeTestConstants.initSettings

  private val floorKey = Base16.decode(
    "4987eb6a8fecbed88a6f733f456cdf4e334b944f4436be4cab50cacb442e15e6").get

  private case class RetryObservation(appendSucceeded: Boolean,
                                      headersSynced: Boolean,
                                      persistedFloor: Int,
                                      firstRequestedTransactionsId: Option[ModifierId])

  private case class FloorObservation(failedWrites: Int,
                                      appendSucceeded: Boolean,
                                      headersSynced: Boolean,
                                      persistedFloor: Int,
                                      firstRequestedTransactionsId: Option[ModifierId],
                                      expectedTransactionsId: ModifierId,
                                      retry: Option[RetryObservation])

  private def observeFirstFloorWrite(failFirstWrite: Boolean): FloorObservation = {
    val root = Files.createTempDirectory("initial-full-block-floor-write")
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
        nipopowSettings = initSettings.nodeSettings.nipopowSettings.copy(nipopowBootstrap = false),
        utxoSettings = initSettings.nodeSettings.utxoSettings.copy(utxoBootstrap = false)
      )
    )

    val indexPath = root.resolve("history").resolve("index")
    Files.createDirectories(indexPath)
    var failedWrites = 0
    val indexStore = new LDBKVStore(
      LDBFactory.factory.open(indexPath.toFile, new Options().createIfMissing(true))) {
      override def update(toInsertKeys: Array[Array[Byte]],
                          toInsertValues: Array[Array[Byte]],
                          toRemove: Array[Array[Byte]]): Try[Unit] = {
        val firstFloorWrite = toInsertKeys.zip(toInsertValues).exists { case (key, value) =>
          key.sameElements(floorKey) && Ints.fromByteArray(value) == 8
        }
        if (failFirstWrite && failedWrites == 0 && firstFloorWrite) {
          failedWrites += 1
          Failure(new IOException("injected initial full-block floor write failure"))
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

    try {
      val staleHeaders = genHeaderChain(BlocksInChain, history,
        diffBitsOpt = None, useRealTs = false)
      val afterStale = applyHeaderChain(history, staleHeaders)
      afterStale.isHeadersChainSynced shouldBe false
      afterStale.minimalFullBlockHeight shouldBe GenesisHeight

      val freshTip = nextHeader(
        prev = Some(staleHeaders.last),
        control = afterStale.difficultyCalculator,
        tsOpt = Some(System.currentTimeMillis()),
        diffBitsOpt = None,
        useRealTs = false
      )
      val appendResult = afterStale.append(freshTip)
      val observedHistory = appendResult.toOption.map(_._1).getOrElse(afterStale)
      val firstRequestedTransactionsId = observedHistory
        .nextModifiersToDownload(1, (_, _) => true)
        .get(BlockTransactions.modifierTypeId)
        .flatMap(_.headOption)
      val headersSyncedBeforeRetry = observedHistory.isHeadersChainSynced
      val persistedFloorBeforeRetry = observedHistory.minimalFullBlockHeight
      val retry = if (failFirstWrite) {
        val retryTip = nextHeader(
          prev = Some(freshTip),
          control = observedHistory.difficultyCalculator,
          tsOpt = Some(math.max(System.currentTimeMillis(), freshTip.timestamp + 1)),
          diffBitsOpt = None,
          useRealTs = false
        )
        val retryResult = observedHistory.append(retryTip)
        val retriedHistory = retryResult.toOption.map(_._1).getOrElse(observedHistory)
        Some(RetryObservation(
          retryResult.isSuccess,
          retriedHistory.isHeadersChainSynced,
          retriedHistory.minimalFullBlockHeight,
          retriedHistory.nextModifiersToDownload(1, (_, _) => true)
            .get(BlockTransactions.modifierTypeId).flatMap(_.headOption)
        ))
      } else None

      FloorObservation(
        failedWrites,
        appendResult.isSuccess,
        headersSyncedBeforeRetry,
        persistedFloorBeforeRetry,
        firstRequestedTransactionsId,
        staleHeaders.headers(7).transactionsId,
        retry
      )
    } finally history.closeStorage()
  }

  property("a failed first floor write cannot start full-block downloads from Genesis") {
    val control = observeFirstFloorWrite(failFirstWrite = false)
    control.failedWrites shouldBe 0
    control.appendSucceeded shouldBe true
    control.headersSynced shouldBe true
    control.persistedFloor shouldBe 8
    control.firstRequestedTransactionsId shouldBe Some(control.expectedTransactionsId)

    val fault = observeFirstFloorWrite(failFirstWrite = true)
    fault.failedWrites shouldBe 1
    fault.appendSucceeded shouldBe true
    fault.persistedFloor shouldBe GenesisHeight
    withClue(s"appendSucceeded=${fault.appendSucceeded}, synced=${fault.headersSynced}, " +
      s"persistedFloor=${fault.persistedFloor}, firstRequest=${fault.firstRequestedTransactionsId}: ") {
      fault.headersSynced shouldBe false
      fault.firstRequestedTransactionsId shouldBe None
    }
    val retry = fault.retry.get
    retry.appendSucceeded shouldBe true
    retry.headersSynced shouldBe true
    retry.persistedFloor shouldBe 8
    retry.firstRequestedTransactionsId shouldBe Some(fault.expectedTransactionsId)
  }
}
