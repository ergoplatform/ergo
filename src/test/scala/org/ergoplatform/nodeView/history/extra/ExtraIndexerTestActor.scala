package org.ergoplatform.nodeView.history.extra

import akka.actor.ActorRef
import org.ergoplatform._
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.SortingOption
import org.ergoplatform.nodeView.state._
import org.ergoplatform.settings._
import org.ergoplatform.wallet.utils.FileUtils
import scorex.util.{ModifierId, bytesToId}

import java.io.File
import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Try}

class ExtraIndexerTestActor(test: ExtraIndexerSpecification) extends ExtraIndexerBase with FileUtils {

  override def receive: Receive = {
    case test.CreateDB(blockCount: Int) => createDB(blockCount)
    case test.ExtendDB(blockCount: Int) => extendDB(blockCount)
    case test.Reset() => reset()
    case test.GenerateBetterChainTip() => GenerateBetterChainTip()
    case test.CacheBlockTransactions(height, transactions) => cacheBlockTransactions(height, transactions)
    case test.DeferNextHeaderOnce(height) => deferNextHeaderOnce(height)
    case test.DeferBlockTransactionsOnce(height) => deferBlockTransactionsOnce(height)
    case test.Reload() => reload()
    case test.FailNextRollbackRemoval(probe) => failNextRollbackRemoval(probe)
    case test.PauseBufferedCatchUpAt(height, limit, probe) => pauseBufferedCatchUpAt(height, limit, probe)
  }

  override protected def loaded(state: IndexerState): Receive = ({
    case test.ForceRollback(height) =>
      beginRollback(state, fullChainHeaderAtHeight(height).get, resume = false)
    case test.GetLoadedState() => sender ! state
  }: Receive).orElse(super.loaded(state))

  override def caughtUpHook(height: Int = 0): Unit = {
    if(height > 0 && height < chainHeight) return
    test.lock.lock()
    test.done.signal()
    test.lock.unlock()
  }

  override def getLastTxForHeight(height: Int): ErgoTransaction = {
    val header = fullChainHeaderAtHeight(height).get
    val block = history.getFullBlock(header)
    block.get.transactions.last
  }

  type ID_LL = mutable.HashMap[ModifierId,(Long,Long)]

  private var configuredSaveLimit: Int = 1
  override protected def saveLimit: Int = configuredSaveLimit
  override protected implicit val segmentThreshold: Int = 8 // split to smaller segments
  override protected implicit val addressEncoder: ErgoAddressEncoder = test.initSettings.chainSettings.addressEncoder
  override protected val retryDelay = 50.millis

  val nodeSettings: NodeConfigurationSettings = NodeConfigurationSettings(StateType.Utxo, verifyTransactions = true,
    -1, UtxoSettings(utxoBootstrap = false, 0, 2), NipopowSettings(nipopowBootstrap = false, 1), mining = false,
    ChainGenerator.txCostLimit, ChainGenerator.txSizeLimit, blockCandidateGenerationInterval = 20.seconds, useExternalMiner = false,
    internalMinersCount = 1, internalMinerPollingInterval = 1.second, miningPubKeyHex = None, offlineGeneration = false,
    200, 5.minutes, 100000, 1.minute, mempoolSorting = SortingOption.FeePerByte, rebroadcastCount = 20,
    1000000, headerChainDiff = 5000, adProofsSuffixLength = 112 * 1024, extraIndex = false)

  private var dir: File = _
  private var stateOpt: Option[UtxoState] = None
  private var deferredHeaderHeightOpt: Option[Int] = None
  private var deferredTransactionsHeightOpt: Option[Int] = None
  private var rollbackFailureProbeOpt: Option[ActorRef] = None
  private var failRollbackRemoval: Boolean = false
  private var pauseCatchUpAtHeightOpt: Option[Int] = None
  private var catchUpPauseProbeOpt: Option[ActorRef] = None

  override protected def continueCatchUpAfterIndex(state: IndexerState): Boolean = {
    if (pauseCatchUpAtHeightOpt.contains(state.indexedHeight)) {
      pauseCatchUpAtHeightOpt = None
      catchUpPauseProbeOpt.foreach(_ ! state)
      catchUpPauseProbeOpt = None
      false
    } else true
  }

  override protected def removeRollbackIndexes(ids: Array[ModifierId]): Try[Unit] =
    if (failRollbackRemoval) {
      failRollbackRemoval = false
      Failure(new IllegalStateException("injected final rollback removal failure"))
    } else super.removeRollbackIndexes(ids)

  override protected def stopIndexer(): Unit = {
    rollbackFailureProbeOpt.foreach(_ ! "indexer-stop-requested")
    rollbackFailureProbeOpt = None
    super.stopIndexer()
  }

  override protected def fullChainHeaderAtHeight(height: Int): Option[Header] = {
    val headerOpt = super.fullChainHeaderAtHeight(height)
    if (deferredHeaderHeightOpt.contains(height)) {
      deferredHeaderHeightOpt = None
      headerOpt.map(_.copy(parentId = bytesToId(Array.fill(32)(0x7f.toByte))))
    } else {
      headerOpt
    }
  }

  override protected def blockTransactionsForHeader(header: Header): Option[BlockTransactions] = {
    if (deferredTransactionsHeightOpt.contains(header.height)) {
      deferredTransactionsHeightOpt = None
      None
    } else {
      super.blockTransactionsForHeader(header)
    }
  }

  def createDB(blockCount: Int): Unit = {
    if(stateOpt.isEmpty) {
      dir = createTempDir
      dir.mkdirs()

      val fullHistorySettings: ErgoSettings = ErgoSettings(dir.getAbsolutePath, NetworkType.TestNet, test.initSettings.chainSettings,
        nodeSettings, test.initSettings.scorexSettings, test.initSettings.walletSettings, test.initSettings.cacheSettings)

      _history = ErgoHistory.readOrGenerate(fullHistorySettings)(null)
    }

    stateOpt = Some(ChainGenerator.generate(blockCount, dir, _history, stateOpt))
    test._history = _history
    context.become(receive.orElse(loaded(IndexerState.fromHistory(_history))))
    test.lock.lock()
    test.created.signal()
    test.lock.unlock()
  }

  def extendDB(blockCount: Int): Unit = {
    stateOpt = Some(ChainGenerator.generate(blockCount, dir, _history, stateOpt))
    test._history = _history
    test.lock.lock()
    test.created.signal()
    test.lock.unlock()
  }

  def reset(): Unit = {
    resetTransientState()
    stateOpt = None
    test._history = null
    general.clear()
    boxes.clear()
    trees.clear()
    templates.clear()
    tokens.clear()
    segments.clear()
    deferredHeaderHeightOpt = None
    deferredTransactionsHeightOpt = None
    rollbackFailureProbeOpt = None
    failRollbackRemoval = false
    configuredSaveLimit = 1
    pauseCatchUpAtHeightOpt = None
    catchUpPauseProbeOpt = None
    context.become(receive.orElse(loaded(IndexerState(0, 0, 0, 0, caughtUp = false))))
  }

  def GenerateBetterChainTip(): Unit = {
    stateOpt = Some(ChainGenerator.generateBetter(_history, stateOpt.get))
    test._history = _history
    test.lock.lock()
    test.created.signal()
    test.lock.unlock()
  }

  private def cacheBlockTransactions(height: Int, transactions: BlockTransactions): Unit = {
    putBlockTransactionsInCache(height, transactions)
    test.lock.lock()
    test.created.signal()
    test.lock.unlock()
  }

  private def deferNextHeaderOnce(height: Int): Unit = {
    deferredHeaderHeightOpt = Some(height)
    test.lock.lock()
    test.created.signal()
    test.lock.unlock()
  }

  private def deferBlockTransactionsOnce(height: Int): Unit = {
    deferredTransactionsHeightOpt = Some(height)
    test.lock.lock()
    test.created.signal()
    test.lock.unlock()
  }

  private def reload(): Unit = {
    resetTransientState()
    context.become(receive.orElse(loaded(IndexerState.fromHistory(_history))))
    self ! ExtraIndexer.ReceivableMessages.Index()
    test.lock.lock()
    test.created.signal()
    test.lock.unlock()
  }

  private def failNextRollbackRemoval(probe: ActorRef): Unit = {
    rollbackFailureProbeOpt = Some(probe)
    failRollbackRemoval = true
    test.lock.lock()
    test.created.signal()
    test.lock.unlock()
  }

  private def pauseBufferedCatchUpAt(height: Int, limit: Int, probe: ActorRef): Unit = {
    configuredSaveLimit = limit
    pauseCatchUpAtHeightOpt = Some(height)
    catchUpPauseProbeOpt = Some(probe)
    probe ! "configured"
  }

}
