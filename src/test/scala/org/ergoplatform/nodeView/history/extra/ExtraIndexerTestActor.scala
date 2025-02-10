package org.ergoplatform.nodeView.history.extra

import org.ergoplatform._
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.SortingOption
import org.ergoplatform.nodeView.state._
import org.ergoplatform.settings._
import org.ergoplatform.wallet.utils.FileUtils
import scorex.util.ModifierId

import java.io.File
import scala.collection.mutable
import scala.concurrent.duration.DurationInt

class ExtraIndexerTestActor(test: ExtraIndexerSpecification) extends ExtraIndexerBase with FileUtils {

  override def receive: Receive = {
    case test.CreateDB(blockCount: Int) => createDB(blockCount)
    case test.Reset() => reset()
  }

  override def caughtUpHook(height: Int = 0): Unit = {
    if(height > 0 && height < chainHeight) return
    test.lock.lock()
    test.done.signal()
    test.lock.unlock()
  }

  type ID_LL = mutable.HashMap[ModifierId,(Long,Long)]

  override protected val saveLimit: Int = 1 // save every block
  override protected implicit val segmentThreshold: Int = 8 // split to smaller segments
  override protected implicit val addressEncoder: ErgoAddressEncoder = test.initSettings.chainSettings.addressEncoder

  val nodeSettings: NodeConfigurationSettings = NodeConfigurationSettings(StateType.Utxo, verifyTransactions = true,
    -1, UtxoSettings(utxoBootstrap = false, 0, 2), NipopowSettings(nipopowBootstrap = false, 1), mining = false,
    ChainGenerator.txCostLimit, ChainGenerator.txSizeLimit, useExternalMiner = false, internalMinersCount = 1,
    internalMinerPollingInterval = 1.second, miningPubKeyHex = None, offlineGeneration = false,
    200, 5.minutes, 100000, 1.minute, mempoolSorting = SortingOption.FeePerByte, rebroadcastCount = 20,
    1000000, headerChainDiff = 5000, adProofsSuffixLength = 112 * 1024, extraIndex = false)

  private var dir: File = _
  private var stateOpt: Option[UtxoState] = None

  def createDB(blockCount: Int): Unit = {
    if(stateOpt.isEmpty) {
      dir = createTempDir

      val fullHistorySettings: ErgoSettings = ErgoSettings(dir.getAbsolutePath, NetworkType.TestNet, test.initSettings.chainSettings,
        nodeSettings, test.initSettings.scorexSettings, test.initSettings.walletSettings, test.initSettings.cacheSettings)

      _history = ErgoHistory.readOrGenerate(fullHistorySettings)(null)
    }

    stateOpt = Some(ChainGenerator.generate(blockCount, dir, _history, stateOpt))
    test._history = _history
    context.become(receive.orElse(loaded(IndexerState.fromHistory(_history))))
  }

  def reset(): Unit = {
    stateOpt = None
    test._history = null
    general.clear()
    boxes.clear()
    trees.clear()
    tokens.clear()
    segments.clear()
    context.become(receive.orElse(loaded(IndexerState(0, 0, 0, 0, caughtUp = false))))
  }

}
