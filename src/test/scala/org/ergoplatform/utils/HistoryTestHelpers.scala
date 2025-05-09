package org.ergoplatform.utils

import org.ergoplatform.nodeView.history.ErgoHistoryUtils._
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.storage.modifierprocessors.{EmptyBlockSectionProcessor, FullBlockPruningProcessor, ToDownloadProcessor}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.SortingOption
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.{ScorexSettings, _}
import org.ergoplatform.wallet.utils.FileUtils
import org.scalacheck.Gen
import scorex.util.ModifierId
import scorex.util.encode.Base16

import scala.concurrent.duration._

object HistoryTestHelpers extends FileUtils {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ChainGenerator._

  lazy val smallInt: Gen[Int] = Gen.choose(0, BlocksInChain)

  val BlocksInChain = 10
  val BlocksToKeep: Int = BlocksInChain + 1

  def ensureMinimalHeight(history: ErgoHistory, height: Int = BlocksInChain): ErgoHistory = {
    val historyHeight = history.headersHeight
    if (historyHeight < height) {
      history match {
        case _: EmptyBlockSectionProcessor =>
          val chain = genHeaderChain(height - historyHeight, history, diffBitsOpt = None, useRealTs = false)
          if (history.isEmpty) applyHeaderChain(history, chain) else applyHeaderChain(history, chain.tail)
        case _ =>
          ???
      }
    } else {
      history
    }
  }

  // todo looks like copy-paste from Stubs.generateHistory
  def generateHistory(verifyTransactions: Boolean,
                      stateType: StateType,
                      PoPoWBootstrap: Boolean,
                      blocksToKeep: Int,
                      epochLength: Int = 100000000,
                      useLastEpochs: Int = 10,
                      initialDiffOpt: Option[BigInt] = None,
                      genesisIdOpt: Option[ModifierId] = None): ErgoHistory = {

    val txCostLimit     = initSettings.nodeSettings.maxTransactionCost
    val txSizeLimit     = initSettings.nodeSettings.maxTransactionSize
    val nodeSettings: NodeConfigurationSettings = NodeConfigurationSettings(stateType, verifyTransactions, blocksToKeep,
      UtxoSettings(false, 0, 2), NipopowSettings(false, 1), mining = false, txCostLimit, txSizeLimit, blockCandidateGenerationInterval = 20.seconds,
      useExternalMiner = false, internalMinersCount = 1, internalMinerPollingInterval = 1.second, miningPubKeyHex = None,
      offlineGeneration = false, 200, 5.minutes, 100000, 1.minute, mempoolSorting = SortingOption.FeePerByte,
      rebroadcastCount = 200, 1000000, 100, adProofsSuffixLength = 112*1024, extraIndex = false
)
    val scorexSettings: ScorexSettings = null
    val walletSettings: WalletSettings = null
    val chainSettings = initialDiffOpt match {
      case Some(diff) =>
        val diffHex = Base16.encode(diff.toByteArray)
        settings.chainSettings.copy(epochLength = epochLength, useLastEpochs = useLastEpochs, initialDifficultyHex = diffHex, genesisId = genesisIdOpt)
      case _ =>
        settings.chainSettings.copy(epochLength = epochLength, useLastEpochs = useLastEpochs, genesisId = genesisIdOpt)
    }

    val dir = createTempDir
    val fullHistorySettings: ErgoSettings = ErgoSettings(dir.getAbsolutePath, NetworkType.TestNet, chainSettings,
      nodeSettings, scorexSettings, walletSettings, settings.cacheSettings)

    ErgoHistory.readOrGenerate(fullHistorySettings)(null)
  }

  /**
    * Use reflection to set `minimalFullBlockHeightVar` to 0 to change regular synchronization rule, that we
    * first apply headers chain, and apply full blocks only after that
    */
  def allowToApplyOldBlocks(history: ErgoHistory): Unit = {
    import scala.reflect.runtime.{universe => ru}
    val runtimeMirror = ru.runtimeMirror(getClass.getClassLoader)
    val procInstance = runtimeMirror.reflect(history.asInstanceOf[ToDownloadProcessor])
    val ppM = ru.typeOf[ToDownloadProcessor].member(ru.TermName("pruningProcessor")).asMethod
    val pp = procInstance.reflectMethod(ppM).apply().asInstanceOf[FullBlockPruningProcessor]
    val f = ru.typeOf[FullBlockPruningProcessor].member(ru.TermName("minimalFullBlockHeightVar")).asTerm.accessed.asTerm
    runtimeMirror.reflect(pp).reflectField(f).set(GenesisHeight)
    val f2 = ru.typeOf[FullBlockPruningProcessor].member(ru.TermName("isHeadersChainSyncedVar")).asTerm.accessed.asTerm
    runtimeMirror.reflect(pp).reflectField(f2).set(true)
  }

}
