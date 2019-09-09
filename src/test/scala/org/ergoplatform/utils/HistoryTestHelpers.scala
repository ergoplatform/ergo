package org.ergoplatform.utils

import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.components.{EmptyBlockSectionComponent, ChainSyncController, ChainSyncComponent}
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings._
import org.scalacheck.Gen
import scorex.core.settings.ScorexSettings
import scorex.util.encode.Base16

import scala.concurrent.duration._

trait HistoryTestHelpers extends ErgoPropertyTest {

  override lazy val smallInt: Gen[Int] = Gen.choose(0, BlocksInChain)

  val BlocksInChain = 10
  val BlocksToKeep: Int = BlocksInChain + 1

  private val poPowSettings = PoPowSettings(prove = false, bootstrap = false,  3, PoPowParams(30, 30, 30, 0.45))

  def ensureMinimalHeight(history: ErgoHistory, height: Int = BlocksInChain): ErgoHistory = {
    val historyHeight = history.bestHeaderHeight
    if (historyHeight < height) {
      history match {
        case _: EmptyBlockSectionComponent =>
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
                      poPoWBootstrap: Boolean,
                      poPowProve: Boolean,
                      blocksToKeep: Int,
                      epochLength: Int = 100000000,
                      useLastEpochs: Int = 10,
                      initialDiffOpt: Option[BigInt] = None): ErgoHistory = {

    val miningDelay = 1.second
    val minimalSuffix = 2
    val nodeSettings: NodeConfigurationSettings = NodeConfigurationSettings(stateType, verifyTransactions, blocksToKeep,
      false, minimalSuffix, mining = false, Constants.DefaultComplexityLimit, miningDelay, useExternalMiner = false, miningPubKeyHex = None,
      offlineGeneration = false, 200, 100000, 100000, 1.minute, 1000000, poPowSettings.copy(bootstrap = poPoWBootstrap, prove = poPowProve))
    val scorexSettings: ScorexSettings = null
    val testingSettings: TestingSettings = null
    val walletSettings: WalletSettings = null
    val chainSettings = initialDiffOpt match {
      case Some(diff) =>
        val diffHex = Base16.encode(diff.toByteArray)
        settings.chainSettings.copy(epochLength = epochLength, useLastEpochs = useLastEpochs, initialDifficultyHex = diffHex)
      case _ =>
        settings.chainSettings.copy(epochLength = epochLength, useLastEpochs = useLastEpochs)
    }

    val dir = createTempDir
    val fullHistorySettings: ErgoSettings = ErgoSettings(dir.getAbsolutePath, NetworkType.TestNet, chainSettings, testingSettings,
      nodeSettings, scorexSettings, walletSettings, CacheSettings.default)

    ErgoHistory.readOrGenerate(fullHistorySettings, timeProvider)
  }

}

object HistoryTestHelpers {

  /**
    * Use reflection to set `minimalFullBlockHeightVar` to 0 to change regular synchronization rule, that we
    * first apply headers chain, and apply full blocks only after that
    */
  def allowToApplyOldBlocks(history: ErgoHistory): Unit = {
    import scala.reflect.runtime.{universe => ru}
    val runtimeMirror = ru.runtimeMirror(getClass.getClassLoader)
    val procInstance = runtimeMirror.reflect(history.asInstanceOf[ChainSyncComponent])
    val ppM = ru.typeOf[ChainSyncComponent].member(ru.TermName("pruningProcessor")).asMethod
    val pp = procInstance.reflectMethod(ppM).apply().asInstanceOf[ChainSyncController]
    val f = ru.typeOf[ChainSyncController].member(ru.TermName("minimalFullBlockHeightVar")).asTerm.accessed.asTerm
    runtimeMirror.reflect(pp).reflectField(f).set(ErgoHistory.GenesisHeight)
    val f2 = ru.typeOf[ChainSyncController].member(ru.TermName("isHeadersChainSyncedVar")).asTerm.accessed.asTerm
    runtimeMirror.reflect(pp).reflectField(f2).set(true)
  }

}
