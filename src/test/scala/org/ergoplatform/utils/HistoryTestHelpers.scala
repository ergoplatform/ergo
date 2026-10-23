package org.ergoplatform.utils

import org.ergoplatform.mining.difficulty.DifficultySerializer
import org.ergoplatform.modifiers.history.header.Header
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
                      genesisIdOpt: Option[ModifierId] = None,
                      utxoBootstrap: Boolean = false,
                      keepVersions: Int = 200,
                      votingLength: Option[Int] = None): ErgoHistory = {

    val txCostLimit     = initSettings.nodeSettings.maxTransactionCost
    val txSizeLimit     = initSettings.nodeSettings.maxTransactionSize
    val nodeSettings: NodeConfigurationSettings = NodeConfigurationSettings(stateType, verifyTransactions, blocksToKeep,
      UtxoSettings(utxoBootstrap, 0, 2), NipopowSettings(false, 1), mining = false, txCostLimit, txSizeLimit, blockCandidateGenerationInterval = 20.seconds,
      useExternalMiner = false, internalMinersCount = 1, internalMinerPollingInterval = 1.second, miningPubKeyHex = None,
      offlineGeneration = false, keepVersions, 5.minutes, 100000, 1.minute, mempoolSorting = SortingOption.FeePerByte,
      rebroadcastCount = 200, 1000000, 100, adProofsSuffixLength = 112*1024, extraIndex = false
)
    val scorexSettings: ScorexSettings = null
    val walletSettings: WalletSettings = null
    val defaultChainSettings = initialDiffOpt match {
      case Some(diff) =>
        val diffHex = Base16.encode(diff.toByteArray)
        settings.chainSettings.copy(epochLength = epochLength, useLastEpochs = useLastEpochs, initialDifficultyHex = diffHex, genesisId = genesisIdOpt)
      case _ =>
        settings.chainSettings.copy(epochLength = epochLength, useLastEpochs = useLastEpochs, genesisId = genesisIdOpt)
    }
    val chainSettings = votingLength.fold(defaultChainSettings) { length =>
      val voting = defaultChainSettings.voting.copy(votingLength = length)
      defaultChainSettings.copy(voting = voting)
    }

    val dir = createTempDir
    val fullHistorySettings: ErgoSettings = ErgoSettings(dir.getAbsolutePath, NetworkType.TestNet, chainSettings,
      nodeSettings, scorexSettings, walletSettings, settings.cacheSettings)

    ErgoHistory.readOrGenerate(fullHistorySettings)(null)
  }

  val RetargetingInitialDifficulty: BigInt = BigInt(1000000)

  /**
    * Headers-only history that recalculates difficulty every 4 headers over the last 2 epochs, so a few headers
    * apart in time move the difficulty
    */
  def retargetingHistory(): ErgoHistory =
    generateHistory(verifyTransactions = false, StateType.Digest, PoPoWBootstrap = false, blocksToKeep = 0,
      epochLength = 4, useLastEpochs = 2, initialDiffOpt = Some(RetargetingInitialDifficulty))

  /**
    * Appends `n` headers after `from` to `history`, each `dt` ms after its parent and carrying the difficulty
    * `history` requires after that parent, and returns them
    */
  def appendHeaders(history: ErgoHistory, from: Header, n: Int, dt: Long): Seq[Header] =
    (1 to n).foldLeft(Vector(from)) { (acc, _) =>
      val parent = acc.last
      val next = nextHeader(Some(parent), history.difficultyCalculator, tsOpt = Some(parent.timestamp + dt),
        diffBitsOpt = Some(DifficultySerializer.encodeCompactBits(history.requiredDifficultyAfter(parent))),
        useRealTs = false)
      history.append(next).get
      acc :+ next
    }.tail

  /**
    * Two retargeting histories holding the same genesis and `prefixLength` headers at the desired interval: the
    * node under test, and a scratch history to build a competing chain in. The last shared header is the fork point.
    */
  def retargetingForkPair(prefixLength: Int = 7): (ErgoHistory, ErgoHistory, Header) = {
    val node = retargetingHistory()
    val scratch = retargetingHistory()
    val genesis = nextHeader(None, node.difficultyCalculator, tsOpt = Some(1L),
      diffBitsOpt = Some(DifficultySerializer.encodeCompactBits(RetargetingInitialDifficulty)), useRealTs = false)
    node.append(genesis).get
    scratch.append(genesis).get
    val prefix = appendHeaders(node, genesis, prefixLength, node.difficultyCalculator.desiredInterval.toMillis)
    prefix.foreach(scratch.append(_).get)
    (node, scratch, prefix.last)
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
