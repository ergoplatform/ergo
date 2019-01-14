package org.ergoplatform.tools

import java.io.File

import org.ergoplatform.local.ErgoMiner
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.mining.{AutolykosPowScheme, CandidateBlock}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{Extension, ExtensionCandidate, Header}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.storage.modifierprocessors.{FullBlockPruningProcessor, ToDownloadProcessor}
import org.ergoplatform.nodeView.state._
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.nodeView.wallet.ErgoProvingInterpreter
import org.ergoplatform.settings._
import org.ergoplatform.utils.ErgoTestHelpers
import scapi.sigma.DLogProtocol.ProveDlog

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.util.Try

/**
  * Application object for chain generation.
  * Takes 2 parameters: start timestamp and path to history folder.
  * Generate blocks starting from start timestamp and until current time with expected block interval
  * between them, to ensure that difficulty does not change.
  */
object ChainGenerator extends TestKit(ActorSystem()) with App with ErgoTestHelpers {

  val EmissionTxCost: Long = 20000

  val prover = new ErgoProvingInterpreter("seed", 1)

  val pow = new AutolykosPowScheme(powScheme.k, powScheme.n)
  val blockInterval = 2.minute

  val startTime = args.headOption.map(_.toLong).getOrElse(timeProvider.time - (blockInterval * 10).toMillis)
  val dir = if (args.length < 2) new File("/tmp/ergo/node1/data") else new File(args(1))
  val txsSize: Int = if (args.length < 3) 100 * 1024 else args(2).toInt

  val miningDelay = 1.second
  val minimalSuffix = 2
  val nodeSettings: NodeConfigurationSettings = NodeConfigurationSettings(StateType.Utxo, verifyTransactions = true,
    -1, PoPoWBootstrap = false, minimalSuffix, mining = false, miningDelay, offlineGeneration = false, 200)
  val monetarySettings = settings.chainSettings.monetary.copy(minerRewardDelay = 720)
  val chainSettings = ChainSettings(0: Byte, 0: Byte, blockInterval, 256, 8, votingSettings, pow, monetarySettings)
  val fullHistorySettings: ErgoSettings = ErgoSettings(dir.getAbsolutePath, chainSettings, settings.testingSettings,
    nodeSettings, settings.scorexSettings, settings.walletSettings, CacheSettings.default)
  val stateDir = ErgoState.stateDir(fullHistorySettings)
  stateDir.mkdirs()

  val history = ErgoHistory.readOrGenerate(fullHistorySettings, timeProvider)
  allowToApplyOldBlocks(history)
  val (state, boxHolder) = ErgoState.generateGenesisUtxoState(stateDir, StateConstants(None, fullHistorySettings))
  log.error(s"Going to generate a chain at ${dir.getAbsoluteFile} starting from ${history.bestFullBlockOpt}")

  val chain = loop(state, boxHolder, None, Seq())
  log.info(s"Chain of length ${chain.length} generated")
  history.bestHeaderOpt shouldBe history.bestFullBlockOpt.map(_.header)
  history.bestFullBlockOpt.get shouldBe chain.last
  log.info("History was generated successfully")
  System.exit(0)

  private def loop(state: UtxoState, boxHolder: BoxHolder,
                   last: Option[Header], acc: Seq[ErgoFullBlock]): Seq[ErgoFullBlock] = {
    val time: Long = last.map(_.timestamp + blockInterval.toMillis).getOrElse(startTime)
    if (time < timeProvider.time) {
      val wus = WrappedUtxoState(state, boxHolder, stateConstants)
      val txs = validTransactionsFromUtxoState(wus)

      val usedBoxes = txs.flatMap(_.inputs)

      val newBoxHolder = BoxHolder(txs.flatMap(_.outputs) ++ boxHolder.boxes.values.filterNot(usedBoxes.contains))

      val candidate = genCandidate(prover.dlogPubkeys.head, last, time, txs, state)

      val block = proveCandidate(candidate.get)
      history.append(block.header).get
      block.blockSections.foreach(s => if (!history.contains(s)) history.append(s).get)
      log.info(s"Block ${block.id} at height ${block.header.height} generated")
      loop(state.applyModifier(block).get, newBoxHolder, Some(block.header), acc :+ block)
    } else {
      acc
    }
  }

  private def genCandidate(minerPk: ProveDlog,
                           lastHeaderOpt: Option[Header],
                           ts: Long,
                           txsFromPool: Seq[ErgoTransaction],
                           state: UtxoStateReader): Try[CandidateBlock] = Try {

    val nBits: Long = lastHeaderOpt
      .map(parent => history.requiredDifficultyAfter(parent))
      .map(d => RequiredDifficulty.encodeCompactBits(d))
      .getOrElse(Constants.InitialNBits)

    val extensionCandidate = ExtensionCandidate(Seq(), Seq())

    val upcomingContext = state.stateContext.upcoming(minerPk.h, ts, nBits, chainSettings.powScheme)

    //only transactions valid from against the current utxo state we take from the mem pool
    val emissionTxOpt = ErgoMiner.collectEmission(state, minerPk, fullHistorySettings.emission).map(_ -> EmissionTxCost)

    val txs = ErgoMiner.collectTxs(
      minerPk,
      Parameters.MaxBlockCost,
      Parameters.MaxBlockSize,
      state,
      upcomingContext,
      txsFromPool,
      emissionTxOpt.toSeq
    )

    state.proofsForTransactions(txs).map { case (adProof, adDigest) =>
      CandidateBlock(lastHeaderOpt, nBits, adDigest, adProof, txs, ts, extensionCandidate)
    }
  }.flatten

  @tailrec
  private def proveCandidate(candidate: CandidateBlock): ErgoFullBlock = {
    log.info(s"Trying to prove block with parent ${candidate.parentOpt.map(_.encodedId)} and timestamp ${candidate.timestamp}")

    pow.proveCandidate(candidate, prover.secrets.head.w) match {
      case Some(fb) => fb
      case _ =>
        val minerTag = scorex.utils.Random.randomBytes(Extension.FieldKeySize)
        proveCandidate(candidate.copy(extension = ExtensionCandidate(Seq(Array(0: Byte, 2: Byte) -> minerTag))))
    }
  }

  /**
    * Use reflection to set `minimalFullBlockHeightVar` to 0 to change regular synchronization rule, that we
    * first apply headers chain, and apply full blocks only after that
    */
  private def allowToApplyOldBlocks(history: ErgoHistory): Unit = {
    import scala.reflect.runtime.{universe => ru}
    val runtimeMirror = ru.runtimeMirror(getClass.getClassLoader)
    val procInstance = runtimeMirror.reflect(history.asInstanceOf[ToDownloadProcessor])
    val ppM = ru.typeOf[ToDownloadProcessor].member(ru.TermName("pruningProcessor")).asMethod
    val pp = procInstance.reflectMethod(ppM).apply().asInstanceOf[FullBlockPruningProcessor]
    val f = ru.typeOf[FullBlockPruningProcessor].member(ru.TermName("minimalFullBlockHeightVar")).asTerm.accessed.asTerm
    runtimeMirror.reflect(pp).reflectField(f).set(ErgoHistory.GenesisHeight)
    val f2 = ru.typeOf[FullBlockPruningProcessor].member(ru.TermName("isHeadersChainSyncedVar")).asTerm.accessed.asTerm
    runtimeMirror.reflect(pp).reflectField(f2).set(true)
  }

}
