package org.ergoplatform.tools

import org.ergoplatform._
import org.ergoplatform.mining.difficulty.DifficultySerializer
import org.ergoplatform.mining.{AutolykosPowScheme, CandidateBlock, CandidateGenerator}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.extension.{Extension, ExtensionCandidate}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.popow.NipopowAlgos
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.ErgoHistoryUtils._
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.SortingOption
import org.ergoplatform.nodeView.state._
import org.ergoplatform.settings._
import org.ergoplatform.utils.{ErgoTestHelpers, HistoryTestHelpers}
import org.ergoplatform.wallet.boxes.{BoxSelector, ReplaceCompactCollectBoxSelector}
import org.scalatest.matchers.should.Matchers
import scorex.util.ModifierId
import sigmastate.crypto.DLogProtocol.ProveDlog

import java.io.File
import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.util.Try

/**
  * Application object for chain generation.
  * Takes 2 parameters: start timestamp and path to history folder.
  * Generate blocks starting from start timestamp and until current time with expected block interval
  * between them, to ensure that difficulty does not change.
  */
object ChainGenerator extends App with ErgoTestHelpers with Matchers {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  implicit val addressEncoder: ErgoAddressEncoder = settings.addressEncoder

  val realNetworkSetting = {
    val initSettings = ErgoSettingsReader.read(Args(None, Some(NetworkType.TestNet)))
    initSettings.copy(chainSettings = initSettings.chainSettings.copy(genesisId = None))
  }

  val EmissionTxCost: Long = 20000
  val MinTxAmount: Long = 2000000
  val RewardDelay: Int = realNetworkSetting.chainSettings.monetary.minerRewardDelay
  val MaxTxsPerBlock: Int = 10

  val prover = defaultProver
  val minerPk = prover.hdKeys.head.publicImage
  val selfAddressScript = P2PKAddress(minerPk).script
  val minerProp = ErgoTreePredef.rewardOutputScript(RewardDelay, minerPk)

  val pow = new AutolykosPowScheme(powScheme.k, powScheme.n)
  val blockInterval = 2.minute

  val boxSelector: BoxSelector = new ReplaceCompactCollectBoxSelector(30, 2, None)

  val startTime = args.headOption.map(_.toLong).getOrElse(System.currentTimeMillis() - (blockInterval * 10).toMillis)
  val dir = if (args.length < 2) new File("/tmp/ergo/data") else new File(args(1))
  val txsSize: Int = if (args.length < 3) 100 * 1024 else args(2).toInt

  val minimalSuffix = 2
  val txCostLimit     = initSettings.nodeSettings.maxTransactionCost
  val txSizeLimit     = initSettings.nodeSettings.maxTransactionSize
  val nodeSettings: NodeConfigurationSettings = NodeConfigurationSettings(StateType.Utxo, verifyTransactions = true,
    -1, UtxoSettings(false, 0, 2), NipopowSettings(false, 1), mining = false, txCostLimit, txSizeLimit, useExternalMiner = false,
    internalMinersCount = 1, internalMinerPollingInterval = 1.second, miningPubKeyHex = None, offlineGeneration = false,
    200, 5.minutes, 100000, 1.minute, mempoolSorting = SortingOption.FeePerByte, rebroadcastCount = 20,
    1000000, 100, adProofsSuffixLength = 112*1024, extraIndex = false)
  val ms = settings.chainSettings.monetary.copy(
    minerRewardDelay = RewardDelay
  )
  val cs = realNetworkSetting.chainSettings

  val fullHistorySettings: ErgoSettings = ErgoSettings(dir.getAbsolutePath, NetworkType.TestNet, cs,
    nodeSettings, settings.scorexSettings, settings.walletSettings, settings.cacheSettings)
  val stateDir = ErgoState.stateDir(fullHistorySettings)
  stateDir.mkdirs()

  val votingEpochLength = votingSettings.votingLength
  val protocolVersion = fullHistorySettings.chainSettings.protocolVersion

  val history = ErgoHistory.readOrGenerate(fullHistorySettings)(context = null)
  HistoryTestHelpers.allowToApplyOldBlocks(history)
  val (state, _) = ErgoState.generateGenesisUtxoState(stateDir, fullHistorySettings)
  log.info(s"Going to generate a chain at ${dir.getAbsoluteFile} starting from ${history.bestFullBlockOpt}")

  val chain = loop(state, None, None, Seq())
  log.info(s"Chain of length ${chain.length} generated")
  history.bestHeaderOpt shouldBe history.bestFullBlockOpt.map(_.header)
  history.bestFullBlockOpt.get.id shouldBe chain.last
  log.info("History was generated successfully")
  System.exit(0)

  private def loop(state: UtxoState,
                   initBox: Option[ErgoBox],
                   last: Option[Header],
                   acc: Seq[ModifierId]): Seq[ModifierId] = {
    val time: Long = last.map(_.timestamp + blockInterval.toMillis).getOrElse(startTime)
    if (time < System.currentTimeMillis()) {
      val (txs, lastOut) = genTransactions(last.map(_.height).getOrElse(GenesisHeight),
        initBox, state.stateContext)

      val candidate = genCandidate(prover.hdPubKeys.head.key, last, time, txs, state)
      val block = proveCandidate(candidate.get)

      history.append(block.header).get
      block.blockSections.foreach(s => if (!history.contains(s)) history.append(s).get)

      val outToPassNext = if (last.isEmpty) {
        block.transactions.flatMap(_.outputs).find(_.ergoTree == minerProp)
      } else {
        lastOut
      }

      assert(outToPassNext.isDefined)

      log.info(
        s"Block ${block.id} with ${block.transactions.size} transactions at height ${block.header.height} generated")

      loop(state.applyModifier(block, None)(_ => ()).get, outToPassNext, Some(block.header), acc :+ block.id)
    } else {
      acc
    }
  }

  private def genTransactions(height: Height,
                              inOpt: Option[ErgoBox],
                              ctx: ErgoStateContext): (Seq[ErgoTransaction], Option[ErgoBox]) = {
    inOpt
      .find { bx =>
        val canUnlock = (bx.creationHeight + RewardDelay <= height) || (bx.ergoTree != minerProp)
        canUnlock && bx.ergoTree != cs.monetary.emissionBoxProposition && bx.value >= MinTxAmount
      }
      .map { input =>
        val qty = MaxTxsPerBlock
        val amount = input.value
        val outs = (0 until qty).map(_ => new ErgoBoxCandidate(amount, selfAddressScript, height))
        val x = outs
          .foldLeft((Seq.empty[ErgoTransaction], input)) { case ((acc, in), out) =>
            val inputs = IndexedSeq(in)
            val unsignedTx = UnsignedErgoTransaction(
              inputs.map(_.id).map(id => new UnsignedInput(id)),
              IndexedSeq(out)
            )

            prover.sign(unsignedTx, inputs, emptyDataBoxes, ctx)
              .fold(_ => acc -> in, tx => (acc :+ ErgoTransaction(tx)) -> unsignedTx.outputs.head)
          }
          ._1
        (x, Some(x.last.outputs.head))
      }
      .getOrElse(Seq.empty -> inOpt)
  }

  private def genCandidate(minerPk: ProveDlog,
                           lastHeaderOpt: Option[Header],
                           ts: Long,
                           txsFromPool: Seq[ErgoTransaction],
                           state: UtxoStateReader): Try[CandidateBlock] = Try {
    val stateContext = state.stateContext
    val nBits: Long = lastHeaderOpt
      .map(parent => history.requiredDifficultyAfter(parent))
      .map(d => DifficultySerializer.encodeCompactBits(d))
      .getOrElse(settings.chainSettings.initialNBits)

    val interlinks = lastHeaderOpt
      .flatMap { h =>
        history.typedModifierById[Extension](h.extensionId)
          .flatMap(ext => NipopowAlgos.unpackInterlinks(ext.fields).toOption)
          .map(nipopowAlgos.updateInterlinks(h, _))
      }
      .getOrElse(Seq.empty)
    val interlinksExtension = nipopowAlgos.interlinksToExtension(interlinks)

    val (extensionCandidate, votes: Array[Byte], version: Byte) = lastHeaderOpt.map { header =>
      val newHeight = header.height + 1
      val currentParams = stateContext.currentParameters
      val betterVersion = protocolVersion > header.version
      val votingFinishHeight: Option[Height] = currentParams.softForkStartingHeight
        .map(_ + votingSettings.votingLength * votingSettings.softForkEpochs)
      val forkVotingAllowed = votingFinishHeight.forall(fh => newHeight < fh)
      val forkOrdered = settings.votingTargets.softFork != 0
      val voteForFork = betterVersion && forkOrdered && forkVotingAllowed

      if (newHeight % votingEpochLength == 0 && newHeight > 0) {
        val (newParams, _) = currentParams.update(newHeight, voteForFork, stateContext.votingData.epochVotes, emptyVSUpdate, votingSettings)
        (newParams.toExtensionCandidate ++ interlinksExtension,
          newParams.suggestVotes(settings.votingTargets.targets, voteForFork),
          newParams.blockVersion)
      } else {
        (nipopowAlgos.interlinksToExtension(interlinks),
          currentParams.vote(settings.votingTargets.targets, stateContext.votingData.epochVotes, voteForFork),
          currentParams.blockVersion)
      }
    }.getOrElse((interlinksExtension, Array(0: Byte, 0: Byte, 0: Byte), Header.InitialVersion))

    val emissionTxOpt = CandidateGenerator.collectEmission(state, minerPk, emptyStateContext)
    val txs = emissionTxOpt.toSeq ++ txsFromPool

    state.proofsForTransactions(txs).map { case (adProof, adDigest) =>
      CandidateBlock(lastHeaderOpt, version, nBits, adDigest, adProof, txs, ts, extensionCandidate, votes)
    }
  }.flatten

  @tailrec
  private def proveCandidate(candidate: CandidateBlock): ErgoFullBlock = {
    log.info(s"Trying to prove block with parent ${candidate.parentOpt.map(_.encodedId)} and timestamp ${candidate.timestamp}")

    pow.proveCandidate(candidate, prover.hdKeys.head.privateInput.w) match {
      case Some(fb) => fb
      case _ =>
        val interlinks = candidate.parentOpt
          .map(nipopowAlgos.updateInterlinks(_, NipopowAlgos.unpackInterlinks(candidate.extension.fields).get))
          .getOrElse(Seq.empty)
        val minerTag = scorex.utils.Random.randomBytes(Extension.FieldKeySize)
        proveCandidate {
          candidate.copy(
            extension = ExtensionCandidate(Seq(Array(0: Byte, 2: Byte) -> minerTag)) ++ nipopowAlgos.interlinksToExtension(interlinks)
          )
        }
    }
  }

}
