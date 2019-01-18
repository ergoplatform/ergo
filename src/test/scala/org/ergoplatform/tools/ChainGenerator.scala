package org.ergoplatform.tools

import java.io.File

import akka.actor.ActorSystem
import akka.testkit.TestKit
import org.ergoplatform._
import org.ergoplatform.local.ErgoMiner
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.mining.{AutolykosPowScheme, CandidateBlock}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{Extension, ExtensionCandidate, Header}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import org.ergoplatform.nodeView.history.storage.modifierprocessors.{FullBlockPruningProcessor, ToDownloadProcessor}
import org.ergoplatform.nodeView.state._
import org.ergoplatform.nodeView.wallet._
import org.ergoplatform.settings._
import org.ergoplatform.utils.ErgoTestHelpers
import scorex.crypto.hash.Digest32
import scorex.util.{ModifierId, idToBytes}
import sigmastate.basics.DLogProtocol.ProveDlog

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.util.{Failure, Random, Success, Try}

/**
  * Application object for chain generation.
  * Takes 2 parameters: start timestamp and path to history folder.
  * Generate blocks starting from start timestamp and until current time with expected block interval
  * between them, to ensure that difficulty does not change.
  */
object ChainGenerator extends TestKit(ActorSystem()) with App with ErgoTestHelpers {

  implicit val ergoAddressEncoder: ErgoAddressEncoder =
    ErgoAddressEncoder(settings.chainSettings.addressPrefix)

  val EmissionTxCost: Long = 20000
  val MinTxAmount: Long = 2000000
  val RewardDelay: Int = 720

  val prover = defaultProver

  val pow = new AutolykosPowScheme(powScheme.k, powScheme.n)
  val blockInterval = 2.minute

  val boxSelector: BoxSelector = DefaultBoxSelector

  val startTime = args.headOption.map(_.toLong).getOrElse(timeProvider.time - (blockInterval * 10).toMillis)
  val dir = if (args.length < 2) new File("/Users/oskin/Desktop/Dev/scala_dev/ergo/ergo/data") else new File(args(1))
  val txsSize: Int = if (args.length < 3) 100 * 1024 else args(2).toInt

  val miningDelay = 1.second
  val minimalSuffix = 2
  val nodeSettings: NodeConfigurationSettings = NodeConfigurationSettings(StateType.Utxo, verifyTransactions = true,
    -1, PoPoWBootstrap = false, minimalSuffix, mining = false, miningDelay, offlineGeneration = false, 200)
  val monetarySettings = settings.chainSettings.monetary.copy(
    minerRewardDelay = RewardDelay,
    afterGenesisStateDigestHex = "d801a0e4573d6993caa2eda7dc97aad2b4c8ed51ebb0afe0dd272c8d7e26d5fd01"
  )
  val chainSettings = ChainSettings(0: Byte, 0: Byte, blockInterval, 256, 8, votingSettings, pow, monetarySettings)
  val fullHistorySettings: ErgoSettings = ErgoSettings(dir.getAbsolutePath, chainSettings, settings.testingSettings,
    nodeSettings, settings.scorexSettings, settings.walletSettings, CacheSettings.default)
  val stateDir = ErgoState.stateDir(fullHistorySettings)
  stateDir.mkdirs()

  val votingEpochLength = votingSettings.votingLength
  val protocolVersion = fullHistorySettings.chainSettings.protocolVersion

  val history = ErgoHistory.readOrGenerate(fullHistorySettings, timeProvider)
  allowToApplyOldBlocks(history)
  val (state, initBoxHolder) = ErgoState.generateGenesisUtxoState(stateDir, StateConstants(None, fullHistorySettings))
  log.error(s"Going to generate a chain at ${dir.getAbsoluteFile} starting from ${history.bestFullBlockOpt}")

  val chain = loop(state, initBoxHolder, None, Seq())
  log.info(s"Chain of length ${chain.length} generated")
  history.bestHeaderOpt shouldBe history.bestFullBlockOpt.map(_.header)
  history.bestFullBlockOpt.get.id shouldBe chain.last
  log.info("History was generated successfully")
  System.exit(0)

  private def loop(state: UtxoState, bh: BoxHolder, last: Option[Header], acc: Seq[ModifierId]): Seq[ModifierId] = {
    val time: Long = last.map(_.timestamp + blockInterval.toMillis).getOrElse(startTime)
    if (time < timeProvider.time) {
      val (txs, newBh) = genTransactions(last.map(_.height).getOrElse(ErgoHistory.GenesisHeight), bh, state.stateContext)

      val candidate = genCandidate(prover.dlogPubkeys.head, last, time, txs, state)

      val block = proveCandidate(candidate.get)
      history.append(block.header).get
      block.blockSections.foreach(s => if (!history.contains(s)) history.append(s).get)
      log.info(s"Block ${block.id} at height ${block.header.height} generated")
      loop(state.applyModifier(block).get, newBh, Some(block.header), acc :+ block.id)
    } else {
      acc
    }
  }

  private def genTransactions(height: Height,
                              bh: BoxHolder,
                              ctx: ErgoStateContext): (Seq[ErgoTransaction], BoxHolder) = {
    val balance = bh.boxes.filter(_._2.creationHeight + RewardDelay >= height).map(_._2.value).sum
    if (balance >= MinTxAmount) {
      val qty = (balance / MinTxAmount).toInt
      val amount = balance / qty
      val outs = (0 to qty).foldLeft(Seq.empty[ErgoBoxCandidate]) { case (acc, _) =>
        val bc = new ErgoBoxCandidate(amount, Pay2SAddress(prover.dlogPubkeys.head).script, height)
        acc :+ bc
      }
      val (txs, usedBoxed) = outs
        .flatMap { out =>
          val bxs = bh.boxes.values.map { box =>
            TrackedBox(invalidErgoTransactionGen.sample.get, 0, None, box, BoxCertainty.Certain)
          }
          boxSelector.select(bxs.iterator, _ => true, amount, Map.empty).map { r =>
            val inputs = r.boxes.toIndexedSeq

            val changeAddress = prover.dlogPubkeys(Random.nextInt(prover.dlogPubkeys.size))

            val changeBoxCandidates = r.changeBoxes.map { case (ergChange, tokensChange) =>
              val assets = tokensChange.map(t => Digest32 @@ idToBytes(t._1) -> t._2).toIndexedSeq
              new ErgoBoxCandidate(ergChange, changeAddress, height, assets)
            }

            val unsignedTx = new UnsignedErgoTransaction(
              inputs.map(_.id).map(id => new UnsignedInput(id)),
              (out +: changeBoxCandidates).toIndexedSeq
            )

            prover.sign(unsignedTx, inputs, ctx)
              .fold(e => Failure(new Exception(s"Failed to sign boxes: $inputs", e)), tx => Success(tx -> inputs))
          }
        }
        .collect { case Success(res) => res }
        .foldLeft(Seq.empty[ErgoTransaction], Seq.empty[ErgoBox]) { case ((txsAcc, inputsAcc), (tx, inputs)) =>
          (txsAcc :+ tx, inputsAcc ++ inputs)
        }
      txs -> bh.filter(!usedBoxed.contains(_))
    } else {
      Seq.empty -> bh
    }
  }

  private def genCandidate(minerPk: ProveDlog,
                           lastHeaderOpt: Option[Header],
                           ts: Long,
                           txsFromPool: Seq[ErgoTransaction],
                           state: UtxoStateReader): Try[CandidateBlock] = Try {
    val stateContext = state.stateContext
    val nBits: Long = lastHeaderOpt
      .map(parent => history.requiredDifficultyAfter(parent))
      .map(d => RequiredDifficulty.encodeCompactBits(d))
      .getOrElse(Constants.InitialNBits)

    val emptyExtensionCandidate = ExtensionCandidate(Seq())
    val (extensionCandidate, votes: Array[Byte], version: Byte) = lastHeaderOpt.map { header =>
      val newHeight = header.height + 1
      val currentParams = stateContext.currentParameters
      val betterVersion = protocolVersion > header.version
      val votingFinishHeight: Option[Height] = currentParams.softForkStartingHeight
        .map(h => h + votingSettings.votingLength * votingSettings.softForkEpochs)
      val forkVotingAllowed = votingFinishHeight.forall(fh => newHeight < fh)
      val forkOrdered = fullHistorySettings.votingTargets.getOrElse(Parameters.SoftFork, 0) != 0
      val voteForFork = betterVersion && forkOrdered && forkVotingAllowed

      if (newHeight % votingEpochLength == 0 && newHeight > 0) {
        val newParams = currentParams.update(newHeight, voteForFork, stateContext.votingData.epochVotes, votingSettings)
        (newParams.toExtensionCandidate(Seq()),
          newParams.suggestVotes(fullHistorySettings.votingTargets, voteForFork),
          newParams.blockVersion)
      } else {
        (emptyExtensionCandidate,
          currentParams.vote(fullHistorySettings.votingTargets, stateContext.votingData.epochVotes, voteForFork),
          currentParams.blockVersion)
      }
    }.getOrElse((emptyExtensionCandidate, Array(0: Byte, 0: Byte, 0: Byte), Header.CurrentVersion))

    val upcomingContext = state.stateContext.upcoming(minerPk.h, ts, nBits, votes, version,
      fullHistorySettings.chainSettings.powScheme)
    val emissionTxOpt = ErgoMiner.collectEmission(state, minerPk, fullHistorySettings.emission).map(_ -> EmissionTxCost)

    val txs = ErgoMiner.collectTxs(minerPk,
      state.stateContext.currentParameters.maxBlockCost,
      state.stateContext.currentParameters.maxBlockSize,
      state,
      upcomingContext,
      txsFromPool,
      emissionTxOpt.toSeq)

    state.proofsForTransactions(txs).map { case (adProof, adDigest) =>
      CandidateBlock(lastHeaderOpt, version, nBits, adDigest, adProof, txs, ts, extensionCandidate, votes)
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
