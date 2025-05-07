package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.mining.difficulty.DifficultySerializer
import org.ergoplatform.mining.{AutolykosPowScheme, CandidateBlock, CandidateGenerator}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.extension.{Extension, ExtensionCandidate}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.popow.NipopowAlgos
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.ErgoHistoryUtils.GenesisHeight
import org.ergoplatform.nodeView.state.{ErgoState, ErgoStateContext, UtxoState, UtxoStateReader}
import org.ergoplatform.utils.ErgoTestHelpers
import org.ergoplatform._
import org.scalatest.matchers.should.Matchers
import scorex.util.ModifierId
import sigma.ast.ErgoTree
import sigma.{Coll, Colls}
import sigma.data.ProveDlog
import sigmastate.eval.Extensions._

import java.io.File
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.{Random, Try}

object ChainGenerator extends ErgoTestHelpers with Matchers {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators._

  val pow: AutolykosPowScheme = new AutolykosPowScheme(powScheme.k, powScheme.n)
  val blockInterval: FiniteDuration = 1.minute
  val EmissionTxCost: Long = 20000
  val MinTxAmount: Long = 2000000
  val RewardDelay: Int = initSettings.chainSettings.monetary.minerRewardDelay
  val MaxTxsPerBlock: Int = 10
  val minerPk: ProveDlog = defaultProver.hdKeys.head.publicImage
  val selfAddressScript: ErgoTree = P2PKAddress(minerPk).script
  val minerProp: ErgoTree = ErgoTreePredef.rewardOutputScript(RewardDelay, minerPk)
  val votingEpochLength: Height = votingSettings.votingLength
  val protocolVersion: Byte = initSettings.chainSettings.protocolVersion
  val minimalSuffix = 2
  val txCostLimit: Height = initSettings.nodeSettings.maxTransactionCost
  val txSizeLimit: Height = initSettings.nodeSettings.maxTransactionSize
  val startTime: Long = System.currentTimeMillis() - ((5000 - 1) * blockInterval.toMillis)

  var endTime: Long = 0

  def generate(length: Int, dir: File, history: ErgoHistory, stateOpt: Option[UtxoState]): Option[UtxoState] = {
    val state = stateOpt.getOrElse {
        val stateDir = new File(s"${dir.getAbsolutePath}/state")
        stateDir.mkdirs()
        ErgoState.generateGenesisUtxoState(stateDir, initSettings)._1
    }
    System.out.println(s"Going to ${if(stateOpt.isEmpty) "generate" else "extend"} chain at " +
      s"${dir.getAbsolutePath} starting from ${history.fullBlockHeight}")
    endTime = startTime + (blockInterval * length).toMillis
    val initBox = history.bestFullBlockOpt.map(_.transactions.last.outputs.head)
    val chain = loop(state, initBox, history.bestHeaderOpt, Seq())(history)
    history.bestHeaderOpt shouldBe history.bestFullBlockOpt.map(_.header)
    history.bestFullBlockOpt.get.id shouldBe chain.last
    System.out.println(s"History ${if(stateOpt.isEmpty) "generated" else "extended"} successfully, " +
      s"blocks: ${history.fullBlockHeight}")
    Some(state)
  }

  @tailrec
  private def loop(state: UtxoState,
                   initBox: Option[ErgoBox],
                   last: Option[Header],
                   acc: Seq[ModifierId])(history: ErgoHistory): Seq[ModifierId] = {
    val time: Long = last.map(_.timestamp + blockInterval.toMillis).getOrElse(startTime)
    if (time < endTime) {
      val (txs, lastOut) = genTransactions(last.map(_.height).getOrElse(GenesisHeight),
        initBox, state.stateContext)

      val candidate = genCandidate(defaultProver.hdPubKeys.head.key, last, time, txs, state)(history)
      val block = proveCandidate(candidate.get)

      assert(history.append(block.header).isSuccess)
      block.blockSections.foreach(s => if (!history.contains(s)) assert(history.append(s).isSuccess))

      val outToPassNext = if (last.isEmpty) {
        block.transactions.flatMap(_.outputs).find(_.ergoTree == minerProp)
      } else {
        lastOut
      }

      assert(outToPassNext.isDefined)

      System.out.println(s"Block ${block.id} with ${block.transactions.size} transactions at height ${block.header.height} generated")

      loop(state.applyModifier(block, None)(_ => ()).get, outToPassNext, Some(block.header), acc :+ block.id)(history)
    } else {
      acc
    }
  }

  private def moveTokens(inOpt: Option[ErgoBox], cond: Boolean): Coll[(TokenId, Long)] = {
    val tokens: ArrayBuffer[(TokenId, Long)] = ArrayBuffer.empty[(TokenId, Long)]
    inOpt match {
      case Some(input) if cond =>
        tokens += Tuple2(input.id.toTokenId, math.abs(Random.nextInt()))
      case Some(tokenBox) if !cond =>
        tokenBox.additionalTokens.toArray.foreach(tokens += _)
      case _ =>
    }
    Colls.fromArray(tokens.toArray)
  }

  private def genTransactions(height: Height,
                              inOpt: Option[ErgoBox],
                              ctx: ErgoStateContext): (Seq[ErgoTransaction], Option[ErgoBox]) = {
    inOpt
      .find { bx =>
        val canUnlock = (bx.creationHeight + RewardDelay <= height) || (bx.ergoTree != minerProp)
        canUnlock && bx.ergoTree != initSettings.chainSettings.monetary.emissionBoxProposition && bx.value >= MinTxAmount
      }
      .map { input =>
        val qty = MaxTxsPerBlock
        val amount = input.value
        val outs = (0 until qty).map(i => new ErgoBoxCandidate(amount, selfAddressScript, height, moveTokens(inOpt, i == 0)))
        var i = 0
        val x = outs
          .foldLeft((Seq.empty[ErgoTransaction], input)) { case ((acc, in), out) =>
            val inputs = IndexedSeq(in)
            val newOut =
              if (i > 0)
                new ErgoBoxCandidate(amount, selfAddressScript, height, moveTokens(acc.lastOption.map(_.outputs.head), cond = false))
              else
                out
            val unsignedTx = UnsignedErgoTransaction(inputs.map(box => new UnsignedInput(box.id)), IndexedSeq(newOut))
            i += 1
            defaultProver.sign(unsignedTx, inputs, emptyDataBoxes, ctx)
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
                           state: UtxoStateReader)(history: ErgoHistory): Try[CandidateBlock] = Try {
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

    pow.proveCandidate(candidate, defaultProver.hdKeys.head.privateInput.w) match {
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
