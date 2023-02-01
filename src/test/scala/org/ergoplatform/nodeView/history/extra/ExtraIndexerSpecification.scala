package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.{ErgoAddress, ErgoAddressEncoder, ErgoBox, ErgoBoxCandidate, ErgoScriptPredef, P2PKAddress, UnsignedInput}
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.mining.{AutolykosPowScheme, CandidateBlock, CandidateGenerator}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.extension.{Extension, ExtensionCandidate}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.popow.NipopowAlgos
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool.SortingOption
import org.ergoplatform.nodeView.state.{ErgoState, ErgoStateContext, StateConstants, StateType, UtxoState, UtxoStateReader}
import org.ergoplatform.settings.{ErgoSettings, NetworkType, NodeConfigurationSettings}
import org.ergoplatform.utils.{ErgoPropertyTest, ErgoTestHelpers, HistoryTestHelpers}
import scorex.util.{ModifierId, bytesToId}
import sigmastate.Values
import sigmastate.basics.DLogProtocol.ProveDlog
import spire.implicits.cfor

import java.io.File
import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.Try

class ExtraIndexerSpecification extends ErgoPropertyTest with ExtraIndexerBase with HistoryTestHelpers {

  override protected val saveLimit: Int = 1 // save every block
  override protected implicit val addressEncoder: ErgoAddressEncoder = initSettings.chainSettings.addressEncoder

  val nodeSettings: NodeConfigurationSettings = NodeConfigurationSettings(StateType.Utxo, verifyTransactions = true,
    -1, utxoBootstrap = false, poPoWBootstrap = false, ChainGenerator.minimalSuffix, mining = false, ChainGenerator.txCostLimit, ChainGenerator.txSizeLimit, useExternalMiner = false,
    internalMinersCount = 1, internalMinerPollingInterval = 1.second, miningPubKeyHex = None, offlineGeneration = false,
    200, 5.minutes, 100000, 1.minute, mempoolSorting = SortingOption.FeePerByte, rebroadcastCount = 20,
    1000000, 100, adProofsSuffixLength = 112 * 1024, extraIndex = false)

  val HEIGHT: Int = 30
  val BRANCHPOINT: Int = HEIGHT / 2

  property("extra indexer rollback") {

    val dir: File = createTempDir
    dir.mkdirs()

    val fullHistorySettings: ErgoSettings = ErgoSettings(dir.getAbsolutePath, NetworkType.TestNet, initSettings.chainSettings,
      nodeSettings, settings.scorexSettings, settings.walletSettings, settings.cacheSettings)

    _history = ErgoHistory.readOrGenerate(fullHistorySettings)(null)

    ChainGenerator.generate(HEIGHT, dir)(_history)

    run()

    val txIndexBefore = globalTxIndex
    val boxIndexBefore = globalBoxIndex

    var txsIndexed: Int = 0
    var boxesIndexed: Int = 0

    // manually count balances
    val addresses: mutable.HashMap[ErgoAddress,Long] = mutable.HashMap[ErgoAddress,Long]()
    cfor(1)(_ <= BRANCHPOINT, _ + 1) { i =>
      _history.getReader.bestBlockTransactionsAt(i).get.txs.foreach(tx => { txsIndexed += 1
        if(i != 1) {
          tx.inputs.foreach(input => {
            val iEb: IndexedErgoBox = _history.getReader.typedExtraIndexById[IndexedErgoBox](bytesToId(input.boxId)).get
            val address: ErgoAddress = ExtraIndexer.getAddress(iEb.box.ergoTree)
            addresses.put(address, addresses(address) - iEb.box.value)
          })
        }
        tx.outputs.foreach(output => { boxesIndexed += 1
          val address: ErgoAddress =  addressEncoder.fromProposition(output.ergoTree).get
          addresses.put(address, addresses.getOrElse[Long](address, 0) + output.value)
        })
      })
    }

    removeAfter(BRANCHPOINT)

    var mismatches: Int = 0

    addresses.foreach(e => {
      _history.getReader.typedExtraIndexById[IndexedErgoAddress](bytesToId(IndexedErgoAddressSerializer.hashErgoTree(e._1.script))) match {
        case Some(iEa) =>
          if(iEa.balanceInfo.get.nanoErgs != e._2) {
            mismatches += 1
            System.err.println(s"Address ${e._1.toString} has ${iEa.balanceInfo.get.nanoErgs / 1000000000}ERG, ${e._2  / 1000000000}ERG expected")
          }
        case None =>
          if(e._2 != 0) {
            mismatches += 1
            System.err.println(s"Address ${e._1.toString} should exist, but was not found")
          }
      }
    })

    // indexnumbers
    globalTxIndex shouldBe txsIndexed
    globalBoxIndex shouldBe boxesIndexed

    // txs
    cfor(0)(_ < txIndexBefore, _ + 1) {txNum =>
      val txOpt = history.typedExtraIndexById[NumericTxIndex](bytesToId(NumericTxIndex.indexToBytes(txNum)))
      if(txNum < globalTxIndex)
        txOpt shouldNot be(empty)
      else
        txOpt shouldBe None
    }

    // boxes
    cfor(0)(_ < boxIndexBefore, _ + 1) { boxNum =>
      val boxOpt = history.typedExtraIndexById[NumericBoxIndex](bytesToId(NumericBoxIndex.indexToBytes(boxNum)))
      if (boxNum < globalBoxIndex)
        boxOpt shouldNot be(empty)
      else
        boxOpt shouldBe None
    }

    // balances
    mismatches shouldBe 0

  }

}

object ChainGenerator extends ErgoTestHelpers {

  val pow: AutolykosPowScheme = new AutolykosPowScheme(powScheme.k, powScheme.n)
  val blockInterval: FiniteDuration = 2.minute
  val EmissionTxCost: Long = 20000
  val MinTxAmount: Long = 2000000
  val RewardDelay: Int = initSettings.chainSettings.monetary.minerRewardDelay
  val MaxTxsPerBlock: Int = 10
  val minerPk: ProveDlog = defaultProver.hdKeys.head.publicImage
  val selfAddressScript: Values.ErgoTree = P2PKAddress(minerPk).script
  val minerProp: Values.ErgoTree = ErgoScriptPredef.rewardOutputScript(RewardDelay, minerPk)
  val votingEpochLength: Height = votingSettings.votingLength
  val protocolVersion: Byte = initSettings.chainSettings.protocolVersion
  val minimalSuffix = 2
  val txCostLimit: Height = initSettings.nodeSettings.maxTransactionCost
  val txSizeLimit: Height = initSettings.nodeSettings.maxTransactionSize

  var startTime: Long = 0

  def generate(length: Int, dir: File)(history: ErgoHistory): Unit = {
    val stateDir = new File(s"${dir.getAbsolutePath}/state")
    stateDir.mkdirs()
    val (state, _) = ErgoState.generateGenesisUtxoState(stateDir, StateConstants(initSettings))
    System.out.println(s"Going to generate a chain at ${dir.getAbsolutePath} starting from ${history.bestFullBlockOpt}")
    startTime = System.currentTimeMillis() - (blockInterval * (length - 1)).toMillis
    val chain = loop(state, None, None, Seq())(history)
    System.out.println(s"Chain of length ${chain.length} generated")
    history.bestHeaderOpt shouldBe history.bestFullBlockOpt.map(_.header)
    history.bestFullBlockOpt.get.id shouldBe chain.last
    System.out.println("History was generated successfully")
  }

  @tailrec
  private def loop(state: UtxoState,
                   initBox: Option[ErgoBox],
                   last: Option[Header],
                   acc: Seq[ModifierId])(history: ErgoHistory): Seq[ModifierId] = {
    val time: Long = last.map(_.timestamp + blockInterval.toMillis).getOrElse(startTime)
    if (time < System.currentTimeMillis()) {
      val (txs, lastOut) = genTransactions(last.map(_.height).getOrElse(ErgoHistory.GenesisHeight),
        initBox, state.stateContext)

      val candidate = genCandidate(defaultProver.hdPubKeys.head.key, last, time, txs, state)(history)
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

      loop(state.applyModifier(block, None)(_ => ()).get, outToPassNext, Some(block.header), acc :+ block.id)(history)
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
        canUnlock && bx.ergoTree != initSettings.chainSettings.monetary.emissionBoxProposition && bx.value >= MinTxAmount
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
      .map(d => RequiredDifficulty.encodeCompactBits(d))
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
