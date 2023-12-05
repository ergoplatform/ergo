package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform._
import org.ergoplatform.mining.difficulty.DifficultySerializer
import org.ergoplatform.mining.{AutolykosPowScheme, CandidateBlock, CandidateGenerator}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.extension.{Extension, ExtensionCandidate}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.popow.NipopowAlgos
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.ErgoHistoryConstants._
import org.ergoplatform.nodeView.history.extra.IndexedErgoAddressSerializer.hashErgoTree
import org.ergoplatform.nodeView.history.extra.SegmentSerializer.{boxSegmentId, txSegmentId}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolTypes.SortingOption
import org.ergoplatform.nodeView.state._
import org.ergoplatform.settings.{ErgoSettings, NetworkType, NipopowSettings, NodeConfigurationSettings, UtxoSettings}
import org.ergoplatform.utils.{ErgoPropertyTest, ErgoTestHelpers, HistoryTestHelpers}
import scorex.util.{ModifierId, bytesToId}
import sigmastate.Values
import sigmastate.crypto.DLogProtocol.ProveDlog
import sigmastate.eval.Extensions._
import sigmastate.eval._
import sigma.{Coll, Colls}
import spire.implicits.cfor

import java.io.File
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.reflect.ClassTag
import scala.util.{Random, Try}

class ExtraIndexerSpecification extends ErgoPropertyTest with ExtraIndexerBase with HistoryTestHelpers {

  type ID_LL = mutable.HashMap[ModifierId,(Long,Long)]

  override protected val saveLimit: Int = 1 // save every block
  override protected implicit val segmentTreshold: Int = 8 // split to smaller segments
  override protected implicit val addressEncoder: ErgoAddressEncoder = initSettings.chainSettings.addressEncoder

  val nodeSettings: NodeConfigurationSettings = NodeConfigurationSettings(StateType.Utxo, verifyTransactions = true,
    -1, UtxoSettings(utxoBootstrap = false, 0, 2), NipopowSettings(nipopowBootstrap = false, 1), mining = false,
    ChainGenerator.txCostLimit, ChainGenerator.txSizeLimit, useExternalMiner = false, internalMinersCount = 1,
    internalMinerPollingInterval = 1.second, miningPubKeyHex = None, offlineGeneration = false,
    200, 5.minutes, 100000, 1.minute, mempoolSorting = SortingOption.FeePerByte, rebroadcastCount = 20,
    1000000, 100, adProofsSuffixLength = 112 * 1024, extraIndex = false)

  val HEIGHT: Int = 50
  val BRANCHPOINT: Int = HEIGHT / 2

  def createDB(): Unit = {
    val dir: File = createTempDir
    dir.mkdirs()

    val fullHistorySettings: ErgoSettings = ErgoSettings(dir.getAbsolutePath, NetworkType.TestNet, initSettings.chainSettings,
      nodeSettings, settings.scorexSettings, settings.walletSettings, settings.cacheSettings)

    _history = ErgoHistory.readOrGenerate(fullHistorySettings)(null)

    ChainGenerator.generate(HEIGHT, dir)(_history)

    // reset all variables
    indexedHeight = 0
    globalTxIndex = 0L
    globalBoxIndex = 0L
    lastWroteToDB = 0
    caughtUp = false
    rollback = false
    general.clear()
    boxes.clear()
    trees.clear()
    tokens.clear()
    segments.clear()
  }

  def manualIndex(limit: Int): (ID_LL, // address -> (erg,tokenSum)
                                ID_LL, // tokenId -> (boxesCount,_)
                                Int, // txs indexed
                                Int) = { // boxes indexed
    var txsIndexed = 0
    var boxesIndexed = 0
    val addresses: ID_LL = mutable.HashMap[ModifierId,(Long,Long)]()
    val indexedTokens: ID_LL = mutable.HashMap[ModifierId,(Long,Long)]()
    cfor(1)(_ <= limit, _ + 1) { i =>
      _history.getReader.bestBlockTransactionsAt(i).get.txs.foreach { tx =>
        txsIndexed += 1
        if (i != 1) {
          tx.inputs.foreach { input =>
            val iEb: IndexedErgoBox = _history.getReader.typedExtraIndexById[IndexedErgoBox](bytesToId(input.boxId)).get
            val address = hashErgoTree(ExtraIndexer.getAddress(iEb.box.ergoTree)(addressEncoder).script)
            val prev = addresses(address)
            addresses.put(address, (prev._1 - iEb.box.value, prev._2 - iEb.box.additionalTokens.toArray.map(_._2).sum))
          }
        }
        tx.outputs.foreach { output =>
          boxesIndexed += 1
          val address = hashErgoTree(addressEncoder.fromProposition(output.ergoTree).get.script)
          val prev = addresses.getOrElse(address, (0L, 0L))
          addresses.put(address, (prev._1 + output.value, prev._2 + output.additionalTokens.toArray.map(_._2).sum))
          cfor(0)(_ < output.additionalTokens.length, _ + 1) { j =>
            val token = IndexedToken.fromBox(new IndexedErgoBox(i, None, None, output, 0), j)
            val prev2 = indexedTokens.getOrElse(token.id, (0L, 0L))
            indexedTokens.put(token.id, (prev2._1 + 1, 0))
          }
        }
      }
    }
    (addresses, indexedTokens, txsIndexed, boxesIndexed)
  }

  def checkSegmentables[T <: Segment[_] : ClassTag](segmentables: ID_LL,
                                                    isChild: Boolean = false,
                                                    check: ((T, (Long, Long))) => Boolean
                                                   ): Int = {
    var errors: Int = 0
    segmentables.foreach { segmentable =>
      history.typedExtraIndexById[T](segmentable._1) match {
        case Some(obj: T) =>
          if(isChild) { // this is a segment
            // check tx segments
            val txSegments: ID_LL = mutable.HashMap.empty[ModifierId,(Long,Long)]
            txSegments ++= (0 until obj.txSegmentCount).map(n => obj.idMod(txSegmentId(obj.parentId, n))).map(Tuple2(_, (0L, 0L)))
            checkSegmentables(txSegments, isChild = true, check) shouldBe 0
            // check box segments
            val boxSegments: ID_LL = mutable.HashMap.empty[ModifierId,(Long,Long)]
            boxSegments ++= (0 until obj.boxSegmentCount).map(n => obj.idMod(boxSegmentId(obj.parentId, n))).map(Tuple2(_, (0L, 0L)))
            checkSegmentables(boxSegments, isChild = true, check) shouldBe 0
          }else { // this is the parent object
            // check properties of object
            if(!check((obj, segmentable._2)))
              errors += 1
            // check boxes in memory
            obj.boxes.foreach { boxNum =>
              NumericBoxIndex.getBoxByNumber(history, boxNum) match {
                case Some(iEb) =>
                  if (iEb.isSpent)
                    boxNum.toInt should be <= 0
                  else
                    boxNum.toInt should be >= 0
                case None =>
                  System.err.println(s"Box $boxNum not found in database")
                  errors += 1
              }
            }
            // check txs in memory
            obj.txs.foreach { txNum =>
              NumericTxIndex.getTxByNumber(history, txNum) shouldNot be(empty)
            }
          }
        case None =>
          System.err.println(s"Segmentable object ${segmentable._1} should exist, but was not found")
          errors += 1
      }
    }
    errors
  }

  def checkAddresses(addresses: ID_LL): Int =
    checkSegmentables[IndexedErgoAddress](addresses, isChild = false, seg => {
      seg._1.balanceInfo.get.nanoErgs == seg._2._1 && seg._1.balanceInfo.get.tokens.map(_._2).sum == seg._2._2
    })

  def checkTokens(indexedTokens: ID_LL): Int =
    checkSegmentables[IndexedToken](indexedTokens, isChild = false, seg => {
      seg._1.boxCount == seg._2._1
    })

  property("extra indexer transactions") {
    createDB()
    run()
    cfor(0)(_ < globalTxIndex, _ + 1) {n =>
      val id = history.typedExtraIndexById[NumericTxIndex](bytesToId(NumericTxIndex.indexToBytes(n)))
      id shouldNot be(empty)
      history.typedExtraIndexById[IndexedErgoTransaction](id.get.m) shouldNot be(empty)
    }
  }

  property("extra indexer boxes") {
    createDB()
    run()
    cfor(0)(_ < globalBoxIndex, _ + 1) { n =>
      val id = history.typedExtraIndexById[NumericBoxIndex](bytesToId(NumericBoxIndex.indexToBytes(n)))
      id shouldNot be(empty)
      history.typedExtraIndexById[IndexedErgoBox](id.get.m) shouldNot be(empty)
    }
  }

  property("extra indexer addresses") {
    createDB()
    run()
    val (addresses, _, _, _) = manualIndex(HEIGHT)
    checkAddresses(addresses) shouldBe 0
  }

  property("extra indexer tokens") {
    createDB()
    run()
    val (_, indexedTokens, _, _) = manualIndex(HEIGHT)
    checkTokens(indexedTokens) shouldBe 0
  }

  property("extra indexer rollback") {
    createDB()

    run()

    val txIndexBefore = globalTxIndex
    val boxIndexBefore = globalBoxIndex

    // manually count balances
    val (addresses, indexedTokens, txsIndexed, boxesIndexed) = manualIndex(BRANCHPOINT)

    // perform rollback
    removeAfter(BRANCHPOINT)

    // address balances
    checkAddresses(addresses) shouldBe 0

    // token indexes
    checkTokens(indexedTokens) shouldBe 0

    // check indexnumbers
    globalTxIndex shouldBe txsIndexed
    globalBoxIndex shouldBe boxesIndexed

    // check txs
    cfor(0)(_ < txIndexBefore, _ + 1) {txNum =>
      val txOpt = history.typedExtraIndexById[NumericTxIndex](bytesToId(NumericTxIndex.indexToBytes(txNum)))
      if(txNum < globalTxIndex)
        txOpt shouldNot be(empty)
      else
        txOpt shouldBe None
    }

    // check boxes
    cfor(0)(_ < boxIndexBefore, _ + 1) { boxNum =>
      val boxOpt = history.typedExtraIndexById[NumericBoxIndex](bytesToId(NumericBoxIndex.indexToBytes(boxNum)))
      if (boxNum < globalBoxIndex)
        boxOpt shouldNot be(empty)
      else
        boxOpt shouldBe None
    }

    // -------------------------------------------------------------------
    // restart indexer to catch up
    run()

    // Check addresses again
    val (addresses2, indexedTokens2, _, _) = manualIndex(HEIGHT)
    checkAddresses(addresses2) shouldBe 0
    checkTokens(indexedTokens2) shouldBe 0

    // check indexnumbers again
    globalTxIndex shouldBe txIndexBefore
    globalBoxIndex shouldBe boxIndexBefore

    // check txs after caught up
    cfor(0)(_ < txIndexBefore, _ + 1) { txNum =>
      history.typedExtraIndexById[NumericTxIndex](bytesToId(NumericTxIndex.indexToBytes(txNum))) shouldNot be(empty)
    }

    // check boxes after caught up
    cfor(0)(_ < boxIndexBefore, _ + 1) { boxNum =>
      history.typedExtraIndexById[NumericBoxIndex](bytesToId(NumericBoxIndex.indexToBytes(boxNum))) shouldNot be(empty)
    }

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
  val minerProp: Values.ErgoTree = ErgoTreePredef.rewardOutputScript(RewardDelay, minerPk)
  val votingEpochLength: Height = votingSettings.votingLength
  val protocolVersion: Byte = initSettings.chainSettings.protocolVersion
  val minimalSuffix = 2
  val txCostLimit: Height = initSettings.nodeSettings.maxTransactionCost
  val txSizeLimit: Height = initSettings.nodeSettings.maxTransactionSize

  var startTime: Long = 0

  def generate(length: Int, dir: File)(history: ErgoHistory): Unit = {
    val stateDir = new File(s"${dir.getAbsolutePath}/state")
    stateDir.mkdirs()
    val (state, _) = ErgoState.generateGenesisUtxoState(stateDir, initSettings)
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
      val (txs, lastOut) = genTransactions(last.map(_.height).getOrElse(GenesisHeight),
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
