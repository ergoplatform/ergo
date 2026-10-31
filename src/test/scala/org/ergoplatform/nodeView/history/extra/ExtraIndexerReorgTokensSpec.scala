package org.ergoplatform.nodeView.history.extra

import akka.actor.{ActorRef, Props}
import akka.testkit.TestProbe
import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.ergoplatform.core.idToVersion
import org.ergoplatform.http.api.SortDirection.{ASC, DESC}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.extra.IndexedTokenSerializer.uniqueId
import org.ergoplatform.nodeView.history.extra.SegmentSerializer.boxSegmentId
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.{Constants, ErgoSettings}
import org.ergoplatform.utils.fixtures.NodeViewFixture
import org.ergoplatform.utils.generators.ValidBlocksGenerators.{createUtxoState, validFullBlock, validTransactionsFromBoxHolder}
import org.ergoplatform.utils.{ErgoCorePropertyTest, NodeViewTestConfig, NodeViewTestOps, RandomWrapper, untilTimeout}
import scorex.util.{ModifierId, bytesToId}
import sigma.Colls
import sigma.ast.{ErgoTree, GE, Height, IntConstant}
import sigma.interpreter.ProverResult
import sigmastate.eval.Extensions._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import scala.util.Try

/**
  * Verifies that token rows, their box segments, emission metadata, and unspent-box queries
  * (`retrieveUtxos`, backing `/blockchain/box/unspent/byTokenId`) remain exact across chain
  * reorganizations on a real node view holder with full UTXO state verification.
  */
class ExtraIndexerReorgTokensSpec extends ErgoCorePropertyTest with NodeViewTestOps {

  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters

  private val settleTimeout = 15.seconds

  private val indexedSettings: ErgoSettings = {
    val base = NodeViewTestConfig(StateType.Utxo, verifyTransactions = true, popowBootstrap = false).toSettings
    base.copy(nodeSettings = base.nodeSettings.copy(extraIndex = true))
  }

  private type BlockTxs = WrappedUtxoState => Seq[ErgoTransaction]

  /** A script satisfied with an empty proof that ordinary generated transactions do not touch. */
  private val watchedTree: ErgoTree = ErgoTree.fromProposition(GE(Height, IntConstant(0)).toSigmaProp)

  private def moveBox(box: ErgoBox, tree: ErgoTree = watchedTree): ErgoTransaction =
    ErgoTransaction(
      IndexedSeq(Input(box.id, ProverResult.empty)),
      IndexedSeq.empty,
      IndexedSeq(new ErgoBoxCandidate(box.value, tree, box.creationHeight, box.additionalTokens))
    )

  /** Mints a new token with id == firstBox.id across two outputs (testing multi-output emission). */
  private def mintMultiOutputToken(firstBox: ErgoBox,
                                   secondBox: ErgoBox,
                                   firstAmount: Long,
                                   secondAmount: Long): (ModifierId, ErgoTransaction) = {
    val tokenIdBytes = firstBox.id
    val tokenId = bytesToId(tokenIdBytes)
    val creationHeight = math.max(firstBox.creationHeight, secondBox.creationHeight)
    val out0Tokens = Colls.fromArray(Array[(TokenId, Long)](tokenIdBytes.toTokenId -> firstAmount))
    val out1Tokens = Colls.fromArray(Array[(TokenId, Long)](tokenIdBytes.toTokenId -> secondAmount))
    val tx = ErgoTransaction(
      IndexedSeq(Input(firstBox.id, ProverResult.empty), Input(secondBox.id, ProverResult.empty)),
      IndexedSeq.empty,
      IndexedSeq(
        new ErgoBoxCandidate(firstBox.value, watchedTree, creationHeight, out0Tokens),
        new ErgoBoxCandidate(secondBox.value, watchedTree, creationHeight, out1Tokens)
      )
    )
    (tokenId, tx)
  }

  /** Mints a new token in a single output box. */
  private def mintSingleOutputToken(inputBox: ErgoBox, amount: Long): (ModifierId, ErgoTransaction) = {
    val tokenIdBytes = inputBox.id
    val tokenId = bytesToId(tokenIdBytes)
    val outTokens = Colls.fromArray(Array[(TokenId, Long)](tokenIdBytes.toTokenId -> amount))
    val tx = ErgoTransaction(
      IndexedSeq(Input(inputBox.id, ProverResult.empty)),
      IndexedSeq.empty,
      IndexedSeq(new ErgoBoxCandidate(inputBox.value, watchedTree, inputBox.creationHeight, outTokens))
    )
    (tokenId, tx)
  }

  private final case class ExpectedTokenState(
    tokenId: ModifierId,
    mintBoxId: ModifierId,
    emissionAmount: Long,
    allBoxNumbersWithSign: Seq[Long],
    unspentBoxIdsAsc: Seq[ModifierId]
  )

  /** Replays the applied `chain` from height 1 to compute ground-truth token expectations. */
  private def expectedTokens(chain: Seq[ErgoFullBlock]): Map[ModifierId, ExpectedTokenState] = {
    val txsWithHeight = chain.flatMap(b => b.blockTransactions.txs.map(_ -> b.header.height))
    val boxById = mutable.HashMap.empty[ModifierId, (ErgoBox, Long)]
    val spentBoxIds = mutable.HashSet.empty[ModifierId]
    val mintBoxByToken = mutable.HashMap.empty[ModifierId, ModifierId]
    val emissionByToken = mutable.HashMap.empty[ModifierId, Long]
    val boxNumsByToken = mutable.HashMap.empty[ModifierId, ArrayBuffer[Long]]
    val createdBoxesByToken = mutable.HashMap.empty[ModifierId, ArrayBuffer[ModifierId]]

    var globalBoxIdx = 0L
    txsWithHeight.foreach { case (tx, height) =>
      val inTokenIds = mutable.HashSet.empty[ModifierId]
      if (height > 1) {
        tx.inputs.foreach { in =>
          val inId = bytesToId(in.boxId)
          spentBoxIds += inId
          boxById.get(inId).foreach { case (spentBox, _) =>
            spentBox.additionalTokens.toArray.foreach { case (tid, _) =>
              inTokenIds += bytesToId(tid.toArray)
            }
          }
        }
      }
      val firstInputIdOpt = tx.inputs.headOption.map(in => bytesToId(in.boxId))
      val mintedHere = mutable.HashSet.empty[ModifierId]
      tx.outputs.foreach { out =>
        val outId = bytesToId(out.id)
        boxById.put(outId, (out, globalBoxIdx))
        out.additionalTokens.toArray.foreach { case (tid, amt) =>
          val tokenId = bytesToId(tid.toArray)
          if (firstInputIdOpt.contains(tokenId) && !inTokenIds.contains(tokenId)) {
            if (mintedHere.contains(tokenId)) {
              emissionByToken.put(tokenId, emissionByToken(tokenId) + amt)
            } else if (!mintBoxByToken.contains(tokenId)) {
              mintBoxByToken.put(tokenId, outId)
              emissionByToken.put(tokenId, amt)
              mintedHere += tokenId
            }
          }
          boxNumsByToken.getOrElseUpdate(tokenId, ArrayBuffer.empty[Long]) += globalBoxIdx
          createdBoxesByToken.getOrElseUpdate(tokenId, ArrayBuffer.empty[ModifierId]) += outId
        }
        globalBoxIdx += 1L
      }
    }

    mintBoxByToken.keys.map { tokenId =>
      val signedNums = createdBoxesByToken(tokenId).zip(boxNumsByToken(tokenId)).map { case (boxId, num) =>
        if (spentBoxIds.contains(boxId)) -num else num
      }.toSeq
      val unspentAsc = createdBoxesByToken(tokenId).filterNot(spentBoxIds.contains).toSeq
      tokenId -> ExpectedTokenState(
        tokenId = tokenId,
        mintBoxId = mintBoxByToken(tokenId),
        emissionAmount = emissionByToken(tokenId),
        allBoxNumbersWithSign = signedNums,
        unspentBoxIdsAsc = unspentAsc
      )
    }.toMap
  }

  private def listedTokenBoxes(history: ErgoHistory, row: IndexedToken): Seq[Long] =
    (0 until row.boxSegmentCount).flatMap { n =>
      val segId = row.factory(boxSegmentId(row.id, n)).id
      history.typedExtraIndexById[IndexedToken](segId).toSeq.flatMap(_.boxes)
    } ++ row.boxes

  private def tokenDifferences(history: ErgoHistory, chain: Seq[ErgoFullBlock])
                              (implicit segmentThreshold: Int): Seq[String] = {
    val reader = history.getReader
    val mempool = ErgoMemPool.empty(indexedSettings)
    expectedTokens(chain).toSeq.flatMap { case (tokenId, exp) =>
      history.typedExtraIndexById[IndexedToken](uniqueId(tokenId)) match {
        case None =>
          Some(s"missing IndexedToken for $tokenId")
        case Some(row) =>
          val listed = listedTokenBoxes(history, row)
          val utxosAsc = row.retrieveUtxos(reader, mempool, 0, Int.MaxValue, ASC, unconfirmed = false, Set.empty).map(_.id)
          val utxosDesc = row.retrieveUtxos(reader, mempool, 0, Int.MaxValue, DESC, unconfirmed = false, Set.empty).map(_.id)
          val problems = ArrayBuffer.empty[String]
          if (row.boxId != Some(exp.mintBoxId))
            problems += s"token $tokenId boxId=${row.boxId}, expected Some(${exp.mintBoxId})"
          if (row.amount != Some(exp.emissionAmount))
            problems += s"token $tokenId amount=${row.amount}, expected Some(${exp.emissionAmount})"
          if (row.boxCount != exp.allBoxNumbersWithSign.size.toLong)
            problems += s"token $tokenId boxCount=${row.boxCount}, expected ${exp.allBoxNumbersWithSign.size}"
          if (listed != exp.allBoxNumbersWithSign)
            problems += s"token $tokenId listedBoxes=$listed, expected ${exp.allBoxNumbersWithSign}"
          if (utxosAsc != exp.unspentBoxIdsAsc)
            problems += s"token $tokenId utxosAsc=$utxosAsc, expected ${exp.unspentBoxIdsAsc}"
          if (utxosDesc != exp.unspentBoxIdsAsc.reverse)
            problems += s"token $tokenId utxosDesc=$utxosDesc, expected ${exp.unspentBoxIdsAsc.reverse}"
          problems.toSeq
      }
    }
  }

  private class IndexedNode(fixture: NodeViewFixture, segmentThresholdOpt: Option[Int]) {
    import fixture._

    implicit val segmentThreshold: Int = segmentThresholdOpt.getOrElse(512)

    val chain: ArrayBuffer[ErgoFullBlock] = ArrayBuffer.empty[ErgoFullBlock]
    private val copyPath: ArrayBuffer[ErgoFullBlock] = ArrayBuffer.empty[ErgoFullBlock]

    private var stateCopy: WrappedUtxoState = {
      val (genesisState, boxHolder) = createUtxoState(settings)
      val genesis = validFullBlock(None, genesisState, boxHolder)
      val afterGenesis = WrappedUtxoState(genesisState, boxHolder, settings).applyModifier(genesis)(_ => ()).get
      applyBlock(genesis) shouldBe 'success
      chain += genesis
      copyPath += genesis
      afterGenesis
    }

    private var branchesBuilt: Int = 0
    (1 to 3).foreach(_ => extend())

    private val indexer: ActorRef = actorSystem.actorOf(segmentThresholdOpt match {
      case Some(threshold) =>
        Props(new ExtraIndexer(settings.cacheSettings, settings.chainSettings.addressEncoder, Some(nodeViewHolderRef), true) {
          override implicit val segmentThreshold: Int = threshold
        })
      case None =>
        Props(new ExtraIndexer(settings.cacheSettings, settings.chainSettings.addressEncoder, Some(nodeViewHolderRef), true))
    })

    awaitAndVerify()

    def close(): Unit = {
      val watcher = TestProbe()
      watcher.watch(indexer)
      actorSystem.stop(indexer)
      Try(watcher.expectTerminated(indexer, 5.seconds))
      stateCopy.closeStorage()
    }

    def history: ErgoHistory = getHistory
    def checkpoint: IndexerState = IndexerState.fromHistory(getHistory)
    def txCount: Long = chain.map(_.blockTransactions.txs.size).sum.toLong

    private def stateAfter(path: Seq[ErgoFullBlock]): WrappedUtxoState = {
      val common = copyPath.zip(path).takeWhile { case (applied, wanted) => applied.id == wanted.id }.size
      if (common < copyPath.size) {
        stateCopy = stateCopy.rollbackTo(idToVersion(copyPath(common - 1).id)).get
        copyPath.remove(common, copyPath.size - common)
      }
      path.drop(common).foreach { block =>
        stateCopy = stateCopy.applyModifier(block)(_ => ()).get
        copyPath += block
      }
      stateCopy
    }

    def ordinaryTxs(state: WrappedUtxoState, atLeast: Int = 1): Seq[ErgoTransaction] = Iterator.continually {
      val avoided = mutable.Set.empty[ModifierId]
      avoided ++= state.versionedBoxHolder.boxes.values.filter(_.ergoTree == watchedTree).map(box => bytesToId(box.id))
      validTransactionsFromBoxHolder(state.versionedBoxHolder, new RandomWrapper)._1.filter { tx =>
        val avoid = (tx.inputs.map(_.boxId) ++ tx.dataInputs.map(_.boxId)).exists(id => avoided(bytesToId(id)))
        if (avoid) avoided ++= tx.outputs.map(out => bytesToId(out.id))
        !avoid
      }
    }.take(100).find(_.size >= atLeast).get

    def ordinary: BlockTxs = ordinaryTxs(_)

    def branch(parent: ErgoFullBlock)(blocks: BlockTxs*): Seq[ErgoFullBlock] = {
      val kept = chain.take(chain.indexWhere(_.id == parent.id) + 1).toList
      branchesBuilt += 1
      blocks.foldLeft(Seq.empty[ErgoFullBlock]) { (built, txs) =>
        val state = stateAfter(kept ++ built)
        val time = if (built.isEmpty) Some(parent.header.timestamp + branchesBuilt) else None
        built :+ validFullBlock(Some(built.lastOption.getOrElse(parent)), state, txs(state), time)
      }
    }

    def deliver(blocks: Seq[ErgoFullBlock]): Unit = {
      blocks.foreach(block => applyBlock(block) shouldBe 'success)
      untilTimeout(settleTimeout, 20.millis) {
        getHistory.bestFullBlockIdOpt shouldBe Some(blocks.last.id)
        getCurrentState.version shouldBe idToVersion(blocks.last.id)
      }
      val kept = chain.indexWhere(_.id == blocks.head.header.parentId) + 1
      chain.remove(kept, chain.size - kept)
      chain ++= blocks
    }

    def extend(): Unit = deliver(branch(chain.last)(ordinary))

    def fund(count: Int): Seq[ErgoBox] = {
      val funded = ArrayBuffer.empty[ErgoBox]
      (1 to 25).iterator.takeWhile(_ => funded.size < count).foreach { _ =>
        deliver(branch(chain.last) { state =>
          val txs = ordinaryTxs(state)
          val spentInBlock = txs.flatMap(_.inputs).map(input => bytesToId(input.boxId)).toSet
          val spare = txs.flatMap(_.outputs).filter(out =>
            out.ergoTree == Constants.TrueTree && out.value >= 10000000L && !spentInBlock(bytesToId(out.id))
          )
          val payments = spare.take(count - funded.size).map(moveBox(_, watchedTree))
          funded ++= payments.map(_.outputs.head)
          txs ++ payments
        })
      }
      funded.size shouldBe count
      funded.toList
    }

    def awaitAndVerify(): Unit = {
      untilTimeout(settleTimeout, 20.millis) {
        val cp = checkpoint
        cp.indexedHeaderId shouldBe Some(chain.last.id)
        cp.rollbackTo shouldBe 0
        cp.globalTxIndex shouldBe txCount
      }
      tokenDifferences(getHistory, chain) shouldBe empty
    }
  }

  private def withNode(segmentThreshold: Option[Int])(test: IndexedNode => Unit): Unit =
    new NodeViewFixture(indexedSettings, parameters).apply { fixture =>
      val node = new IndexedNode(fixture, segmentThreshold)
      try test(node) finally node.close()
    }

  property("segmented multi-output and fork-minted tokens preserve unspent boxes and clean up abandoned segments across reorgs") {
    withNode(segmentThreshold = Some(2)) { node =>
      val funded = node.fund(5)
      val (sharedTokenId, mintSharedTx) = mintMultiOutputToken(funded(0), funded(1), 60L, 40L)

      // Block on common prefix: mint sharedTokenId in 2 outputs, then move output 0 twice -> 4 boxes total (boxSegmentCount >= 1).
      val move1 = moveBox(mintSharedTx.outputs(0))
      val move2 = moveBox(move1.outputs(0))
      node.deliver(node.branch(node.chain.last)(state => node.ordinaryTxs(state) ++ Seq(mintSharedTx, move1, move2)))
      node.awaitAndVerify()

      val sharedBeforeFork = node.history.typedExtraIndexById[IndexedToken](uniqueId(sharedTokenId)).get
      sharedBeforeFork.amount shouldBe Some(100L)
      sharedBeforeFork.boxSegmentCount should be >= 1

      val forkPoint = node.chain.last

      // Abandoned branch (1 block): spends both remaining unspent boxes of sharedTokenId across multiple hops
      // AND mints an abandoned token transferred across 4 boxes (creating box segments that must be deleted on rollback).
      val (abandonedTokenId, mintAbandonedTx) = mintSingleOutputToken(funded(2), 500L)
      val abMove1 = moveBox(mintAbandonedTx.outputs(0))
      val abMove2 = moveBox(abMove1.outputs(0))
      val abMove3 = moveBox(abMove2.outputs(0))
      val sharedForkASpend0 = moveBox(mintSharedTx.outputs(1))
      val sharedForkASpend1 = moveBox(move2.outputs(0))

      val abandonedBranch = node.branch(forkPoint) { state =>
        node.ordinaryTxs(state) ++ Seq(mintAbandonedTx, abMove1, abMove2, abMove3, sharedForkASpend0, sharedForkASpend1)
      }
      node.deliver(abandonedBranch)
      node.awaitAndVerify()

      val abandonedRowOnForkA = node.history.typedExtraIndexById[IndexedToken](uniqueId(abandonedTokenId)).get
      val abandonedSegmentsCount = abandonedRowOnForkA.boxSegmentCount
      abandonedSegmentsCount should be >= 1

      // Winning longer branch (2 blocks from forkPoint): spends only move2.outputs(0), leaving mintSharedTx.outputs(1)
      // unspent from before the fork, and mints winnerTokenId.
      val (winnerTokenId, mintWinnerTx) = mintSingleOutputToken(funded(3), 900L)
      val sharedForkBSpend = moveBox(move2.outputs(0))
      val winningBranch = node.branch(forkPoint)(
        state => node.ordinaryTxs(state) ++ Seq(sharedForkBSpend, mintWinnerTx),
        node.ordinary
      )

      node.deliver(winningBranch)
      node.awaitAndVerify()

      // Abandoned token AND all of its box segment rows must be completely removed from extraStore.
      val abandonedParentKey = uniqueId(abandonedTokenId)
      node.history.typedExtraIndexById[IndexedToken](abandonedParentKey) shouldBe None
      (0 until abandonedSegmentsCount).foreach { segIdx =>
        val segRecordId = IndexedToken(boxSegmentId(abandonedParentKey, segIdx)).id
        node.history.typedExtraIndexById[IndexedToken](segRecordId) shouldBe None
      }

      // Shared token retains exact multi-output emission metadata and both its pre-fork and Fork-B unspent boxes.
      val sharedAfterReorg = node.history.typedExtraIndexById[IndexedToken](uniqueId(sharedTokenId)).get
      sharedAfterReorg.amount shouldBe Some(100L)
      val utxosAfterReorg = sharedAfterReorg
        .retrieveUtxos(node.history.getReader, ErgoMemPool.empty(indexedSettings), 0, 100, ASC, unconfirmed = false, Set.empty)
        .map(_.id)
      utxosAfterReorg shouldBe Seq(bytesToId(mintSharedTx.outputs(1).id), bytesToId(sharedForkBSpend.outputs(0).id))

      // Winner token is present with its unspent box.
      val winnerRow = node.history.typedExtraIndexById[IndexedToken](uniqueId(winnerTokenId)).get
      winnerRow.amount shouldBe Some(900L)
    }
  }

}
