package org.ergoplatform.nodeView.history.llm_generated

import com.google.common.io.Files.createTempDir
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.ergoplatform.mining.InputBlockFields
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{BoxHolder, StateType, UtxoState}
import org.ergoplatform.nodeView.wallet.IdUtils.EncodedBoxId
import org.ergoplatform.nodeView.wallet.persistence.{InputBlockDiff, OffChainRegistry}
import org.ergoplatform.settings.Algos
import org.ergoplatform.subblocks.InputBlockAnnouncement
import org.ergoplatform.utils.ErgoCoreTestConstants.parameters
import org.ergoplatform.utils.ErgoNodeTestConstants.settings
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.HistoryTestHelpers.generateHistory
import org.ergoplatform.utils.generators.ChainGenerator.{applyChain, genChain}
import org.ergoplatform.wallet.boxes.TrackedBox
import scorex.crypto.authds.merkle.BatchMerkleProof
import scorex.crypto.hash.Digest32
import scorex.util.{bytesToId, idToBytes}
import sigma.Colls
import sigma.ast.ErgoTree
import sigma.data.TrivialProp.TrueProp
import sigma.interpreter.ProverResult

class MatrixDisjointForkRollbackSpec extends ErgoCorePropertyTest {

  private val genesisBoxes = Seq(
    testBox("matrix-disjoint-fork-box-1", 0),
    testBox("matrix-disjoint-fork-box-2", 1)
  )

  private def testBox(label: String, index: Short): ErgoBox =
    new ErgoBox(
      value = 1000000000L,
      ergoTree = ErgoTree.fromProposition(TrueProp),
      creationHeight = 0,
      additionalTokens = Colls.emptyColl,
      additionalRegisters = Map.empty,
      transactionId = bytesToId(Algos.hash(label)),
      index = index
    )

  private def parentOnly(parentId: Array[Byte]): InputBlockFields =
    new InputBlockFields(
      Some(parentId),
      Digest32 @@ Array.fill(32)(0.toByte),
      Digest32 @@ Array.fill(32)(0.toByte),
      BatchMerkleProof(Seq.empty, Seq.empty)(Algos.hash)
    )

  private def spend(box: ErgoBox, creationHeight: Int): ErgoTransaction =
    new ErgoTransaction(
      IndexedSeq(Input(box.id, ProverResult.empty)),
      IndexedSeq.empty,
      IndexedSeq(
        new ErgoBoxCandidate(
          box.value,
          box.ergoTree,
          creationHeight,
          box.additionalTokens,
          Map.empty
        )
      )
    )

  private def tracked(box: ErgoBox): TrackedBox =
    TrackedBox(box.transactionId, box.index, None, None, None, box, Set.empty)

  private def initializedHistory(us: UtxoState): ErgoHistory = {
    val history = generateHistory(
      verifyTransactions = true,
      StateType.Utxo,
      PoPoWBootstrap = false,
      blocksToKeep = -1,
      epochLength = 10000,
      useLastEpochs = 3,
      initialDiffOpt = None,
      None
    )
    applyChain(history, genChain(height = 2, history = history, stateOpt = Some(us)))
    history
  }

  private def newRoot(history: ErgoHistory): InputBlockAnnouncement = {
    val header = genChain(height = 2, history = history).tail.head.header
    InputBlockAnnouncement(1, header, InputBlockFields.empty, None)
  }

  private def child(
    history: ErgoHistory,
    parent: InputBlockAnnouncement
  ): InputBlockAnnouncement = {
    val header = genChain(height = 2, history = history).tail.head.header
    InputBlockAnnouncement(1, header, parentOnly(idToBytes(parent.id)), None)
  }

  property("a disjoint fork switch reports its processed abandoned root") {
    val us = UtxoState.fromBoxHolder(
      BoxHolder(genesisBoxes),
      None,
      createTempDir,
      settings,
      parameters
    )
    val history = initializedHistory(us)
    val abandonedRoot = newRoot(history)
    val winningRoot = newRoot(history)
    val winningTip = child(history, winningRoot)

    history.applyInputBlock(abandonedRoot) shouldBe None
    history.applyInputBlockTransactions(abandonedRoot.id, Seq.empty, us) shouldBe
      (Seq(abandonedRoot.id) -> Seq.empty)

    history.applyInputBlock(winningRoot) shouldBe None
    history.applyInputBlock(winningTip) shouldBe None
    history.applyInputBlockTransactions(winningRoot.id, Seq.empty, us) shouldBe
      (Seq.empty -> Seq.empty)

    val (forward, rollback) =
      history.applyInputBlockTransactions(winningTip.id, Seq.empty, us)

    forward shouldBe Seq(winningRoot.id, winningTip.id)
    rollback shouldBe Seq(abandonedRoot.id)
    history.bestInputBlocksChain() shouldBe Seq(winningTip.id, winningRoot.id)
  }

  property("rollback output keeps wallet and mempool consumers consistent") {
    val us = UtxoState.fromBoxHolder(
      BoxHolder(genesisBoxes),
      None,
      createTempDir,
      settings,
      parameters
    )
    val history = initializedHistory(us)
    val height = us.stateContext.currentHeight

    val abandonedRoot = newRoot(history)
    val abandonedTip = child(history, abandonedRoot)
    val abandonedRootTx = spend(genesisBoxes.head, height)
    val abandonedTipTx = spend(abandonedRootTx.outputs.head, height)

    val winningRoot = newRoot(history)
    val winningMiddle = child(history, winningRoot)
    val winningTip = child(history, winningMiddle)
    val winningRootTx = spend(genesisBoxes(1), height)
    val winningMiddleTx = spend(winningRootTx.outputs.head, height)

    history.applyInputBlock(abandonedRoot) shouldBe None
    history.applyInputBlock(abandonedTip) shouldBe None
    val (firstForward, firstRollback) =
      history.applyInputBlockTransactions(abandonedRoot.id, Seq(abandonedRootTx), us)
    firstForward shouldBe Seq(abandonedRoot.id)
    firstRollback shouldBe empty
    val (secondForward, secondRollback) =
      history.applyInputBlockTransactions(abandonedTip.id, Seq(abandonedTipTx), us)
    secondForward shouldBe Seq(abandonedTip.id)
    secondRollback shouldBe empty

    history.applyInputBlock(winningRoot) shouldBe None
    history.applyInputBlock(winningMiddle) shouldBe None
    history.applyInputBlock(winningTip) shouldBe None
    history.applyInputBlockTransactions(winningRoot.id, Seq(winningRootTx), us) shouldBe
      (Seq.empty -> Seq.empty)
    history.applyInputBlockTransactions(winningMiddle.id, Seq(winningMiddleTx), us) shouldBe
      (Seq.empty -> Seq.empty)

    val (forward, rollback) =
      history.applyInputBlockTransactions(winningTip.id, Seq.empty, us)

    rollback shouldBe Seq(abandonedRoot.id, abandonedTip.id)
    forward shouldBe Seq(winningRoot.id, winningMiddle.id, winningTip.id)

    val allTransactions = Seq(
      abandonedRootTx,
      abandonedTipTx,
      winningRootTx,
      winningMiddleTx
    )
    var pool = ErgoMemPool.empty(settings).put(
      allTransactions.map(tx => UnconfirmedTransaction(tx, None))
    )
    (firstForward ++ secondForward).foreach { id =>
      pool = pool.removeWithDoubleSpends(history.getInputBlockTransactions(id).get)
    }
    rollback.foreach { id =>
      val transactions = history.getInputBlockTransactions(id).get
      pool = pool.put(transactions.map(tx => UnconfirmedTransaction(tx, None)))
    }
    forward.foreach { id =>
      pool = pool.removeWithDoubleSpends(history.getInputBlockTransactions(id).get)
    }

    pool.contains(abandonedRootTx.id) shouldBe true
    pool.contains(abandonedTipTx.id) shouldBe true
    pool.contains(winningRootTx.id) shouldBe false
    pool.contains(winningMiddleTx.id) shouldBe false

    val originalBox = tracked(genesisBoxes.head)
    val parentBox = tracked(abandonedRootTx.outputs.head)
    val childBox = tracked(abandonedTipTx.outputs.head)
    val originalRegistry = OffChainRegistry.empty.updateOnTransaction(
      Seq(originalBox),
      Seq.empty,
      Seq.empty
    )
    val (afterParent, parentRemovedOffChain, parentRemovedOnChain) =
      originalRegistry.updateOnTransactionWithDiff(
        Seq(parentBox),
        Seq(EncodedBoxId @@@ originalBox.boxId),
        Seq.empty
      )
    val parentDiff = InputBlockDiff(Seq(parentBox), parentRemovedOffChain, parentRemovedOnChain)
    val withParentDiff = afterParent.copy(
      inputBlockDiffs = afterParent.inputBlockDiffs + (abandonedRoot.id -> parentDiff)
    )
    val (afterChild, childRemovedOffChain, childRemovedOnChain) =
      withParentDiff.updateOnTransactionWithDiff(
        Seq(childBox),
        Seq(EncodedBoxId @@@ parentBox.boxId),
        Seq.empty
      )
    val childDiff = InputBlockDiff(Seq(childBox), childRemovedOffChain, childRemovedOnChain)
    val walletRegistry = afterChild.copy(
      inputBlockDiffs = afterChild.inputBlockDiffs + (abandonedTip.id -> childDiff)
    )

    val rolledBackRegistry = rollback.reverse.foldLeft(walletRegistry) { (registry, id) =>
      registry.rollbackInputBlock(id)
    }
    rolledBackRegistry.offChainBoxes.map(_.boxId) should contain theSameElementsAs Seq(originalBox.boxId)
    rolledBackRegistry.inputBlockDiffs.keySet.intersect(rollback.toSet) shouldBe empty
  }
}
