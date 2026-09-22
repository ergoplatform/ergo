package org.ergoplatform.nodeView.viewholder

import akka.testkit.TestProbe
import org.ergoplatform.mining.{CandidateGenerator, InputBlockFields}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.ProcessOrderingBlock
import org.ergoplatform.network.message.inputblocks.OrderingBlockAnnouncement
import org.ergoplatform.nodeView.ErgoNodeViewHolder.DownloadRequest
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{BoxHolder, ErgoState, StateType}
import org.ergoplatform.settings.Constants
import org.ergoplatform.subblocks.InputBlockAnnouncement
import org.ergoplatform.utils.{
  ErgoCorePropertyTest, HistoryTestHelpers, NodeViewTestConfig, NodeViewTestOps, RandomWrapper
}
import org.ergoplatform.utils.fixtures.NodeViewFixture
import org.ergoplatform.utils.generators.ChainGenerator.{applyChain, genHeaderChain}
import org.ergoplatform.utils.generators.ValidBlocksGenerators.{
  createUtxoState, validFullBlock, validTransactionsFromBoxHolder
}
import org.ergoplatform.{ErgoBoxCandidate, Input, OrderingBlockFound}

import scala.concurrent.Await
import scala.concurrent.duration.{DurationInt, FiniteDuration}

class OrderingBlockReconstructionSpec extends ErgoCorePropertyTest with NodeViewTestOps {
  import org.ergoplatform.utils.ErgoCoreTestConstants.{
    defaultMinerSecret, defaultMinerSecretNumber, emptyProverResult, parameters, powScheme
  }

  private val timeout: FiniteDuration = 5.seconds

  Seq(
    (false, false, "reconstruct the parent's input chain before ordering transactions without downloading"),
    (true, false, "download the full body when the parent input chain body is missing"),
    (false, true, "a parent tree that is non-empty but LONGER than the committed chain falls back")
  ).foreach { case (missingBody, longerChain, description) =>
    property(description) {
      val fixture = new NodeViewFixture(
        NodeViewTestConfig(StateType.Utxo, verifyTransactions = true, popowBootstrap = false).toSettings,
        parameters)
      import fixture._
      val initial = createUtxoState(fixture.settings)
      var state = initial._1
      var minerHistory = HistoryTestHelpers.generateHistory(
        verifyTransactions = true, stateType = StateType.Utxo,
        PoPoWBootstrap = false, blocksToKeep = 100)
      try {
        def build(parent: Option[ErgoFullBlock], boxes: BoxHolder,
                  seed: Int, time: Long): (ErgoFullBlock, BoxHolder) = {
          val (txs, nextBoxes) = validTransactionsFromBoxHolder(boxes, new RandomWrapper(Some(seed)))
          val block = validFullBlock(parent, state, txs, Some(time))
          state = state.applyModifier(block, None)(_ => ()).get
          applyBlock(block).isSuccess shouldBe true
          block -> nextBoxes
        }

        val start = System.currentTimeMillis() - 10000
        val (root, rootBoxes) = build(None, initial._2, 1, start)
        val (parent, parentBoxes) = build(Some(root), rootBoxes, 2, start + 1)
        minerHistory = applyChain(minerHistory, Seq(root, parent))
        val parentOutputIds = parent.transactions.flatMap(_.outputs).map(_.id.toSeq).toSet
        val input = parentBoxes.boxes.values.find { box =>
          box.ergoTree == Constants.TrueTree && parentOutputIds.contains(box.id.toSeq)
        }.get
        val inputTx = ErgoTransaction(
          IndexedSeq(Input(input.id, emptyProverResult)), IndexedSeq.empty,
          IndexedSeq(new ErgoBoxCandidate(input.value, Constants.TrueTree,
            parent.height, input.additionalTokens)))
        inputTx.statelessValidity().get

        // Use the same real input-tree fixture as CandidateRetryReorgSpec.
        val inputHeader = genHeaderChain(1, minerHistory, diffBitsOpt = None, useRealTs = false).last
        val inputBlock = InputBlockAnnouncement(1, inputHeader, InputBlockFields.empty, None)
        minerHistory.applyInputBlock(inputBlock) shouldBe None
        minerHistory.applyInputBlockTransactions(inputBlock.id, Seq(inputTx), state)._1 should
          contain(inputBlock.id)
        val candidate = CandidateGenerator.generateCandidate(
          minerHistory, state, ErgoMemPool.empty(fixture.settings),
          defaultMinerSecret.publicImage, Seq.empty, None, fixture.settings).get.get._1.candidateBlock
        val inputTxs = minerHistory.getCollectedInputBlocksTransactions(parent.id).get
        inputTxs.map(_.id) shouldBe Seq(inputTx.id)
        candidate.orderingBlockTransactions should not be empty
        candidate.transactions.map(_.id) shouldBe
          (inputTxs ++ candidate.orderingBlockTransactions).map(_.id)
        candidate.transactions.size should be > 1
        val block = powScheme.proveBlock(
          candidate.parentOpt, candidate.version, candidate.nBits, candidate.stateRoot,
          candidate.adProofBytes, candidate.transactions, candidate.timestamp, candidate.extension,
          candidate.votes, defaultMinerSecretNumber, Long.MinValue, Long.MaxValue, parameters) match {
          case OrderingBlockFound(fullBlock) => fullBlock
          case other => fail(s"Expected a mined ordering block, got $other")
        }
        // Ensure reversing these nonempty groups really changes the committed root.
        BlockTransactions.transactionsRoot(
          candidate.orderingBlockTransactions ++ inputTxs, block.header.version).toSeq should not be
          block.header.transactionsRoot.toSeq

        val followerHistory = getHistory
        followerHistory.applyInputBlock(inputBlock) shouldBe None
        if (!missingBody) {
          followerHistory.applyInputBlockTransactions(inputBlock.id, Seq(inputTx), getCurrentState)
            ._1 should contain(inputBlock.id)
          followerHistory.getCollectedInputBlocksTransactions(parent.id).get.map(_.id) shouldBe
            inputTxs.map(_.id)
        } else {
          followerHistory.getCollectedInputBlocksTransactions(parent.id).get shouldBe empty
        }
        if (longerChain) {
          // Extend only the follower after the miner has committed the shorter chain.
          val extraInput = inputTx.outputs.head
          val extraTx = ErgoTransaction(
            IndexedSeq(Input(extraInput.id, emptyProverResult)), IndexedSeq.empty,
            IndexedSeq(new ErgoBoxCandidate(extraInput.value, Constants.TrueTree,
              parent.height, extraInput.additionalTokens)))
          extraTx.statelessValidity().get
          val fields = InputBlockFields.empty
          val extraFields = new InputBlockFields(
            Some(inputHeader.serializedId), fields.transactionsDigest,
            fields.prevTransactionsDigest, fields.inputBlockFieldsProof)
          val extraHeader = inputHeader.copy(timestamp = inputHeader.timestamp + 1)
          val extraBlock = InputBlockAnnouncement(1, extraHeader, extraFields, None)
          extraHeader.parentId shouldBe parent.id
          extraBlock.prevInputBlockId shouldBe Some(inputBlock.id)
          followerHistory.applyInputBlock(extraBlock) shouldBe None
          followerHistory.applyInputBlockTransactions(extraBlock.id, Seq(extraTx), getCurrentState)
            ._1 should contain(extraBlock.id)
          val followerTxs = followerHistory.getCollectedInputBlocksTransactions(parent.id).get
          followerTxs.map(_.id) shouldBe (inputTxs :+ extraTx).map(_.id)
          followerTxs.size shouldBe inputTxs.size + 1
          BlockTransactions.transactionsRoot(
            followerTxs ++ candidate.orderingBlockTransactions, block.header.version).toSeq should not be
            block.header.transactionsRoot.toSeq
        }
        followerHistory.getCollectedInputBlocksTransactions(block.id) shouldBe None
        // Header synchronization independently schedules missing sections. Preload the
        // header before observing announcement reconstruction, without supplying a body.
        applyHeader(block.header).isSuccess shouldBe true
        getHistory.typedModifierById[BlockTransactions](block.header.transactionsId) shouldBe None
        val downloads = TestProbe()(actorSystem)
        downloads.ignoreMsg {
          case DownloadRequest(sections) =>
            !sections.get(BlockTransactions.modifierTypeId).exists(_.nonEmpty)
          case _ => false
        }
        actorSystem.eventStream.subscribe(downloads.ref, classOf[DownloadRequest])
        val announcement = OrderingBlockAnnouncement(
          1, block.header, candidate.orderingBlockTransactions, Seq.empty, block.extension.fields)
        val processing = TestProbe()(actorSystem)
        processing.send(nodeViewHolderRef, ProcessOrderingBlock(announcement))
        // A same-sender query is a barrier after announcement processing.
        processing.send(nodeViewHolderRef, GetDataFromCurrentView[ErgoState[_], Boolean](_ => true))
        processing.expectMsg(timeout, true)

        if (missingBody || longerChain) {
          downloads.expectMsgType[DownloadRequest](timeout).modifiersToFetch shouldBe
            Map(BlockTransactions.modifierTypeId -> Seq(block.header.transactionsId))
          getHistory.typedModifierById[BlockTransactions](block.header.transactionsId) shouldBe None
        } else {
          downloads.expectNoMessage(500.millis)
          val reconstructed = getHistory
            .typedModifierById[BlockTransactions](block.header.transactionsId).get
          reconstructed.txs.map(_.id) shouldBe block.transactions.map(_.id)
          reconstructed.digest.toSeq shouldBe block.header.transactionsRoot.toSeq
          getBestFullBlockOpt.map(_.id) shouldBe Some(block.id)
        }
      } finally {
        minerHistory.closeStorage()
        state.closeStorage()
        Await.result(actorSystem.terminate(), 15.seconds)
      }
    }
  }
}
